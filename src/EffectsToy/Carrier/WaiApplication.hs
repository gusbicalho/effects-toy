{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module EffectsToy.Carrier.WaiApplication
  ( WaiApplication (..)
  , askRequest, tellHeaders, putStatus, sendChunk
  -- Carrier
  , WaiApplicationC, runWaiApplicationC
  , runWaiApplication
  , Has
  ) where

import GHC.Generics (Generic1)

import Control.Algebra
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString as BS

import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Carrier.Writer.Strict
import Control.Carrier.Lift
import qualified Data.ByteString.Streaming as Streaming
import           Data.Functor.Of ( Of(..))



-- Effect interface

data WaiApplication m k
  = AskRequest (Wai.Request -> m k)
  | TellHeaders HTTP.ResponseHeaders (m k)
  | PutStatus HTTP.Status (m k)
  | SendChunk BS.ByteString (m k)
  deriving (Generic1, Functor, HFunctor, Effect)

askRequest :: (Has WaiApplication sig m) => m Wai.Request
askRequest = send $ AskRequest pure

tellHeaders :: (Has WaiApplication sig m) => HTTP.ResponseHeaders -> m ()
tellHeaders headers = send $ TellHeaders headers (pure ())

putStatus :: (Has WaiApplication sig m) => HTTP.Status -> m ()
putStatus status = send $ PutStatus status (pure ())

sendChunk :: (Has WaiApplication sig m) => BS.ByteString -> m ()
sendChunk chunk = send $ SendChunk chunk (pure ())

-- Carrier

newtype WaiApplicationC m a = WaiApplicationC {
  runWaiApplicationC :: StateC
                          HTTP.Status
                          (WriterC
                            HTTP.ResponseHeaders
                            (ReaderC
                              Wai.Request
                              m))
                          a
  } deriving newtype (Functor, Applicative, Monad)

(<<) :: Monad m => m a -> m b -> m a
(<<) = flip (>>)

instance ( Algebra sig m
         , Effect sig
         , Has (Lift (Streaming.ByteString IO)) sig m
         ) => Algebra (WaiApplication :+: sig) (WaiApplicationC m) where
  alg (L (AskRequest k))          = k =<< WaiApplicationC (ask)
  alg (L (TellHeaders headers k)) = k << WaiApplicationC (tell headers)
  alg (L (PutStatus status k))    = k << WaiApplicationC (put status)
  alg (L (SendChunk chunk k))     = k << sendM @(Streaming.ByteString IO) (Streaming.fromStrict chunk)
  alg (R other) = send other
  {-# INLINE alg #-}

runWaiApplication :: WaiApplicationC _ () -> Wai.Application
runWaiApplication waiApp request respond = do
  result <-
    Streaming.toLazy
    . runM
    . runReader @Wai.Request request
    . runWriter @HTTP.ResponseHeaders
    . runState @HTTP.Status HTTP.status500
    . runWaiApplicationC
    $ waiApp
  let (respBody :> (headers, (status, ()))) = result
  respond (Wai.responseLBS status headers respBody)
