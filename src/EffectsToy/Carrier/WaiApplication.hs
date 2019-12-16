{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module EffectsToy.Carrier.WaiApplication
  ( WaiApplicationC, runWaiApplicationC
  , runWaiApplication
  , module EffectsToy.Effect.WaiApplication
  ) where

import Control.Algebra
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP

import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Carrier.Writer.Strict
import Control.Carrier.Lift
import qualified Data.ByteString.Streaming as Streaming
import           Data.Functor.Of ( Of(..))

import EffectsToy.Effect.WaiApplication

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
