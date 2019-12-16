{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module EffectsToy.Carrier.WaiApplication
  ( WaiApplicationC, runWaiApplicationC
  , runWaiApplication
  , module EffectsToy.Effect.WaiApplication
  ) where

import           Control.Algebra
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP

import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Carrier.Writer.Strict
import qualified EffectsToy.Carrier.ByteStream as BS
import           EffectsToy.Carrier.ByteStream ( Of(..) )

import           EffectsToy.Effect.WaiApplication

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
         , Has (BS.ByteStream) sig m
         ) => Algebra (WaiApplication :+: sig) (WaiApplicationC m) where
  alg (L (AskRequest k))          = k =<< WaiApplicationC (ask)
  alg (L (TellHeaders headers k)) = k << WaiApplicationC (tell headers)
  alg (L (PutStatus status k))    = k << WaiApplicationC (put status)
  alg (L (SendChunk chunk k))     = k << BS.sendChunk chunk
  alg (R other) = send other
  {-# INLINE alg #-}

handleRequest :: (Has BS.ByteStream sig m)
              => Wai.Request
              -> WaiApplicationC m ()
              -> m (HTTP.ResponseHeaders, HTTP.Status)
handleRequest request waiApp = do
  result <- runReader @Wai.Request request
          . runWriter @HTTP.ResponseHeaders
          . runState @HTTP.Status HTTP.status500
          . runWaiApplicationC
          $ waiApp
  let (headers, (status , ())) = result
  return (headers, status)

runWaiApplication :: (Has BS.ByteStream sig m, Monad n)
                  => (forall x. m x -> n (BS.ByteString `Of` x))
                  -> WaiApplicationC m ()
                  -> Wai.Request
                  -> (Wai.Response -> n b) -> n b
runWaiApplication runByteStream waiApp request respond = do
  result <-
    runByteStream
    . handleRequest request
    $ waiApp
  let (respBody :> (headers, status)) = result
  respond (Wai.responseLBS status headers respBody)
