{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module EffectsToy.Carrier.WaiHandler
  ( WaiHandlerC
  , runWaiHandler
  , module EffectsToy.Effect.WaiHandler
  ) where

import           Control.Algebra
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP

import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Carrier.Writer.Strict
import           EffectsToy.Effect.WaiHandler
import qualified EffectsToy.Effect.ByteStream as ByteStream

newtype WaiHandlerC m a = WaiHandlerC {
  runWaiHandlerC :: StateC
                      HTTP.Status
                      (WriterC
                        HTTP.ResponseHeaders
                        (ReaderC
                          Wai.Request
                          m))
                      a
  } deriving newtype (Functor, Applicative, Monad)

instance ( Algebra sig m
         , Effect sig
         , Has ByteStream.ByteStream sig m
         ) => Algebra (WaiHandler :+: sig) (WaiHandlerC m) where
  alg (L (AskRequest k))          = k =<< WaiHandlerC (ask)
  alg (L (TellHeaders headers k)) = k  <* WaiHandlerC (tell headers)
  alg (L (PutStatus status k))    = k  <* WaiHandlerC (put status)
  alg (L (TellChunk chunk k))     = k  <* WaiHandlerC (ByteStream.tellChunk chunk)
  alg (R other)                   = WaiHandlerC (alg (R (R (R (handleCoercible other)))))
  {-# INLINE alg #-}

runWaiHandler :: Monad m => Wai.Request -> WaiHandlerC m () -> m (HTTP.ResponseHeaders, HTTP.Status)
runWaiHandler request waiApp = do
    result <- runReader @Wai.Request request
            . runWriter @HTTP.ResponseHeaders
            . runState @HTTP.Status HTTP.status500
            . runWaiHandlerC
            $ waiApp
    let (!headers, !(!status , ())) = result
    return (headers, status)
