{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module EffectsToy.Carrier.WaiHandler
  ( WaiHandlerC, runWaiHandlerC
  , runWaiHandler
  , module EffectsToy.Effect.WaiHandler
  ) where

import           Control.Algebra
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP

import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Carrier.Writer.Strict
import qualified Data.ByteString.Lazy as LBS
import           EffectsToy.Effect.WaiHandler

newtype WaiHandlerC m a = WaiHandlerC {
  runWaiHandlerC :: StateC
                          HTTP.Status
                          (WriterC
                            HTTP.ResponseHeaders
                            (ReaderC
                              Wai.Request
                              (WriterC
                                LBS.ByteString
                                m)))
                          a
  } deriving newtype (Functor, Applicative, Monad)

(<<) :: Monad m => m a -> m b -> m a
(<<) = flip (>>)

instance ( Algebra sig m
         , Effect sig
         ) => Algebra (WaiHandler :+: sig) (WaiHandlerC m) where
  alg (L (AskRequest k))          = k =<< WaiHandlerC (ask)
  alg (L (TellHeaders headers k)) = k << WaiHandlerC (tell headers)
  alg (L (PutStatus status k))    = k << WaiHandlerC (put status)
  alg (L (TellChunk chunk k))     = k << WaiHandlerC (tell (LBS.fromStrict chunk))
  alg (R other)                   = send other
  {-# INLINE alg #-}

runWaiHandler :: (Monad m)
              => Wai.Request
              -> WaiHandlerC m ()
              -> m Wai.Response
runWaiHandler request waiApp = do
  result <- runWriter @LBS.ByteString
          . runReader @Wai.Request request
          . runWriter @HTTP.ResponseHeaders
          . runState @HTTP.Status HTTP.status500
          . runWaiHandlerC
          $ waiApp
  let (body, (headers, (status , ()))) = result
  return $ Wai.responseLBS status headers body
