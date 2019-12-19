module Eff.EffectsToy.Handler.WaiHandler
  ( runWaiHandlerByteStream
  , module Eff.EffectsToy.Effect.WaiHandler
  ) where

import Control.Effect
import Control.Effect.State
import Control.Effect.Reader
import Control.Effect.Writer
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Eff.EffectsToy.Effect.ByteStream as ByteStream
import Eff.EffectsToy.Effect.WaiHandler
import qualified Data.ByteString.Lazy as LBS

data WaiHandlerByteStream
type WaiHandlerByteStreamT = HandlerT WaiHandlerByteStream '[ ReaderT Wai.Request
                                                            , StateT HTTP.Status
                                                            , WriterT HTTP.ResponseHeaders
                                                            ]
type instance Handles WaiHandlerByteStreamT eff = eff == WaiHandler

instance (Monad m, ByteStream.ByteStream m) => WaiHandler (WaiHandlerByteStreamT m) where
  askRequest          = HandlerT $ ask
  tellHeaders headers = HandlerT $ tell headers
  putStatus status    = HandlerT $ put status
  tellChunk chunk     = HandlerT $ ByteStream.tellChunk chunk

runWaiHandlerByteStream :: Monad m => Wai.Request -> EffT WaiHandlerByteStreamT m () -> m (HTTP.ResponseHeaders, HTTP.Status)
runWaiHandlerByteStream request waiApp = do
  (headers, (status, ())) <- runWriter @HTTP.ResponseHeaders
                           . runState HTTP.status500
                           . runReader request
                           . runHandlerT
                           . runEffT
                           $ waiApp
  return (headers, status)
