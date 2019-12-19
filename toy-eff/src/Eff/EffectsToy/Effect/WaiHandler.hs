{-# LANGUAGE UndecidableInstances #-}
module Eff.EffectsToy.Effect.WaiHandler
  ( WaiHandler (..)
  ) where

import Control.Effect
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString.Lazy as LBS

class Monad m => WaiHandler m where
  askRequest :: m Wai.Request
  tellHeaders :: HTTP.ResponseHeaders -> m ()
  putStatus :: HTTP.Status -> m ()
  tellChunk :: LBS.ByteString -> m ()

instance (Monad (t m), Send WaiHandler t m) => WaiHandler (EffT t m) where
  askRequest = send @WaiHandler askRequest
  tellHeaders headers = send @WaiHandler (tellHeaders headers)
  putStatus status = send @WaiHandler (putStatus status)
  tellChunk chunk = send @WaiHandler (tellChunk chunk)
