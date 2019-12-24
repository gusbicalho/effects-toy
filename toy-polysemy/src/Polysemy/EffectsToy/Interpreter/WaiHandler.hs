{-# LANGUAGE BlockArguments #-}
module Polysemy.EffectsToy.Interpreter.WaiHandler
  ( runWaiHandler
  -- * Re-exports
  , module Polysemy.EffectsToy.Effect.WaiHandler
  ) where

import           Polysemy
import           Polysemy.Reader
import           Polysemy.State
import           Polysemy.Writer
import           Polysemy.EffectsToy.Effect.WaiHandler
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Polysemy.EffectsToy.Effect.ByteStream as ByteStream

runWaiHandler' :: ( Member ByteStream.ByteStream r
                  ) => Wai.Request -> Sem (WaiHandler : r) a -> Sem r (HTTP.ResponseHeaders, (HTTP.Status, a))
runWaiHandler' request = runWriter @HTTP.ResponseHeaders
                       . runState HTTP.status500
                       . runReader request
                       . reinterpret3 \case
                          AskRequest          -> ask
                          PutStatus status    -> put status
                          TellHeaders headers -> tell headers
                          TellChunk chunk     -> ByteStream.tellChunk chunk

runWaiHandler :: ( Member ByteStream.ByteStream r
                 ) => Wai.Request -> Sem (WaiHandler : r) () -> Sem r (HTTP.ResponseHeaders, HTTP.Status)
runWaiHandler request waiApp = do
    (headers, (status, ())) <- runWaiHandler' request waiApp
    return (headers, status)
