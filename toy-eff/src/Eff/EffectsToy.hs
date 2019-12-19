{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Eff.EffectsToy
  ( start, start2
  ) where

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.ByteString.Lazy as LBS
import Eff.EffectsToy.Handler.IOEffect
import qualified Eff.EffectsToy.Handler.ByteStream.Strict as BSStrict
import qualified Eff.EffectsToy.Handler.ByteStream.Streaming as BSStreaming
import Eff.EffectsToy.Handler.WaiHandler

start :: IO ()
start = Warp.run 8087 (runWaiApplication helloWorld)

runWaiApplication :: _ () -> Wai.Application
runWaiApplication waiApp request respond = do
  (body, (headers, status)) <- runIOEffect
                               . BSStrict.runByteStream
                               . runWaiHandler request
                               $ waiApp
  respond $ Wai.responseLBS status headers body

start2 :: IO ()
start2 = Warp.run 8087 (runWaiApplication2 helloWorld)

runWaiApplication2 :: _ () -> Wai.Application
runWaiApplication2 waiApp request respond = do
  (body, (headers, status)) <- runIOEffect
                               . BSStreaming.runByteStream
                               . runWaiHandler request
                               $ waiApp
  respond $ Wai.responseLBS status headers body

helloWorld :: (WaiHandler m, IOEffect m) => m ()
helloWorld = do
  sendIO $ putStrLn "Request received"
  req <- askRequest
  tellHeaders [(HTTP.hContentType, "text/plain")]
  tellChunk "Hello, world!\n"
  tellChunk $ "You requested " <> LBS.fromStrict (Wai.rawQueryString req)
  putStatus HTTP.ok200
