{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Eff.EffectsToy
  ( start --, start2
  ) where

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.ByteString.Lazy as LBS
import Eff.EffectsToy.Handler.IOEffect
import Eff.EffectsToy.Handler.ByteStream.Strict

start :: IO ()
start = do
  (w, ()) <- runIOEffect . runByteStreamStrict $ helloWorld
  LBS.putStrLn w
  return ()

helloWorld :: (ByteStream m, IOEffect m) => m ()
helloWorld = do
  tellChunk "effects!"
  sendIO (putStrLn "hello world!")
  tellChunk "more effects!"
-- import           Control.Carrier.Lift
-- import           FusedEffects.EffectsToy.Carrier.IOEffect
-- import           FusedEffects.EffectsToy.Carrier.WaiHandler
-- import qualified FusedEffects.EffectsToy.Carrier.ByteStream.Strict as BSStrict
-- import qualified FusedEffects.EffectsToy.Carrier.ByteStream.Streaming as BSStreaming
-- import           FusedEffects.EffectsToy.Carrier.ByteStream.Streaming ( Of(..) )

-- start :: IO ()
-- start = Warp.run 8087 (runWaiApplication helloWorld)

-- runWaiApplication :: WaiHandlerC _ () -> Wai.Application
-- runWaiApplication waiApp request respond = do
--   (body, (headers, status)) <- runM
--                                . runIOEffect
--                                . BSStrict.runByteStream
--                                . runWaiHandler request
--                                $ waiApp
--   respond $ Wai.responseLBS status headers body

-- start2 :: IO ()
-- start2 = Warp.run 8087 (runWaiApplication2 helloWorld)

-- runWaiApplication2 :: WaiHandlerC _ () -> Wai.Application
-- runWaiApplication2 waiApp request respond = do
--   (body :> (headers, status)) <- BSStreaming.toLazy
--                                . runM
--                                . runIOEffect
--                                . BSStreaming.runByteStream
--                                . runWaiHandler request
--                                $ waiApp
--   respond $ Wai.responseLBS status headers body

-- helloWorld :: ( Has WaiHandler sig m
--               , Has IOEffect sig m
--               ) => m ()
-- helloWorld = do
--   sendIO $ putStrLn "Request received"
--   req <- askRequest
--   tellHeaders [(HTTP.hContentType, "text/plain")]
--   tellChunk "Hello, world!\n"
--   tellChunk $ "You requested " <> LBS.fromStrict (Wai.rawQueryString req)
--   putStatus HTTP.ok200
