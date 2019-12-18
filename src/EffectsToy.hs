{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module EffectsToy
  ( start, start2
  ) where

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import           Control.Carrier.Lift
import           EffectsToy.Carrier.WaiHandler
import qualified EffectsToy.Carrier.ByteStream.Strict as BSStrict
import qualified EffectsToy.Carrier.ByteStream.Streaming as BSStreaming
import           EffectsToy.Carrier.ByteStream.Streaming ( Of(..) )
import qualified Data.ByteString.Lazy as LBS

start :: IO ()
start = Warp.run 8087 (runWaiApplication helloWorld)

runWaiApplication :: WaiHandlerC _ () -> Wai.Application
runWaiApplication waiApp request respond = do
  (body, (headers, status)) <- runM
                               . BSStrict.runByteStream
                               . runWaiHandler request
                               $ waiApp
  respond $ Wai.responseLBS status headers body

start2 :: IO ()
start2 = Warp.run 8087 (runWaiApplication2 helloWorld)

runWaiApplication2 :: WaiHandlerC _ () -> Wai.Application
runWaiApplication2 waiApp request respond = do
  (body :> (headers, status)) <- BSStreaming.toLazy
                               . runM
                               . BSStreaming.runByteStream
                               . runWaiHandler request
                               $ waiApp
  respond $ Wai.responseLBS status headers body

helloWorld :: ( Has WaiHandler sig m
              ) => m ()
helloWorld = do
  req <- askRequest
  -- sendM (liftIO $ putStrLn "here")
  tellHeaders [(HTTP.hContentType, "text/plain")]
  tellChunk "Hello, world!\n"
  tellChunk $ "You requested " <> LBS.fromStrict (Wai.rawQueryString req)
  putStatus HTTP.ok200
