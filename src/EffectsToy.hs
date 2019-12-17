{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module EffectsToy
  ( start
  ) where

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import           Control.Carrier.Lift
import           EffectsToy.Carrier.WaiHandler
import           EffectsToy.Carrier.ByteStream.Strict (runByteStream)
import qualified Data.ByteString.Lazy as LBS

start :: IO ()
start = Warp.run 8087 (runWaiApplication helloWorld)

runWaiApplication :: WaiHandlerC _ () -> Wai.Application
runWaiApplication waiApp request respond = do
  (body, (headers, status)) <- runM
                               . runByteStream
                               . runWaiHandler request
                               $ waiApp
  respond $ Wai.responseLBS status headers body

helloWorld :: ( Has WaiHandler sig m
              , Has (Lift IO) sig m
              ) => m ()
helloWorld = do
  req <- askRequest
  sendM (putStrLn "here")
  tellHeaders [(HTTP.hContentType, "text/plain")]
  tellChunk "Hello, world!\n"
  tellChunk $ "You requested " <> LBS.fromStrict (Wai.rawQueryString req)
  putStatus HTTP.ok200
