{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module EffectsToy
  ( start
  ) where

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Handler.Warp as Warp

import           Control.Carrier.Lift
import           EffectsToy.Carrier.WaiApplication
import qualified EffectsToy.Carrier.ByteStream as BS

start :: IO ()
start = Warp.run 8080 (runAppIO helloWorld)

runAppIO :: WaiApplicationC _ () -> Wai.Application
runAppIO = runWaiApplication (runM @IO . BS.runByteStream)

helloWorld :: ( Has WaiApplication sig m )
           => m ()
helloWorld = do
  req <- askRequest
  tellHeaders [(HTTP.hContentType, "text/plain")]
  sendChunk "Hello, world!\n"
  sendChunk ("You requested " <> (Wai.rawQueryString req))
  putStatus HTTP.ok200
