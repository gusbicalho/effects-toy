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
import qualified EffectsToy.Carrier.ByteStream as BS
import           EffectsToy.Carrier.ByteStream ( Of (..) )

start :: IO ()
start = Warp.run 8080 (runWaiApplication helloWorld)

runWaiApplication :: WaiHandlerC _ () -> Wai.Application
runWaiApplication waiApp request respond = do
  result <-
    runM @IO
    . BS.runByteStream
    . runWaiHandler request
    $ waiApp
  let (respBody :> (headers, status)) = result
  respond (Wai.responseLBS status headers respBody)

helloWorld :: ( Has WaiHandler sig m )
           => m ()
helloWorld = do
  req <- askRequest
  tellHeaders [(HTTP.hContentType, "text/plain")]
  sendChunk "Hello, world!\n"
  sendChunk ("You requested " <> (Wai.rawQueryString req))
  putStatus HTTP.ok200
