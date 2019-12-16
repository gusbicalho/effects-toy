{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module EffectsToy
  ( start
  ) where

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import           Control.Carrier.Lift
import           Data.String (fromString)
import           EffectsToy.Carrier.WaiHandler
import           EffectsToy.Carrier.ByteStream.Streaming ( runByteStream, Of (..) )

start :: IO ()
start = Warp.run 8087 (runWaiApplication helloWorld)

runWaiApplication :: WaiHandlerC _ () -> Wai.Application
runWaiApplication waiApp request respond = do
  putStrLn "1"
  result <-
    runByteStream
    . runWaiHandler request
    $ waiApp
  putStrLn "2"
  let (respBody :> (headers, status)) = result
  respond (Wai.responseLBS status headers respBody)

helloWorld :: ( Has WaiHandler sig m )
           => m ()
helloWorld = do
  req <- askRequest
  tellHeaders [(HTTP.hContentType, "text/plain")]
  num <- return (42 :: Int) -- ask @Int
  -- sendChunk $ "Hello, world!\n" <> fromString (show num) <> "\n"
  sendChunk $ "You requested " <> (Wai.rawQueryString req)
  putStatus HTTP.ok200
