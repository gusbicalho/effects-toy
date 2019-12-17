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

start :: IO ()
start = Warp.run 8087 (runWaiApplication helloWorld)

runWaiApplication :: WaiHandlerC _ () -> Wai.Application
runWaiApplication waiApp request respond = do
  putStrLn "1"
  result <-
    runM
    . runWaiHandler request
    $ waiApp
  putStrLn "2"
  respond result

helloWorld :: ( Has WaiHandler sig m )
           => m ()
helloWorld = do
  req <- askRequest
  tellHeaders [(HTTP.hContentType, "text/plain")]
  tellChunk $ "Hello, world!\n"
  tellChunk $ "You requested " <> (Wai.rawQueryString req)
  putStatus HTTP.ok200
