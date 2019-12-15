{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module EffectsToy
  ( start
  ) where

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Handler.Warp as Warp

import EffectsToy.Carrier.WaiApplication

start :: IO ()
start = Warp.run 8080 (runWaiApplication helloWorld)

helloWorld :: ( Has WaiApplication sig m )
           => m ()
helloWorld = do
  req <- askRequest
  tellHeaders [(HTTP.hContentType, "text/plain")]
  sendChunk "Hello, world!\n"
  sendChunk ("You requested " <> (Wai.rawQueryString req))
  putStatus HTTP.ok200
