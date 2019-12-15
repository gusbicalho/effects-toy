{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module EffectsToy
    ( start
    ) where

import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Carrier.Writer.Strict
import Control.Carrier.Lift

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.ByteString.Streaming as Streaming
import           Data.Functor.Of ( Of(..))

type ByteStream = Streaming.ByteString IO

start :: IO ()
start = Warp.run 8080 (runWaiApplication helloWorld)

runWaiApplication :: _ () -> Wai.Application
runWaiApplication action request respond = do
  result <-
    Streaming.toLazy
    . runM @ByteStream
    . runReader @Wai.Request request
    . runWriter @HTTP.ResponseHeaders
    . runState @HTTP.Status HTTP.status500
    $ action
  let (respBody :> (headers, (status, ()))) = result
  respond (Wai.responseLBS status headers respBody)

helloWorld :: ( Has (Reader Wai.Request) sig m
              , Has (State HTTP.Status) sig m
              , Has (Writer HTTP.ResponseHeaders) sig m
              , Has (Lift ByteStream) sig m
              )
           => m ()
helloWorld = do
  req <- ask @Wai.Request
  tell @HTTP.ResponseHeaders [(HTTP.hContentType, "text/plain")]
  sendM @ByteStream "Hello, world!\n"
  sendM @ByteStream ("You requested " <> Streaming.fromStrict (Wai.rawQueryString req))
  put @HTTP.Status HTTP.ok200
