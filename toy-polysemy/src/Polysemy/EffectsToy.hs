{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Polysemy.EffectsToy
  ( start
  ) where

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.ByteString.Lazy as LBS
import           Data.Function
import           Data.String (fromString)
import qualified Polysemy.EffectsToy.Interpreter.ByteStream.Strict as BSStrict
-- importing effects for now, replace with interpreters later
import           Polysemy
import           Polysemy.Trace
import           Polysemy.EffectsToy.Effect.WaiHandler
import           Polysemy.EffectsToy.Effect.Db.TestDb

start :: IO ()
start = do
    runBaseStack initApp
    Warp.run 8087 (runWaiApplication runBaseStack small)
  where
    -- initApp = initDb
    initApp = return ()

runBaseStack :: _ a -> IO a
runBaseStack = runM

runWaiApplication :: (forall x. Sem r x -> IO x)
                     -> _ ()
                     -> Wai.Application
runWaiApplication runToIO waiApp request respond =
    waiApp
    -- & runWaiHandler request
    & BSStrict.runByteStream
    & runToIO
    & (fmap toResponse)
    & (>>= respond)
  where
    -- toResponse (body, (headers, status)) = Wai.responseLBS status headers body
    toResponse (body, _) = Wai.responseLBS HTTP.ok200 [] body

small :: ( Member BSStrict.ByteStream r
         ) => Sem r ()
small = do
  BSStrict.tellChunk "hello, small world"

helloWorld :: ( Member Trace r
              , Member WaiHandler r
              , Member TestDb r
              ) => Sem r ()
helloWorld = do
  trace "Request received"
  req <- askRequest
  tellHeaders [(HTTP.hContentType, "text/plain")]
  tellChunk "Hello, world!\n"
  (reqId, str) <- req & Wai.rawQueryString
                      & LBS.fromStrict
                      & storeAndLookup
  tellChunk $ "You requested " <> str <> "\n"
  tellChunk $ "Your request was number " <> (fromString $ show reqId)
  putStatus HTTP.ok200
