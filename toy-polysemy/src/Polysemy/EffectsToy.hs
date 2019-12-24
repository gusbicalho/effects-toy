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
import           Polysemy.EffectsToy.Interpreter.WaiHandler
-- importing effects for now, replace with interpreters later
import           Polysemy
import           Polysemy.Trace
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
    & runWaiHandler request
    & BSStrict.runByteStream
    & runToIO
    & (fmap toResponse)
    & (>>= respond)
  where
    toResponse (body, (headers, status)) = Wai.responseLBS status headers body

small :: ( Member WaiHandler r
         ) => Sem r ()
small = do
  tellChunk "hello, small world"
  tellHeaders [(HTTP.hContentType, "text/plain")]
  putStatus HTTP.ok200

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
