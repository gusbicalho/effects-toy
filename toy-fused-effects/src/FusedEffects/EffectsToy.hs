{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module FusedEffects.EffectsToy
  ( start
  ) where

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import           Control.Carrier.Lift
import           FusedEffects.EffectsToy.Carrier.Trace.StdOut
import           FusedEffects.EffectsToy.Carrier.WaiHandler
import qualified FusedEffects.EffectsToy.Carrier.ByteStream.Strict as BSStrict
import           FusedEffects.EffectsToy.Carrier.SQLiteSimple
import           FusedEffects.EffectsToy.Carrier.Db.TestDb.SQLite
import qualified Data.ByteString.Lazy as LBS
import           Data.Function
import           Data.String (fromString)

start :: IO ()
start = do
    runBaseStack initApp
    Warp.run 8087 (runWaiApplication runBaseStack helloWorld)
  where
    initApp = initDb

runBaseStack :: _ a -> IO a 
runBaseStack = runM @IO
             . runTrace
             . withConnection "/tmp/tempdb.db"
             . runTestDb
                               
runWaiApplication :: ( Monad n
                     ) => (forall x. n x -> IO x)
                       -> _ ()
                       -> Wai.Application
runWaiApplication runToIO waiApp request respond = do
    response <- (fmap toResponse)
                . runToIO
                . BSStrict.runByteStream
                . runWaiHandler request
                $ waiApp
    respond response
  where
    toResponse (body, (headers, status)) = Wai.responseLBS status headers body

helloWorld :: ( Has Trace sig m
              , Has WaiHandler sig m
              , Has TestDb sig m
              ) => m ()
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
