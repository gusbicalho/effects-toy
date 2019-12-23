{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module FusedEffects.EffectsToy
  ( start
  ) where

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import           Control.Carrier.Lift
import           FusedEffects.EffectsToy.Carrier.WaiHandler
import qualified FusedEffects.EffectsToy.Carrier.ByteStream.Strict as BSStrict
import           FusedEffects.EffectsToy.Carrier.SQLiteSimple
import qualified Data.ByteString.Lazy as LBS
import           Data.Function
import           Data.Int
import           Data.String (fromString)

start :: IO ()
start = do
    runBaseStack initApp
    Warp.run 8087 (runWaiApplication runBaseStack helloWorld)
  where
    runBaseStack :: SQLiteSimpleC (LiftC IO) a -> IO a 
    runBaseStack m = m & withConnection "/tmp/tempdb.db"
                       & runM @IO
    initApp = initDB

runWaiApplication :: ( Monad m
                     , Monad n
                     ) => (forall x. n x -> m x)
                       -> WaiHandlerC _ ()
                       -> Wai.Request
                       -> (Wai.Response -> m b)
                       -> m b
runWaiApplication runBaseStack waiApp request respond = do
  (body, (headers, status)) <- runBaseStack
                               . BSStrict.runByteStream
                               . runWaiHandler request
                               $ waiApp
  respond $ Wai.responseLBS status headers body

initDB :: ( Has SQLiteSimple sig m
          ) => m ()
initDB = execute_ $ "CREATE TABLE IF NOT EXISTS test "
                 <> "( id integer not null primary key"
                 <> ", query_params text"
                 <> ")"

helloWorld :: ( Has WaiHandler sig m
              , Has (Lift IO) sig m
              , Has SQLiteSimple sig m
              ) => m ()
helloWorld = do
  sendM $ putStrLn "Request received"
  req <- askRequest
  tellHeaders [(HTTP.hContentType, "text/plain")]
  tellChunk "Hello, world!\n"
  (reqId, str) <- req & Wai.rawQueryString 
                      & LBS.fromStrict
                      & storeAndLookup
  tellChunk $ "You requested " <> str <> "\n"
  tellChunk $ "Your request was number " <> (fromString $ show reqId)
  putStatus HTTP.ok200

storeAndLookup :: ( Has (Lift IO) sig m
                  , Has SQLiteSimple sig m
                  ) => LBS.ByteString -> m (Int64, LBS.ByteString)
storeAndLookup queryString = withTransaction $ do
  execute "INSERT INTO test (query_params) values (?)" [queryString]
  reqId <- lastInsertRowId
  rows <- query "SELECT query_params FROM test WHERE id = ?" [reqId]
  case rows of
    []         -> return (reqId, "")
    (Only s:_) -> return (reqId, s)
