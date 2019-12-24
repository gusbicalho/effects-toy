module Eff.EffectsToy.Handler.Db.TestDb.SQLite
  ( runTestDb
  , module Eff.EffectsToy.Effect.Db.TestDb
  ) where

import Control.Effect
import Control.Monad.Trans.Control
import Eff.EffectsToy.Effect.SQLiteSimple
import Eff.EffectsToy.Effect.Db.TestDb

data TestDbSQLite
type TestDbT = HandlerT TestDbSQLite '[]
type instance Handles TestDbT eff = eff == TestDb

instance ( Monad m
         , MonadBaseControl IO m
         , SQLiteSimple m
         ) => TestDb (TestDbT m) where
  initDb = HandlerT . withTransaction $ do
    execute_ $ "CREATE TABLE IF NOT EXISTS test "
            <> "( id integer not null primary key"
            <> ", query_params text"
            <> ")"
  storeAndLookup queryParams = HandlerT . withTransaction $ do
    execute "INSERT INTO test (query_params) values (?)" [queryParams]
    reqId <- lastInsertRowId
    rows <- query "SELECT query_params FROM test WHERE id = ?" [reqId]
    case rows of
      []         -> return (reqId, "")
      (Only s:_) -> return (reqId, s)
  {-# INLINE initDb #-}
  {-# INLINE storeAndLookup #-}

runTestDb :: EffT TestDbT m a -> m a
runTestDb = runHandlerT . runEffT
