{-# LANGUAGE BlockArguments #-}
module Polysemy.EffectsToy.Interpreter.Db.TestDb.SQLite
  ( runTestDb
  -- * Re-exports
  , module Polysemy.EffectsToy.Effect.Db.TestDb
  ) where

import           Polysemy
import           Polysemy.Resource
import           Polysemy.EffectsToy.Effect.Db.TestDb
import           Polysemy.EffectsToy.Effect.SQLiteSimple

runTestDb :: ( Member SQLiteSimple r
             , Member Resource r
             ) => Sem (TestDb : r) a -> Sem r a
runTestDb = interpret \case
  InitDb -> withTransaction do
    execute_ $ "CREATE TABLE IF NOT EXISTS test "
            <> "( id integer not null primary key"
            <> ", query_params text"
            <> ")"
  StoreAndLookup queryParams -> withTransaction do
    execute "INSERT INTO test (query_params) values (?)" [queryParams]
    reqId <- lastInsertRowId
    rows <- query "SELECT query_params FROM test WHERE id = ?" [reqId]
    case rows of
      []         -> return (reqId, "")
      (Only s:_) -> return (reqId, s)
