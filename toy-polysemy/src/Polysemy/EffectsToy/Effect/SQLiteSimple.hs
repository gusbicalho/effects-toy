{-# LANGUAGE TemplateHaskell #-}
module Polysemy.EffectsToy.Effect.SQLiteSimple
  ( SQLiteSimple (..)
  , query, execute, execute_, lastInsertRowId, withTransaction
  -- * Re-exports
  , SQLite.Only (..)
  , SQLite.FromRow (..), FromField (..), SQLite.ToRow (..), ToField (..)
  ) where

import           Data.Int
import qualified Database.SQLite.Simple as SQLite
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Polysemy
import           Polysemy.Resource

data SQLiteSimple m k where
  Query   :: ( SQLite.ToRow q, SQLite.FromRow r
             ) => SQLite.Query -> q -> SQLiteSimple m [r]
  Execute :: ( SQLite.ToRow q
             ) => SQLite.Query -> q -> SQLiteSimple m ()
  LastInsertRowId :: SQLiteSimple m Int64

makeSem ''SQLiteSimple

execute_ :: Member SQLiteSimple r => SQLite.Query -> Sem r ()
execute_ q = execute q ()

withTransaction :: ( Member Resource r
                   , Member SQLiteSimple r
                   ) => Sem r b -> Sem r b
withTransaction action =
  bracket
    begin
    (const commit)
    (const $ action `onException` rollback)
  where
    begin    = execute_ "BEGIN TRANSACTION"
    commit   = execute_ "COMMIT TRANSACTION"
    rollback = execute_ "ROLLBACK TRANSACTION"
