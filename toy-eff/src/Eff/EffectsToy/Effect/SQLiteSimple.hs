{-# LANGUAGE UndecidableInstances #-}
module Eff.EffectsToy.Effect.SQLiteSimple
  ( SQLiteSimple (..)
  , execute_, withTransaction
  -- * Re-exports
  , SQLite.Only (..)
  , SQLite.FromRow (..), FromField (..), SQLite.ToRow (..), ToField (..)
  ) where

import           Control.Effect
import           Control.Exception.Lifted
import           Control.Monad.Trans.Control
import           Data.Int
import qualified Database.SQLite.Simple as SQLite
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField

class Monad m => SQLiteSimple m where
  query           :: ( SQLite.ToRow params
                     , SQLite.FromRow result
                     ) => SQLite.Query -> params -> m [result]
  execute         :: ( SQLite.ToRow params
                     ) => SQLite.Query -> params -> m ()
  lastInsertRowId :: m Int64

instance (Monad (t m), Send SQLiteSimple t m) => SQLiteSimple (EffT t m) where
  query   q params = send @SQLiteSimple (query q params)
  execute q params = send @SQLiteSimple (execute q params)
  lastInsertRowId  = send @SQLiteSimple lastInsertRowId
  {-# INLINE query #-}
  {-# INLINE execute #-}
  {-# INLINE lastInsertRowId #-}

execute_ :: SQLiteSimple m => SQLite.Query -> m ()
execute_ q = execute q ()

withTransaction :: ( MonadBaseControl IO m
                   , SQLiteSimple m
                   ) => m b -> m b
withTransaction action =
  mask $ \restore -> do
    begin
    r <- restore action `onException` rollback
    commit
    return r
  where
    begin    = execute_ "BEGIN TRANSACTION"
    commit   = execute_ "COMMIT TRANSACTION"
    rollback = execute_ "ROLLBACK TRANSACTION"
