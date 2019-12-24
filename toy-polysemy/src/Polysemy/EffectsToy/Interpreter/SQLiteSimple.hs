{-# LANGUAGE BlockArguments #-}
module Polysemy.EffectsToy.Interpreter.SQLiteSimple
  ( runSQLiteSimple, withConnection, withConnection'
  -- * Re-exports
  , module Polysemy.EffectsToy.Effect.SQLiteSimple
  ) where

import           Control.Monad.IO.Class
import qualified Database.SQLite.Simple as SQLite
import           Polysemy
import           Polysemy.Reader
import           Polysemy.Resource
import           Polysemy.EffectsToy.Effect.SQLiteSimple

runSQLiteSimple :: ( Member (Embed IO) r
                   ) => SQLite.Connection -> Sem (SQLiteSimple : r) a -> Sem r a
runSQLiteSimple conn = runReader conn . reinterpret \case
  Query q params -> do
    c <- ask
    liftIO $ SQLite.query c q params
  Execute q params -> do
    c <- ask
    liftIO $ SQLite.execute c q params
  LastInsertRowId -> do
    c <- ask
    liftIO $ SQLite.lastInsertRowId c

withConnection' :: ( Member (Embed IO) r
                   , Member Resource r
                   ) => String -> (SQLite.Connection -> Sem r b) -> Sem r b
withConnection' dbpath =
  bracket
    (liftIO $ SQLite.open dbpath)
    (liftIO . SQLite.close)

withConnection :: ( Member (Embed IO) r
                  , Member Resource r
                  ) => String -> Sem (SQLiteSimple : r) b -> Sem r b
withConnection dbpath action = withConnection' dbpath $ \conn -> runSQLiteSimple conn action
