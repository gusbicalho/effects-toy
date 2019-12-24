module Eff.EffectsToy.Handler.SQLiteSimple
  ( runSQLiteSimple, withConnection, withConnection'
  -- * Re-exports
  , module Eff.EffectsToy.Effect.SQLiteSimple
  ) where

import           Control.Effect
import           Control.Exception.Lifted
import           Control.Monad.Trans.Control
import           Control.Monad.IO.Class
import           Control.Effect.Reader
import qualified Database.SQLite.Simple as SQLite
import           Eff.EffectsToy.Effect.SQLiteSimple


data SQLiteSimpleMIO
type SQLiteSimpleT = HandlerT SQLiteSimpleMIO '[ReaderT SQLite.Connection]
type instance Handles SQLiteSimpleT eff = eff == SQLiteSimple

instance MonadIO m => SQLiteSimple (SQLiteSimpleT m) where
  query   q params = HandlerT $ do
    conn <- ask
    liftIO $ SQLite.query conn q params
  execute q params = HandlerT $ do
    conn <- ask
    liftIO $ SQLite.execute conn q params
  lastInsertRowId  = HandlerT $ do
    conn <- ask
    liftIO $ SQLite.lastInsertRowId conn
  {-# INLINE query #-}
  {-# INLINE execute #-}
  {-# INLINE lastInsertRowId #-}

runSQLiteSimple :: SQLite.Connection -> EffT SQLiteSimpleT m a -> m a
runSQLiteSimple conn = runReader conn . runHandlerT . runEffT

withConnection' :: ( MonadBaseControl IO m
                   , MonadIO m
                   ) => String -> (SQLite.Connection -> m c) -> m c
withConnection' dbpath =
  bracket
    (liftIO $ SQLite.open dbpath)
    (liftIO . SQLite.close)

withConnection :: ( MonadBaseControl IO m
                  , MonadIO m
                  ) => String -> EffT SQLiteSimpleT m c -> m c
withConnection dbpath action = withConnection' dbpath $ \conn -> runSQLiteSimple conn action
