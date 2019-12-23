{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module FusedEffects.EffectsToy.Carrier.SQLiteSimple
  ( SQLiteSimpleC, runSQLiteSimple, withConnection, withConnection'
  , module FusedEffects.EffectsToy.Effect.SQLiteSimple
  ) where

import           Control.Algebra
import           Control.Carrier.Reader
import qualified Control.Effect.Exception as Exc
import           Control.Effect.Lift
import           FusedEffects.EffectsToy.Effect.SQLiteSimple
import qualified Database.SQLite.Simple as SQLite

newtype SQLiteSimpleC m a = SQLiteSimpleC {
  runSQLiteSimpleC :: ReaderC SQLite.Connection m a
  } deriving newtype (Functor, Applicative, Monad)

instance ( Algebra sig m
         , Effect sig
         , Has (Lift IO) sig m
         ) => Algebra (SQLiteSimple :+: sig) (SQLiteSimpleC m) where
  alg (L (Query   q params k)) = k =<< SQLiteSimpleC do
    conn <- ask
    sendM $ SQLite.query conn q params
  alg (L (Execute q params k)) = k <* SQLiteSimpleC do
    conn <- ask
    sendM $ SQLite.execute conn q params
  alg (L (LastInsertRowId k)) = k =<< SQLiteSimpleC do
    conn <- ask
    sendM $ SQLite.lastInsertRowId conn
  alg (R other)                = SQLiteSimpleC (alg (R (handleCoercible other)))
  {-# INLINE alg #-}

runSQLiteSimple :: SQLite.Connection -> SQLiteSimpleC m a -> m a
runSQLiteSimple conn = runReader conn . runSQLiteSimpleC

withConnection' :: (Has (Lift IO) sig m) => String -> (SQLite.Connection -> m c) -> m c
withConnection' dbpath =
  Exc.bracket
    (sendM $ SQLite.open dbpath)
    (sendM . SQLite.close)

withConnection :: (Has (Lift IO) sig m) => String -> SQLiteSimpleC m c -> m c
withConnection dbpath action = withConnection' dbpath $ \conn -> runSQLiteSimple conn action
