{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module FusedEffects.EffectsToy.Carrier.SQLiteSimple
  ( SQLiteSimpleC
  , module FusedEffects.EffectsToy.Effect.SQLiteSimple
  ) where

import           Control.Algebra
import           FusedEffects.EffectsToy.Effect.IOEffect
import           FusedEffects.EffectsToy.Effect.SQLiteSimple
import qualified Database.SQLite.Simple as SQLite

newtype SQLiteSimpleC m a = SQLiteSimpleC {
  runSQLiteSimpleC :: SQLite.Connection -> m a
  }

instance Functor m => Functor (SQLiteSimpleC m) where
  fmap f s = SQLiteSimpleC $ \conn -> fmap f (runSQLiteSimpleC s conn)

instance Applicative m => Applicative (SQLiteSimpleC m) where
  pure x = SQLiteSimpleC (const $ pure x)
  ma <*> mb = SQLiteSimpleC $ \conn ->
    (runSQLiteSimpleC ma conn) <*> (runSQLiteSimpleC mb conn)

instance Monad m => Monad (SQLiteSimpleC m) where
  ma >>= f = SQLiteSimpleC $ \conn -> do
    a <- runSQLiteSimpleC ma conn
    runSQLiteSimpleC (f a) conn

instance ( Algebra sig m
         , Effect sig
         , Has IOEffect sig m
         ) => Algebra (SQLiteSimple :+: sig) (SQLiteSimpleC m) where
  alg (L (Query   q params k)) = k =<< SQLiteSimpleC runQuery
    where runQuery conn = sendIO $ SQLite.query conn q params
  alg (L (Execute q params k)) = k <* SQLiteSimpleC runExec
    where runExec conn = sendIO $ SQLite.execute conn q params
  alg (R other)                = SQLiteSimpleC $ \conn -> alg (hmap (\m -> runSQLiteSimpleC m conn) other)
  {-# INLINE alg #-}

-- runSQLiteSimple :: _ a -> m a
-- runSQLiteSimple = runSQLiteSimpleC
