{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module FusedEffects.EffectsToy.Carrier.Db.TestDb.SQLite
  ( TestDbC, runTestDb
  , module FusedEffects.EffectsToy.Effect.Db.TestDb
  ) where

import           Control.Algebra
import           Control.Effect.Lift
import           FusedEffects.EffectsToy.Effect.SQLiteSimple
import           FusedEffects.EffectsToy.Effect.Db.TestDb

newtype TestDbC m a = TestDbC {
  runTestDbC :: m a
  } deriving newtype (Functor, Applicative, Monad)

instance ( Algebra sig m
         , Effect sig
         , Has (Lift IO) sig m
         , Has SQLiteSimple sig m
         ) => Algebra (TestDb :+: sig) (TestDbC m) where
  alg (L (InitDb k)) = k <* (TestDbC . withTransaction) do
    execute_ $ "CREATE TABLE IF NOT EXISTS test "
            <> "( id integer not null primary key"
            <> ", query_params text"
            <> ")"
         
  alg (L (StoreAndLookup queryString k)) = k =<< (TestDbC . withTransaction) do
    execute "INSERT INTO test (query_params) values (?)" [queryString]
    reqId <- lastInsertRowId
    rows <- query "SELECT query_params FROM test WHERE id = ?" [reqId]
    case rows of
      []         -> return (reqId, "")
      (Only s:_) -> return (reqId, s)

  alg (R other)                = TestDbC (alg (handleCoercible other))
  {-# INLINE alg #-}

runTestDb :: TestDbC m a -> m a
runTestDb = runTestDbC
