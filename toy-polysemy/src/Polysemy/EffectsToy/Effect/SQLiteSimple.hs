{-# LANGUAGE DeriveAnyClass #-}
module Polysemy.EffectsToy.Effect.SQLiteSimple
  (
  ) where
--   ( SQLiteSimple (..)
--   , withTransaction, query, execute, execute_, lastInsertRowId
--   , SQLiteDB, sqliteDB
--   -- * Re-exports
--   , SQLite.Only (..)
--   , SQLite.FromRow (..), FromField (..), SQLite.ToRow (..), ToField (..)
--   , Algebra
--   , Has
--   , run
--   ) where

-- import           Control.Algebra
-- import           Data.Functor
-- import           Data.Int
-- import qualified Database.SQLite.Simple as SQLite
-- import           Database.SQLite.Simple.FromField
-- import           Database.SQLite.Simple.ToField
-- import qualified Control.Effect.Exception as Exc

-- newtype SQLiteDB = SQLiteDB FilePath

-- sqliteDB :: FilePath -> SQLiteDB
-- sqliteDB = SQLiteDB

-- data SQLiteSimple m k where
--   Query   :: ( SQLite.ToRow q, SQLite.FromRow r
--              ) => SQLite.Query -> q -> ([r] -> m k) -> SQLiteSimple m k
--   Execute :: ( SQLite.ToRow q
--              ) => SQLite.Query -> q -> m k -> SQLiteSimple m k
--   LastInsertRowId :: (Int64 -> m k) -> SQLiteSimple m k

-- instance Functor m => Functor (SQLiteSimple m) where
--   fmap f (Query   q params kont) = Query   q params (fmap f . kont)
--   fmap f (Execute q params kont) = Execute q params (fmap f kont)
--   fmap f (LastInsertRowId  kont) = LastInsertRowId  (fmap f . kont)

-- instance HFunctor SQLiteSimple where
--   hmap nt (Query   q params kont) = Query   q params (nt . kont)
--   hmap nt (Execute q params kont) = Execute q params (nt kont)
--   hmap nt (LastInsertRowId  kont) = LastInsertRowId  (nt . kont)

-- instance Effect SQLiteSimple where
--   thread ctx handle (Query   q params kont) = Query   q params (\a -> handle (ctx $> kont a))
--   thread ctx handle (Execute q params kont) = Execute q params (handle (ctx $> kont))
--   thread ctx handle (LastInsertRowId  kont) = LastInsertRowId  (\a -> handle (ctx $> kont a))

-- query :: ( Has SQLiteSimple sig m
--          , SQLite.ToRow q
--          , SQLite.FromRow r
--          ) => SQLite.Query -> q -> m [r]
-- query q params = send $ Query q params pure

-- execute :: ( Has SQLiteSimple sig m
--            , SQLite.ToRow q
--            ) => SQLite.Query -> q -> m ()
-- execute q params = send $ Execute q params (pure ())

-- execute_ :: ( Has SQLiteSimple sig m
--            ) => SQLite.Query -> m ()
-- execute_ q = execute q ()

-- lastInsertRowId :: ( Has SQLiteSimple sig m
--                    ) => m Int64
-- lastInsertRowId = send $ LastInsertRowId pure

-- withTransaction :: ( Has (Exc.Lift IO) sig m
--                    , Has SQLiteSimple sig m
--                    ) => m b -> m b
-- withTransaction action =
--   Exc.mask $ \restore -> do
--     begin
--     r <- restore action `Exc.onException` rollback
--     commit
--     return r
--   where
--     begin    = execute_ "BEGIN TRANSACTION"
--     commit   = execute_ "COMMIT TRANSACTION"
--     rollback = execute_ "ROLLBACK TRANSACTION"
