{-# LANGUAGE DeriveAnyClass #-}
module FusedEffects.EffectsToy.Effect.Db.TestDb
  ( TestDb (..)
  , initDb, storeAndLookup
  -- * Re-exports
  , Algebra
  , Has
  , run
  ) where

import           GHC.Generics (Generic1)
import           Control.Algebra
import qualified Data.ByteString.Lazy as LBS
import           Data.Int

data TestDb m k
  = InitDb (m k)
  | StoreAndLookup LBS.ByteString ((Int64, LBS.ByteString) -> m k)
  deriving (Generic1, Functor, HFunctor, Effect)

initDb :: (Has TestDb sig m) => m ()
initDb = send $ InitDb (pure ())

storeAndLookup :: (Has TestDb sig m) => LBS.ByteString -> m (Int64, LBS.ByteString)
storeAndLookup queryString = send $ StoreAndLookup queryString pure
