{-# LANGUAGE UndecidableInstances #-}
module Eff.EffectsToy.Effect.Db.TestDb
  ( TestDb (..)
  ) where

import           Control.Effect
import qualified Data.ByteString.Lazy as LBS
import           Data.Int

class Monad m => TestDb m where
  initDb :: m ()
  storeAndLookup :: LBS.ByteString -> m (Int64, LBS.ByteString)

instance (Monad (t m), Send TestDb t m) => TestDb (EffT t m) where
  initDb                     = send @TestDb initDb
  storeAndLookup queryParams = send @TestDb $ storeAndLookup queryParams
  {-# INLINE initDb #-}
  {-# INLINE storeAndLookup #-}
