{-# LANGUAGE TemplateHaskell #-}
module Polysemy.EffectsToy.Effect.Db.TestDb
  ( TestDb (..)
  , initDb, storeAndLookup
  ) where

import qualified Data.ByteString.Lazy as LBS
import           Data.Int
import           Polysemy

data TestDb m k where
  InitDb :: TestDb m ()
  StoreAndLookup :: LBS.ByteString -> TestDb m (Int64, LBS.ByteString)

makeSem ''TestDb
