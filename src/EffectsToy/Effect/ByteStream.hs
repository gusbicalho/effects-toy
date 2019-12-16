{-# LANGUAGE DeriveAnyClass #-}
module EffectsToy.Effect.ByteStream
  ( ByteStream (..)
  , sendChunk
  -- * Re-exports
  , Algebra
  , Has
  , run
  ) where

import           GHC.Generics (Generic1)
import           Control.Algebra
import qualified Data.ByteString as BS

data ByteStream m k
  = SendChunk BS.ByteString (m k)
  deriving (Generic1, Functor, HFunctor, Effect)

sendChunk :: (Has ByteStream sig m) => BS.ByteString -> m ()
sendChunk chunk = send $ SendChunk chunk (pure ())
