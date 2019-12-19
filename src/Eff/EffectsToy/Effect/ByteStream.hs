{-# LANGUAGE UndecidableInstances #-}
module Eff.EffectsToy.Effect.ByteStream
  ( ByteStream (..)
  ) where

import Control.Effect
import qualified Data.ByteString.Lazy as LBS

class Monad m => ByteStream m where
  tellChunk :: LBS.ByteString -> m ()

instance (Monad (t m), Send ByteStream t m) => ByteStream (EffT t m) where
  tellChunk chunk = send @ByteStream (tellChunk chunk)
