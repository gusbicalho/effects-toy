{-# LANGUAGE BlockArguments #-}
module Polysemy.EffectsToy.Interpreter.ByteStream.Strict
  ( runByteStream
  -- * Re-exports
  , module Polysemy.EffectsToy.Effect.ByteStream
  ) where

import Polysemy
import Polysemy.Output
import Polysemy.EffectsToy.Effect.ByteStream
import qualified Data.ByteString.Lazy as LBS

runByteStream :: Sem (ByteStream : r) a -> Sem r (LBS.ByteString, a)
runByteStream = runOutputMonoid id . reinterpret \case
  (TellChunk chunk) -> output chunk
