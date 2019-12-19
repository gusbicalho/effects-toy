module Eff.EffectsToy.Handler.ByteStream.Strict
  ( runByteStreamStrict
  , module Eff.EffectsToy.Effect.ByteStream
  ) where

import Control.Effect
import Control.Effect.Writer
import Eff.EffectsToy.Effect.ByteStream
import qualified Data.ByteString.Lazy as LBS

data ByteStreamStrict
type ByteStreamStrictT = HandlerT ByteStreamStrict '[WriterT LBS.ByteString]
type instance Handles ByteStreamStrictT eff = eff == ByteStream

instance Monad m => ByteStream (ByteStreamStrictT m) where
  tellChunk chunk = HandlerT $ tell chunk

runByteStreamStrict :: Functor m => EffT ByteStreamStrictT m a -> m (LBS.ByteString, a)
runByteStreamStrict = runWriter @LBS.ByteString . runHandlerT . runEffT

-- runIOEffect :: EffT IOEffectT m a -> m a
-- runIOEffect = runHandlerT . runEffT
