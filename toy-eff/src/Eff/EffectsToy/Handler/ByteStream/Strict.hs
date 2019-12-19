module Eff.EffectsToy.Handler.ByteStream.Strict
  ( runByteStream
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

runByteStream :: Functor m => EffT ByteStreamStrictT m a -> m (LBS.ByteString, a)
runByteStream = runWriter @LBS.ByteString . runHandlerT . runEffT
