{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module EffectsToy.Carrier.ByteStream.Streaming
  ( ByteStreamC, runByteStream
  , module EffectsToy.Effect.ByteStream
  , Streaming.toLazy, Of (..)
  ) where

import           Control.Algebra
import           Control.Effect.Lift
import           EffectsToy.Effect.ByteStream
import qualified Data.ByteString.Streaming as Streaming
import           Data.Functor.Of ( Of (..) )

newtype ByteStreamC m a = ByteStreamC {
  runByteStreamC :: m a
  } deriving newtype (Functor, Applicative, Monad)

type ByteStreamIO = Streaming.ByteString IO

instance ( Algebra sig m
         , Effect sig
         , Has (Lift ByteStreamIO) sig m
         ) => Algebra (ByteStream :+: sig) (ByteStreamC m) where
  alg (L (TellChunk chunk k))     = k <* sendM @ByteStreamIO (Streaming.fromLazy chunk)
  alg (R other)                   = ByteStreamC (alg (handleCoercible other))
  {-# INLINE alg #-}

runByteStream :: ByteStreamC m a -> m a
runByteStream = runByteStreamC
