{-# LANGUAGE UndecidableInstances #-}
module EffectsToy.Carrier.ByteStream
  ( ByteStreamC, runByteStream, Of (..), LazyBS.ByteString
  , module EffectsToy.Effect.ByteStream
  ) where

import           Control.Algebra
import qualified Data.ByteString.Streaming as Streaming
import qualified Data.ByteString.Lazy as LazyBS
import           Data.Functor.Of ( Of(..))

import           EffectsToy.Effect.ByteStream

newtype ByteStreamC m a = ByteStreamC (Streaming.ByteString m a)
  deriving newtype (Functor, Applicative, Monad)

(<<) :: Monad m => m a -> m b -> m a
(<<) = flip (>>)

instance ( Algebra sig m
         , Effect sig
         ) => Algebra (ByteStream :+: sig) (ByteStreamC m) where
  alg (L (SendChunk chunk k)) = k << ByteStreamC (Streaming.fromStrict chunk)
  alg (R other) = send other
  {-# INLINE alg #-}

runByteStream :: Monad m => ByteStreamC m a -> m (LazyBS.ByteString `Of` a)
runByteStream (ByteStreamC m) = Streaming.toLazy m
