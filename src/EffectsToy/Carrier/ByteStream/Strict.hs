{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module EffectsToy.Carrier.ByteStream.Strict
  ( ByteStreamC, runByteStream
  , module EffectsToy.Effect.ByteStream
  ) where

import           Control.Algebra
import           Control.Carrier.Writer.Strict
import qualified Data.ByteString.Lazy as LBS
import           EffectsToy.Effect.ByteStream

newtype ByteStreamC m a = ByteStreamC {
  runByteStreamC :: WriterC LBS.ByteString m a
  } deriving newtype (Functor, Applicative, Monad)

instance ( Algebra sig m
         , Effect sig
         ) => Algebra (ByteStream :+: sig) (ByteStreamC m) where
  alg (L (TellChunk chunk k))     = k <* ByteStreamC (tell chunk)
  alg (R other)                   = ByteStreamC (alg (R (handleCoercible other)))
  {-# INLINE alg #-}

runByteStream :: ByteStreamC m a -> m (LBS.ByteString, a)
runByteStream = runWriter @LBS.ByteString . runByteStreamC
