{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module EffectsToy.Carrier.IOEffect
  ( IOEffectC, runIOEffect
  , module EffectsToy.Effect.IOEffect
  ) where

import           Control.Algebra
import           EffectsToy.Effect.IOEffect
import           Control.Monad.IO.Class

newtype IOEffectC m a = IOEffectC {
  runIOEffect :: m a
  } deriving newtype (Functor, Applicative, Monad)

instance ( Algebra sig m
         , Effect sig
         , MonadIO m
         ) => Algebra (IOEffect :+: sig) (IOEffectC m) where
  alg (L (RunIO ioAction k)) = k <* IOEffectC (liftIO ioAction)
  alg (R other)              = IOEffectC (alg (handleCoercible other))
  {-# INLINE alg #-}
