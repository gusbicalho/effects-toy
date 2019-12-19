{-# LANGUAGE DeriveAnyClass #-}
module FusedEffects.EffectsToy.Effect.IOEffect
  ( IOEffect (..)
  , sendIO
  -- * Re-exports
  , Algebra
  , Has
  , run
  ) where

import           Control.Algebra
import           Data.Functor

data IOEffect m k where
  SendIO :: IO a -> (a -> m k) -> IOEffect m k

instance Functor m => Functor (IOEffect m) where
  fmap f (SendIO ioAction kont) = SendIO ioAction (fmap f . kont)

instance HFunctor IOEffect where
  hmap nt (SendIO ioAction kont) = SendIO ioAction (nt . kont)

instance Effect IOEffect where
  thread ctx handle (SendIO ioAction kont) = SendIO ioAction (\a -> handle (ctx $> kont a))

sendIO :: (Has IOEffect sig m) => IO a -> m a
sendIO ioAction = send $ SendIO ioAction pure
