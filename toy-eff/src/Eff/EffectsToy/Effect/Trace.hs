{-# LANGUAGE UndecidableInstances #-}
module Eff.EffectsToy.Effect.Trace
  ( Trace (..)
  ) where

import Control.Effect

class Monad m => Trace m where
  trace :: String -> m ()

instance (Monad (t m), Send Trace t m) => Trace (EffT t m) where
  trace ioAction = send @Trace (trace ioAction)
  {-# INLINE trace #-}
