{-# LANGUAGE UndecidableInstances #-}
module Eff.EffectsToy.Effect.IOEffect
  ( IOEffect (..)
  ) where

import Control.Effect

class Monad m => IOEffect m where
  sendIO :: IO () -> m ()

instance (Monad (t m), Send IOEffect t m) => IOEffect (EffT t m) where
  sendIO ioAction = send @IOEffect (sendIO ioAction)
