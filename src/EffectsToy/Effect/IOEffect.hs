{-# LANGUAGE DeriveAnyClass #-}
module EffectsToy.Effect.IOEffect
  ( IOEffect (..)
  , runIO
  -- * Re-exports
  , Algebra
  , Has
  , run
  ) where

import           GHC.Generics (Generic1)
import           Control.Algebra

data IOEffect m k
  = RunIO (IO ()) (m k)
  deriving (Generic1, Functor, HFunctor, Effect)

runIO :: (Has IOEffect sig m) => IO () -> m ()
runIO ioAction = send $ RunIO ioAction (pure ())
