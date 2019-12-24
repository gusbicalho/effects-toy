{-# LANGUAGE UndecidableInstances #-}
module FusedEffects.EffectsToy.Carrier.Trace.StdOut
( -- * Trace carrier
  runTrace
, TraceC(..)
  -- * Trace effect
, module Control.Effect.Trace
) where

import Control.Algebra
import Control.Applicative (Alternative(..))
import Control.Effect.Trace
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import System.IO

runTrace :: TraceC m a -> m a
runTrace (TraceC m) = m

newtype TraceC m a = TraceC (m a)
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance (MonadIO m, Algebra sig m) => Algebra (Trace :+: sig) (TraceC m) where
  alg (L (Trace s k)) = liftIO (hPutStrLn stdout s) *> k
  alg (R other)       = TraceC (alg (handleCoercible other))
  {-# INLINE alg #-}
