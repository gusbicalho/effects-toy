module Eff.EffectsToy.Handler.Trace.StdOut
  ( runTrace
  , module Eff.EffectsToy.Effect.Trace
  ) where

import Control.Effect
import Control.Monad.IO.Class
import Eff.EffectsToy.Effect.Trace
import System.IO

data TraceMIO
type TraceT = HandlerT TraceMIO '[]
type instance Handles TraceT eff = eff == Trace

instance MonadIO m => Trace (TraceT m) where
  trace msg = liftIO (hPutStrLn stdout msg)
  {-# INLINE trace #-}

runTrace :: EffT TraceT m a -> m a
runTrace = runHandlerT . runEffT
