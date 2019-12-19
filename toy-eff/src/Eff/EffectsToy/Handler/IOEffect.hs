module Eff.EffectsToy.Handler.IOEffect
  ( runIOEffect
  , module Eff.EffectsToy.Effect.IOEffect
  ) where

import Control.Effect
import Control.Monad.IO.Class
import Eff.EffectsToy.Effect.IOEffect

data IOEffectMIO
type IOEffectT = HandlerT IOEffectMIO '[]
type instance Handles IOEffectT eff = eff == IOEffect

instance MonadIO m => IOEffect (IOEffectT m) where
  sendIO ioAction = liftIO ioAction

runIOEffect :: EffT IOEffectT m a -> m a
runIOEffect = runHandlerT . runEffT
