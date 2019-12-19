{-# LANGUAGE UndecidableInstances #-}
module Eff.EffectsToy.Handler.ByteStream.Streaming
  ( runByteStream
  , module Eff.EffectsToy.Effect.ByteStream
  ) where

import           Control.Effect
import           Control.Monad.Trans
import           Control.Monad.Trans.Control
import           Eff.EffectsToy.Effect.ByteStream
import qualified Data.ByteString.Streaming as Streaming
import qualified Data.ByteString.Lazy as LBS
import           Data.Functor.Of ( Of (..) )

type instance Handles StreamingByteStream eff = eff == ByteStream

newtype StreamingByteStream m a = SBS { runSBS :: Streaming.ByteString m a }
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

instance Monad m => ByteStream (StreamingByteStream m) where
  tellChunk chunk = SBS $ Streaming.fromLazy chunk

instance MonadTransControl StreamingByteStream where
  type StT StreamingByteStream a = (LBS.ByteString `Of` a)
  liftWith f = wrap . inEmpty $ f run
    where run :: Monad n => StreamingByteStream n b -> n (StT StreamingByteStream b)
          run = Streaming.toLazy . runSBS
          wrap = SBS . Streaming.mwrap
          inEmpty a = fmap (<$ Streaming.empty) a
  restoreT :: forall m a. Monad m => m (StT StreamingByteStream a) -> StreamingByteStream m a
  restoreT mstate = SBS $ Streaming.mwrap r
    where r :: m (Streaming.ByteString m a)
          r = do (state :> a) <- mstate
                 return $ a <$ Streaming.fromLazy state

runByteStream :: Monad m => EffT StreamingByteStream m a -> m (LBS.ByteString, a)
runByteStream bs = do
  (s :> a) <- Streaming.toLazy
            . runSBS
            . runEffT
            $ bs
  return (s, a)
