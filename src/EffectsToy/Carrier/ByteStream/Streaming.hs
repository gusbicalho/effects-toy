{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module EffectsToy.Carrier.ByteStream.Streaming
  ( ByteStreamC, runByteStream, Of (..), LazyBS.ByteString
  , module EffectsToy.Effect.ByteStream
  ) where

import           Control.Algebra
import Control.Monad.IO.Class
import           Control.Carrier.Lift
import qualified Data.ByteString.Streaming as Streaming
import qualified Data.ByteString.Lazy as LazyBS
import           Data.Functor.Of ( Of(..))
import           EffectsToy.Effect.ByteStream

import Data.String (fromString)
import System.IO.Unsafe

type ByteStreamIO = Streaming.ByteString IO

newtype ByteStreamC m a = ByteStreamC { runByteStreamC :: m a }
  deriving newtype (Functor, Applicative, Monad)

(<<) :: Monad m => m a -> m b -> m a
(<<) = flip (>>)

tap l x = unsafePerformIO (putStrLn l) `seq` x

instance ( Algebra sig m
         , Effect sig
         , Has (Lift (ByteStreamIO)) sig m
         ) => Algebra (ByteStream :+: sig) (ByteStreamC m) where
  alg (L (SendChunk chunk k)) = do sendM @ByteStreamIO (Streaming.fromStrict chunk)
                                   k
  alg (R other) = send other
  {-# INLINE alg #-}

runByteStream :: ByteStreamC _ a -> IO (LazyBS.ByteString `Of` a)
runByteStream bsc =
  let result = do liftIO $ putStrLn "before"
                  res <- runM @ByteStreamIO . runByteStreamC $ bsc
                  liftIO $ putStrLn "after"
                  return res
  in  Streaming.toLazy result

dope = do
  a <- Streaming.toLazy . runM @ByteStreamIO . runByteStreamC $ (
          tap "one" (return "two") >>= \q ->
          tap q (sendChunk (fromString q)) >>= \w ->
          return (w `seq` ())
       )
  putStrLn (show a)

noice = do
  a <- Streaming.toLazy . runM @ByteStreamIO $ (
          tap "one" (return "two") >>= \q ->
          tap q (sendM @ByteStreamIO (fromString q)) >>= \w ->
          return (w `seq` ())
        )
  putStrLn (show a)
  