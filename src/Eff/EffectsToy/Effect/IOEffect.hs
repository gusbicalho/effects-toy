{-# LANGUAGE DeriveAnyClass #-}
module Eff.EffectsToy.Effect.IOEffect
  ( --IOEffect (..)
  -- , sendIO
  -- * Re-exports
  -- , Algebra
  -- , Has
  -- , run
  ) where

-- import Control.Effect
-- import Control.Effect.Error
-- import Control.Effect.State
-- import Control.Monad.IO.Class
-- import Data.Functor.Identity

-- class Monad m => FileSystem m where
--   readFile :: FilePath -> m String
--   writeFile :: FilePath -> String -> m ()

-- instance (Monad (t m), Send FileSystem t m) => FileSystem (EffT t m) where
--   readFile path = send @FileSystem (readFile path)
--   writeFile path contents = send @FileSystem (writeFile path contents)

-- import           Control.Algebra
-- import           Data.Functor

-- data IOEffect m k where
--   SendIO :: IO a -> (a -> m k) -> IOEffect m k

-- instance Functor m => Functor (IOEffect m) where
--   fmap f (SendIO ioAction kont) = SendIO ioAction (fmap f . kont)

-- instance HFunctor IOEffect where
--   hmap nt (SendIO ioAction kont) = SendIO ioAction (nt . kont)

-- instance Effect IOEffect where
--   thread ctx handle (SendIO ioAction kont) = SendIO ioAction (\a -> handle (ctx $> kont a))

-- sendIO :: (Has IOEffect sig m) => IO a -> m a
-- sendIO ioAction = send $ SendIO ioAction pure
