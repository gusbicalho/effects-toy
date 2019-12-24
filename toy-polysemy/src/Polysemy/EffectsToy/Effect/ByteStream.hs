{-# LANGUAGE DeriveAnyClass #-}
module Polysemy.EffectsToy.Effect.ByteStream
  (
  ) where
--   ( ByteStream (..)
--   , tellChunk
--   -- * Re-exports
--   , Algebra
--   , Has
--   , run
--   ) where

-- import           GHC.Generics (Generic1)
-- import           Control.Algebra
-- import qualified Data.ByteString.Lazy as LBS

-- data ByteStream m k
--   = TellChunk LBS.ByteString (m k)
--   deriving (Generic1, Functor, HFunctor, Effect)

-- tellChunk :: (Has ByteStream sig m) => LBS.ByteString -> m ()
-- tellChunk chunk = send $ TellChunk chunk (pure ())
