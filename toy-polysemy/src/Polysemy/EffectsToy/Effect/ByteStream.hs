{-# LANGUAGE TemplateHaskell #-}
module Polysemy.EffectsToy.Effect.ByteStream
  ( ByteStream (..)
  , tellChunk
  ) where

import qualified Data.ByteString.Lazy as LBS
import           Polysemy

data ByteStream m k where
  TellChunk :: LBS.ByteString -> ByteStream m ()

makeSem ''ByteStream
