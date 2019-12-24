{-# LANGUAGE TemplateHaskell #-}
module Polysemy.EffectsToy.Effect.WaiHandler
  ( WaiHandler (..)
  , askRequest, tellHeaders, putStatus, tellChunk
  ) where

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString.Lazy as LBS
import           Polysemy

data WaiHandler m k where
  AskRequest :: WaiHandler m Wai.Request
  TellHeaders :: HTTP.ResponseHeaders -> WaiHandler m ()
  PutStatus :: HTTP.Status -> WaiHandler m ()
  TellChunk :: LBS.ByteString -> WaiHandler m ()

makeSem ''WaiHandler
