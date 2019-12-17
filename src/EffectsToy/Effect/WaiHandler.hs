{-# LANGUAGE DeriveAnyClass #-}
module EffectsToy.Effect.WaiHandler
  ( WaiHandler (..)
  , askRequest, tellHeaders, putStatus, tellChunk
  -- * Re-exports
  , Algebra
  , Has
  , run
  ) where

import           GHC.Generics (Generic1)
import           Control.Algebra
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString.Lazy as LBS

data WaiHandler m k
  = AskRequest (Wai.Request -> m k)
  | TellHeaders HTTP.ResponseHeaders (m k)
  | PutStatus HTTP.Status (m k)
  | TellChunk LBS.ByteString (m k)
  deriving (Generic1, Functor, HFunctor, Effect)

askRequest :: (Has WaiHandler sig m) => m Wai.Request
askRequest = send $ AskRequest pure

tellHeaders :: (Has WaiHandler sig m) => HTTP.ResponseHeaders -> m ()
tellHeaders headers = send $ TellHeaders headers (pure ())

putStatus :: (Has WaiHandler sig m) => HTTP.Status -> m ()
putStatus status = send $ PutStatus status (pure ())

tellChunk :: (Has WaiHandler sig m) => LBS.ByteString -> m ()
tellChunk chunk = send $ TellChunk chunk (pure ())
