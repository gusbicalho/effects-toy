{-# LANGUAGE DeriveAnyClass #-}
module EffectsToy.Effect.WaiHandler
  ( WaiHandler (..)
  , askRequest, tellHeaders, putStatus, sendChunk
  -- * Re-exports
  , Algebra
  , Has
  , run
  ) where

import           GHC.Generics (Generic1)
import           Control.Algebra
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString as BS

data WaiHandler m k
  = AskRequest (Wai.Request -> m k)
  | TellHeaders HTTP.ResponseHeaders (m k)
  | PutStatus HTTP.Status (m k)
  | SendChunk BS.ByteString (m k)
  deriving (Generic1, Functor, HFunctor, Effect)

askRequest :: (Has WaiHandler sig m) => m Wai.Request
askRequest = send $ AskRequest pure

tellHeaders :: (Has WaiHandler sig m) => HTTP.ResponseHeaders -> m ()
tellHeaders headers = send $ TellHeaders headers (pure ())

putStatus :: (Has WaiHandler sig m) => HTTP.Status -> m ()
putStatus status = send $ PutStatus status (pure ())

sendChunk :: (Has WaiHandler sig m) => BS.ByteString -> m ()
sendChunk chunk = send $ SendChunk chunk (pure ())
