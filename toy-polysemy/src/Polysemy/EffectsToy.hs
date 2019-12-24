{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Polysemy.EffectsToy
  ( start
  ) where

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.ByteString.Lazy as LBS
import           Data.Function
import           Data.String (fromString)

start :: IO ()
start = do
    runBaseStack initApp
    Warp.run 8087 (runWaiApplication runBaseStack helloWorld)
  where
    -- initApp = initDb
    initApp = return ()

runBaseStack :: _ a -> IO a 
runBaseStack = id
                               
runWaiApplication :: ( Monad n
                     ) => (forall x. n x -> IO x)
                       -> _ ()
                       -> Wai.Application
runWaiApplication runToIO waiApp request respond =
    waiApp
    -- & runWaiHandler request
    -- & BSStrict.runByteStream
    & runToIO
    & (fmap toResponse)
    & (>>= respond)
  where
    -- toResponse (body, (headers, status)) = Wai.responseLBS status headers body
    toResponse _ = Wai.responseLBS HTTP.ok200 [] "hello, world"

-- helloWorld :: ( Has Trace sig m
--               , Has WaiHandler sig m
--               , Has TestDb sig m
--               ) => m ()
helloWorld = do
  return ()
  -- trace "Request received"
  -- req <- askRequest
  -- tellHeaders [(HTTP.hContentType, "text/plain")]
  -- tellChunk "Hello, world!\n"
  -- (reqId, str) <- req & Wai.rawQueryString 
  --                     & LBS.fromStrict
  --                     & storeAndLookup
  -- tellChunk $ "You requested " <> str <> "\n"
  -- tellChunk $ "Your request was number " <> (fromString $ show reqId)
  -- putStatus HTTP.ok200
