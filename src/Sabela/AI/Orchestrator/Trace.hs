{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Orchestrator.Trace (
    traceReset,
    traceEvent,
    snip,
) where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Aeson (ToJSON, Value, encode, object, toJSON)
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.State (App (..), Environment (..))

tracePath :: App -> FilePath
tracePath app = envWorkDir (appEnv app) </> ".sabela" </> "ai-trace.jsonl"

traceReset :: App -> IO ()
traceReset app = guarded $ do
    let p = tracePath app
    createDirectoryIfMissing True (takeDirectory p)
    ignore (BL.writeFile p "")

traceEvent :: App -> [Pair] -> IO ()
traceEvent app kvs = guarded (ignore (BL.appendFile (tracePath app) line))
  where
    line = encode (object kvs :: Value) <> "\n"

snip :: (ToJSON a) => Int -> a -> Value
snip n = toJSON . T.take n . TE.decodeUtf8 . BL.toStrict . encode

guarded :: IO () -> IO ()
guarded act = do
    on <- featureEnabled "SABELA_AI_TRACE"
    when on act

ignore :: IO () -> IO ()
ignore act = do
    _ <- try act :: IO (Either SomeException ())
    pure ()
