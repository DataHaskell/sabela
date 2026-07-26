{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Siza.Agent.Preflight (
    Preflight (..),
    classifyPreflight,
    preflightMessage,
    ensureOllama,
) where

import Control.Exception (SomeException, try)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (
    Manager,
    httpLbs,
    parseRequest,
    responseTimeout,
    responseTimeoutMicro,
 )
import System.Directory (findExecutable)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Sabela.LLM.Ollama.Client (ollamaBaseUrl)

data Preflight = Ready | MissingBinary | Unreachable
    deriving (Eq, Show)

classifyPreflight :: Bool -> Bool -> Preflight
classifyPreflight hasBinary reachable
    | not hasBinary = MissingBinary
    | not reachable = Unreachable
    | otherwise = Ready

preflightMessage :: Preflight -> String -> Maybe Text
preflightMessage p base = case p of
    Ready -> Nothing
    MissingBinary ->
        Just
            "ollama is not installed or not on PATH. Install it from \
            \https://ollama.com, then pull a model (e.g. `ollama pull gpt-oss:20b`)."
    Unreachable ->
        Just
            ( "ollama is installed but not reachable at "
                <> T.pack base
                <> ". Start it with `ollama serve` (or set OLLAMA_HOST)."
            )

ollamaReachable :: Manager -> String -> IO Bool
ollamaReachable mgr base = do
    er <- try (parseRequest (base <> "/api/tags"))
    case er of
        Left (_ :: SomeException) -> pure False
        Right req0 -> do
            let req = req0{responseTimeout = responseTimeoutMicro 3000000}
            r <- try (httpLbs req mgr)
            pure $ case r of
                Left (_ :: SomeException) -> False
                Right _ -> True

ensureOllama :: Manager -> IO ()
ensureOllama mgr = do
    base <- ollamaBaseUrl
    hasBin <- isJust <$> findExecutable "ollama"
    reachable <- ollamaReachable mgr base
    case preflightMessage (classifyPreflight hasBin reachable) base of
        Nothing -> pure ()
        Just msg -> hPutStrLn stderr ("siza: " <> T.unpack msg) >> exitFailure
