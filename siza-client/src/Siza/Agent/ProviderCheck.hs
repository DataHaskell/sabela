{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Startup check that the local Ollama binary and model are reachable
[Infrastructure]. Not a code gate: the parse/security gate is Siza.Preflight.
Formerly Siza.Agent.Preflight.
-}
module Siza.Agent.ProviderCheck (
    Preflight (..),
    classifyPreflight,
    classifyModel,
    preflightMessage,
    ensureOllama,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), decode)
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (
    Manager,
    httpLbs,
    parseRequest,
    responseBody,
    responseTimeout,
    responseTimeoutMicro,
 )
import System.Directory (findExecutable)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Sabela.LLM.Ollama.Client (ollamaBaseUrl)

data Preflight = Ready | MissingBinary | Unreachable | MissingModel Text [Text]
    deriving (Eq, Show)

classifyPreflight :: Bool -> Bool -> Preflight
classifyPreflight hasBinary reachable
    | not hasBinary = MissingBinary
    | not reachable = Unreachable
    | otherwise = Ready

{- | Given the requested model and the server's tag list, report whether the
model is pulled; a bare name matches its @:latest@ tag.
-}
classifyModel :: Text -> [Text] -> Preflight
classifyModel model tags
    | model `elem` tags || (model <> ":latest") `elem` tags = Ready
    | otherwise = MissingModel model tags

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
    MissingModel model tags ->
        Just
            ( "model '"
                <> model
                <> "' is not pulled on the ollama at "
                <> T.pack base
                <> ". Run `ollama pull "
                <> model
                <> "`"
                <> ( if null tags
                        then "; that server has no models at all (wrong OLLAMA_MODELS store?)."
                        else ". Available: " <> T.intercalate ", " tags <> "."
                   )
            )

-- | The server's pulled model tags from @/api/tags@, or Nothing if unreachable.
ollamaTags :: Manager -> String -> IO (Maybe [Text])
ollamaTags mgr base = do
    er <- try (parseRequest (base <> "/api/tags"))
    case er of
        Left (_ :: SomeException) -> pure Nothing
        Right req0 -> do
            let req = req0{responseTimeout = responseTimeoutMicro 3000000}
            r <- try (httpLbs req mgr)
            pure $ case r of
                Left (_ :: SomeException) -> Nothing
                Right resp -> Just (tagsOf (decode (responseBody resp)))

tagsOf :: Maybe Value -> [Text]
tagsOf (Just (Object o))
    | Just (Array models) <- KM.lookup "models" o =
        mapMaybe modelName (toList models)
tagsOf _ = []

modelName :: Value -> Maybe Text
modelName (Object m)
    | Just (String n) <- KM.lookup "name" m = Just n
    | Just (String n) <- KM.lookup "model" m = Just n
modelName _ = Nothing

-- | Exit with a hint unless ollama is installed, reachable, and has @model@.
ensureOllama :: Manager -> Text -> IO ()
ensureOllama mgr model = do
    base <- ollamaBaseUrl
    hasBin <- isJust <$> findExecutable "ollama"
    tags <- ollamaTags mgr base
    let status = case classifyPreflight hasBin (isJust tags) of
            Ready -> classifyModel model (fromMaybe [] tags)
            other -> other
    case preflightMessage status base of
        Nothing -> pure ()
        Just msg -> hPutStrLn stderr ("siza: " <> T.unpack msg) >> exitFailure
