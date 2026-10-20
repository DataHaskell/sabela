{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Which models the local Ollama server has pulled, and which of them the
chat should use when nobody has chosen one.
-}
module Sabela.State.OllamaModels (
    installedOllamaModels,
    chooseOllamaModel,
    defaultProvider,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), decode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.List (find)
import Data.Maybe (isJust, mapMaybe)
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

import Sabela.LLM.Ollama.Client (ollamaBaseUrl)
import Sabela.State.Config (defaultOllamaModel, ollamaProviderId)

{- | The tags Ollama reports at @/api/tags@, newest first as Ollama lists
them. 'Nothing' means the server could not be reached at all, which is
different from a server with nothing pulled.
-}
installedOllamaModels :: Manager -> IO (Maybe [Text])
installedOllamaModels mgr = do
    base <- ollamaBaseUrl
    result <- try $ do
        initial <- parseRequest (base <> "/api/tags")
        let request = initial{responseTimeout = responseTimeoutMicro 2000000}
        responseBody <$> httpLbs request mgr
    pure $ case result of
        Left (_ :: SomeException) -> Nothing
        Right body -> Just (maybe [] tagNames (decode body))
  where
    tagNames (Object o) = case KM.lookup (Key.fromText "models") o of
        Just (Array ms) -> mapMaybe nameOf (toList ms)
        _ -> []
    tagNames _ = []
    nameOf (Object m) = case KM.lookup (Key.fromText "name") m of
        Just (String s) -> Just s
        _ -> Nothing
    nameOf _ = Nothing

{- | Families that call tools reliably, most preferred first. A model from
earlier in the list wins over a newer pull from later in it.
-}
preferredFamilies :: [Text]
preferredFamilies =
    [ "gpt-oss"
    , "qwen3"
    , "qwen2.5-coder"
    , "qwen2.5"
    , "devstral"
    , "llama3.3"
    , "llama3.2"
    , "llama3.1"
    , "mistral"
    , "gemma3"
    , "deepseek"
    ]

{- | The model the chat should use: a saved choice always, else the best
installed one, else the documented default so the error, when it comes,
names a tag worth pulling.
-}
chooseOllamaModel :: Maybe [Text] -> Maybe Text -> Text
chooseOllamaModel _ (Just saved) = saved
chooseOllamaModel Nothing Nothing = defaultOllamaModel
chooseOllamaModel (Just installed) Nothing
    | Just m <- preferred = m
    | (m : _) <- usable = m
    | otherwise = defaultOllamaModel
  where
    usable = filter (not . T.isInfixOf "embed") installed
    preferred =
        find
            (const True)
            [m | fam <- preferredFamilies, m <- usable, fam `T.isPrefixOf` m]

{- | Ollama is the default; a key alone only wins when no local model is
there to answer.
-}
defaultProvider :: Maybe [Text] -> Bool -> Text
defaultProvider installed hasKey
    | maybe False (not . null) installed = ollamaProviderId
    | hasKey = "anthropic"
    | isJust installed || not hasKey = ollamaProviderId
    | otherwise = "anthropic"
