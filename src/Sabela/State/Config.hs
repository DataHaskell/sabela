{-# LANGUAGE OverloadedStrings #-}

module Sabela.State.Config (
    ollamaProviderId,
    defaultOllamaModel,
    buildProvider,
    providerNameOf,
    defaultNumCtx,
    defaultToolLimit,
    configJson,
    resolveConfig,
    readConfigFile,
) where

import Control.Applicative ((<|>))
import Data.Aeson (
    Result (..),
    Value (..),
    eitherDecodeStrict,
    fromJSON,
    object,
    (.=),
 )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

import Sabela.Anthropic.Types (AnthropicConfig (..))
import Sabela.LLM.Anthropic (anthropicProvider)
import Sabela.LLM.Ollama (ollamaProvider)
import Sabela.LLM.Provider (ModelProvider, mpName)

ollamaProviderId :: Text
ollamaProviderId = "ollama"

defaultOllamaModel :: Text
defaultOllamaModel = "gpt-oss:20b"

defaultNumCtx :: Int
defaultNumCtx = 32768

defaultToolLimit :: Int
defaultToolLimit = 25

buildProvider :: Manager -> Text -> AnthropicConfig -> Int -> ModelProvider
buildProvider mgr provider cfg numCtx
    | provider == ollamaProviderId = ollamaProvider mgr (acModel cfg) numCtx
    | otherwise = anthropicProvider mgr cfg

providerNameOf :: ModelProvider -> Text
providerNameOf mp
    | ollamaProviderId `T.isPrefixOf` mpName mp = ollamaProviderId
    | otherwise = "anthropic"

configJson :: Text -> Text -> Text -> Int -> Int -> Value
configJson key model provider numCtx toolLimit =
    object
        [ "anthropicKey" .= key
        , "anthropicModel" .= model
        , "provider" .= provider
        , "numCtx" .= numCtx
        , "toolLimit" .= toolLimit
        ]

resolveConfig ::
    FilePath ->
    IO (Maybe String, Maybe Text, Maybe Text, Maybe Int, Maybe Int)
resolveConfig workDir = do
    mEnv <- lookupEnv "ANTHROPIC_API_KEY"
    (fileKey, fileModel, fileProvider, fileNumCtx, fileToolLimit) <-
        readConfigFile workDir
    pure (mEnv <|> fileKey, fileModel, fileProvider, fileNumCtx, fileToolLimit)

readConfigFile ::
    FilePath ->
    IO (Maybe String, Maybe Text, Maybe Text, Maybe Int, Maybe Int)
readConfigFile workDir = do
    let configFile = workDir </> ".sabela" </> "config.json"
    exists <- doesFileExist configFile
    if not exists
        then pure (Nothing, Nothing, Nothing, Nothing, Nothing)
        else do
            bs <- BS.readFile configFile
            case eitherDecodeStrict bs of
                Right (Object obj) ->
                    let key = case KM.lookup (Key.fromText "anthropicKey") obj of
                            Just (String s) -> Just (T.unpack s)
                            _ -> Nothing
                        model = case KM.lookup (Key.fromText "anthropicModel") obj of
                            Just (String s) | not (T.null s) -> Just s
                            _ -> Nothing
                        provider = case KM.lookup (Key.fromText "provider") obj of
                            Just (String s) | not (T.null s) -> Just s
                            _ -> Nothing
                        intField k = KM.lookup (Key.fromText k) obj >>= jsonInt
                     in pure
                            ( key
                            , model
                            , provider
                            , intField "numCtx"
                            , intField "toolLimit"
                            )
                _ -> pure (Nothing, Nothing, Nothing, Nothing, Nothing)

jsonInt :: Value -> Maybe Int
jsonInt v = case fromJSON v of
    Success i -> Just i
    Error _ -> Nothing
