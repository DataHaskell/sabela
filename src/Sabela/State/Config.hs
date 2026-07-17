{-# LANGUAGE OverloadedStrings #-}

{- | The @.sabela/config.json@ file and the provider knobs it carries: defaults,
reading/rendering the wire shape, and building a 'ModelProvider' from it. All
'App'-free, so "Sabela.State.App" and "Sabela.State.AIConfig" can both use it.
-}
module Sabela.State.Config (
    -- * Provider selection
    ollamaProviderId,
    defaultOllamaModel,
    buildProvider,
    providerNameOf,

    -- * Knob defaults
    defaultNumCtx,
    defaultToolLimit,

    -- * The config file
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

-- | Provider ids accepted in the config. Ollama needs no API key.
ollamaProviderId :: Text
ollamaProviderId = "ollama"

-- | Default Ollama model when the caller selects Ollama without naming one.
defaultOllamaModel :: Text
defaultOllamaModel = "gpt-oss:20b"

{- | Default Ollama context window. Matches the client's own fallback; holds a
whole agent episode so the oldest messages aren't silently evicted.
-}
defaultNumCtx :: Int
defaultNumCtx = 32768

-- | Default per-turn tool-call round cap for the agentic loop.
defaultToolLimit :: Int
defaultToolLimit = 25

{- | Build the backend the loop drives, from the selected provider + config.
@numCtx@ is baked into the Ollama provider (Anthropic ignores it).
-}
buildProvider :: Manager -> Text -> AnthropicConfig -> Int -> ModelProvider
buildProvider mgr provider cfg numCtx
    | provider == ollamaProviderId = ollamaProvider mgr (acModel cfg) numCtx
    | otherwise = anthropicProvider mgr cfg

-- | Recover the provider id from a live backend (its 'mpName' is @ollama:…@/@anthropic@).
providerNameOf :: ModelProvider -> Text
providerNameOf mp
    | ollamaProviderId `T.isPrefixOf` mpName mp = ollamaProviderId
    | otherwise = "anthropic"

{- | The @.sabela/config.json@ wire shape. Kept as a pure function so its keys
(@anthropicKey@ / @anthropicModel@ / @provider@ / @numCtx@ / @toolLimit@ — read
back by 'readConfigFile') are pinned by 'Test.ConfigWireSpec'.
-}
configJson :: Text -> Text -> Text -> Int -> Int -> Value
configJson key model provider numCtx toolLimit =
    object
        [ "anthropicKey" .= key
        , "anthropicModel" .= model
        , "provider" .= provider
        , "numCtx" .= numCtx
        , "toolLimit" .= toolLimit
        ]

{- | Resolve API key + saved model + saved provider + knobs. Env
@ANTHROPIC_API_KEY@ wins for the key; the rest come from the config file.
-}
resolveConfig ::
    FilePath ->
    IO (Maybe String, Maybe Text, Maybe Text, Maybe Int, Maybe Int)
resolveConfig workDir = do
    mEnv <- lookupEnv "ANTHROPIC_API_KEY"
    (fileKey, fileModel, fileProvider, fileNumCtx, fileToolLimit) <-
        readConfigFile workDir
    pure (mEnv <|> fileKey, fileModel, fileProvider, fileNumCtx, fileToolLimit)

{- | Read @.sabela/config.json@. Every field is optional: a missing file, bad
JSON, or an absent key yields 'Nothing' (provider is absent in pre-provider
config files, which the caller reads as @anthropic@).
-}
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

-- | A JSON value as an @Int@ (via aeson's own decoder); 'Nothing' if not integral.
jsonInt :: Value -> Maybe Int
jsonInt v = case fromJSON v of
    Success i -> Just i
    Error _ -> Nothing
