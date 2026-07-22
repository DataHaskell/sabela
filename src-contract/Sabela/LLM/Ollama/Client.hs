{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.LLM.Ollama.Client (
    ToolCall (..),
    Turn (..),
    ParseFailure (..),
    ParseFailureClass (..),
    OllamaReqOpts (..),
    turnThinking,
    ollamaBaseUrl,
    chat,
    chatWith,
    chatWithParsed,
    chatSeeded,
    chatRequestBody,
    parseTurn,
    parseTurnWithTools,
    rawWithCalls,
    repairJsonEscapes,
    recoverFromError,
    recoverFromContent,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), eitherDecodeStrict', encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (
    Manager,
    RequestBody (RequestBodyLBS),
    checkResponse,
    httpLbs,
    method,
    parseRequest,
    requestBody,
    requestHeaders,
    responseBody,
    responseTimeout,
    responseTimeoutMicro,
 )
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Sabela.LLM.Ollama.Client.Parse (
    ParseFailure (..),
    ParseFailureClass (..),
    parseTurnWithTools,
 )
import Sabela.LLM.Ollama.Client.Repair (
    recoverFromContent,
    recoverFromError,
    repairJsonEscapes,
 )
import Sabela.LLM.Ollama.Client.Types (ToolCall (..), Turn (..), rawWithCalls)

turnThinking :: Turn -> Text
turnThinking turn = case turnRaw turn of
    Object fields -> textField "thinking" fields
    _ -> ""

ollamaBaseUrl :: IO String
ollamaBaseUrl = fromMaybe "http://localhost:11434" <$> lookupEnv "OLLAMA_HOST"

chat :: Manager -> Text -> [Value] -> [Value] -> IO (Either Text Turn)
chat = chatSeeded False Nothing

chatWith ::
    Int -> Manager -> Text -> [Value] -> [Value] -> IO (Either Text Turn)
chatWith numCtx mgr model messages tools =
    mapLeft pfReprompt <$> chatWithParsed numCtx mgr model messages tools

chatWithParsed ::
    Int -> Manager -> Text -> [Value] -> [Value] -> IO (Either ParseFailure Turn)
chatWithParsed = chatCore False Nothing

chatSeeded ::
    Bool ->
    Maybe Int ->
    Manager ->
    Text ->
    [Value] ->
    [Value] ->
    IO (Either Text Turn)
chatSeeded think seed mgr model messages tools = do
    numCtx <- fromMaybe 32768 . (>>= readMaybe) <$> lookupEnv "OLLAMA_NUM_CTX"
    mapLeft pfReprompt <$> chatCore think seed numCtx mgr model messages tools

chatCore ::
    Bool ->
    Maybe Int ->
    Int ->
    Manager ->
    Text ->
    [Value] ->
    [Value] ->
    IO (Either ParseFailure Turn)
chatCore think seed numCtx mgr model messages tools = do
    keepAlive <- T.pack . fromMaybe "30m" <$> lookupEnv "OLLAMA_KEEP_ALIVE"
    temp <- fromMaybe 0.4 . (>>= readMaybe) <$> lookupEnv "OLLAMA_TEMPERATURE"
    let opts = OllamaReqOpts think seed keepAlive numCtx temp
    requestChat mgr (chatRequestBody opts model messages tools) tools

requestChat :: Manager -> Value -> [Value] -> IO (Either ParseFailure Turn)
requestChat mgr body tools = do
    base <- ollamaBaseUrl
    parsed <- try (parseRequest (base <> "/api/chat"))
    case parsed of
        Left (err :: SomeException) -> pure (Left (transportFailure err))
        Right initial -> do
            let request =
                    initial
                        { method = "POST"
                        , requestHeaders = [("content-type", "application/json")]
                        , requestBody = RequestBodyLBS (encode body)
                        , responseTimeout = responseTimeoutMicro 600000000
                        , checkResponse = \_ _ -> pure ()
                        }
            response <- try (httpLbs request mgr)
            pure $ case response of
                Left (err :: SomeException) -> Left (transportFailure err)
                Right result -> parseTurnWithTools tools (responseBody result)

data OllamaReqOpts = OllamaReqOpts
    { oroThink :: Bool
    , oroSeed :: Maybe Int
    , oroKeepAlive :: Text
    , oroNumCtx :: Int
    , oroTemperature :: Double
    }
    deriving (Eq, Show)

chatRequestBody :: OllamaReqOpts -> Text -> [Value] -> [Value] -> Value
chatRequestBody opts model messages tools =
    object
        [ "model" .= model
        , "stream" .= False
        , "think" .= oroThink opts
        , "messages" .= messages
        , "tools" .= tools
        , "options" .= options
        , "keep_alive" .= oroKeepAlive opts
        ]
  where
    options =
        object $
            [ "temperature" .= oroTemperature opts
            , "num_ctx" .= oroNumCtx opts
            ]
                ++ ["seed" .= seed | Just seed <- [oroSeed opts]]

parseTurn :: LBS.ByteString -> Either Text Turn
parseTurn raw = case eitherDecodeStrict' (LBS.toStrict raw) of
    Left err -> Left ("ollama: invalid JSON (" <> T.pack err <> "): " <> snippet raw)
    Right (Object outer)
        | Just (String err) <- KM.lookup "error" outer ->
            maybe (Left ("ollama error: " <> err)) Right (recoverFromError err)
        | Just msg@(Object fields) <- KM.lookup "message" outer ->
            let native = parseCalls (KM.lookup "tool_calls" fields)
                content = textField "content" fields
                thinking = textField "thinking" fields
                recovered = if null native then recoverCalls content thinking else native
             in Right
                    Turn
                        { turnRaw = if null native then rawWithCalls msg recovered else msg
                        , turnContent = content
                        , turnCalls = recovered
                        }
        | otherwise -> Left ("ollama: no message field: " <> snippet raw)
    Right _ -> Left "ollama: unexpected non-object response"

recoverCalls :: Text -> Text -> [ToolCall]
recoverCalls content thinking = case recoverFromContent content of
    Just call -> [call]
    Nothing -> maybeToList (recoverFromContent thinking)

parseCalls :: Maybe Value -> [ToolCall]
parseCalls (Just (Array calls)) = mapMaybe one (toList calls)
  where
    one (Object call) = case KM.lookup "function" call of
        Just (Object fn) ->
            Just
                ToolCall
                    { tcName = textField "name" fn
                    , tcArgs = fromMaybe (object []) (KM.lookup "arguments" fn)
                    }
        _ -> Nothing
    one _ = Nothing
parseCalls _ = []

textField :: Text -> KM.KeyMap Value -> Text
textField key fields = case KM.lookup (K.fromText key) fields of
    Just (String value) -> value
    _ -> ""

snippet :: LBS.ByteString -> Text
snippet = T.take 300 . TE.decodeUtf8Lenient . LBS.toStrict

transportFailure :: SomeException -> ParseFailure
transportFailure err =
    ParseFailure TransportFailure (T.pack (show err)) (T.pack (show err)) Nothing

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right
