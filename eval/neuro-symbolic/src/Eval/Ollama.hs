-- | Minimal client for Ollama's native @/api/chat@ with tool calling.
module Eval.Ollama (
    ToolCall (..),
    Turn (..),
    chat,
    chatSeeded,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), eitherDecode, encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (
    Manager,
    RequestBody (RequestBodyLBS),
    httpLbs,
    method,
    parseRequest,
    requestBody,
    requestHeaders,
    responseBody,
    responseTimeout,
    responseTimeoutMicro,
 )

data ToolCall = ToolCall
    { tcName :: Text
    , tcArgs :: Value
    }
    deriving (Show)

data Turn = Turn
    { turnRaw :: Value
    , turnContent :: Text
    , turnCalls :: [ToolCall]
    }
    deriving (Show)

ollamaChatUrl :: String
ollamaChatUrl = "http://localhost:11434/api/chat"

chat :: Manager -> Text -> [Value] -> [Value] -> IO (Either Text Turn)
chat = chatSeeded False Nothing

{- | Chat once. @think@ asks Ollama for the model's reasoning channel (nested in
@message.thinking@, so it rides along in 'turnRaw'); leave it off for benchmark
runs, since it changes model behaviour and adds latency.
-}
chatSeeded ::
    Bool ->
    Maybe Int ->
    Manager ->
    Text ->
    [Value] ->
    [Value] ->
    IO (Either Text Turn)
chatSeeded think mseed mgr model messages tools = do
    let opts =
            object
                (("temperature" .= (0.4 :: Double)) : ["seed" .= s | Just s <- [mseed]])
        body =
            object
                [ "model" .= model
                , "stream" .= False
                , "think" .= think
                , "messages" .= messages
                , "tools" .= tools
                , "options" .= opts
                ]
    er <- try (parseRequest ollamaChatUrl)
    case er of
        Left (e :: SomeException) -> pure (Left (T.pack (show e)))
        Right req0 -> do
            let req =
                    req0
                        { method = "POST"
                        , requestHeaders = [("content-type", "application/json")]
                        , requestBody = RequestBodyLBS (encode body)
                        , responseTimeout = responseTimeoutMicro 600000000
                        }
            res <- try (httpLbs req mgr)
            pure $ case res of
                Left (e :: SomeException) -> Left (T.pack (show e))
                Right r -> parseTurn (responseBody r)

parseTurn :: LBS.ByteString -> Either Text Turn
parseTurn raw = case eitherDecode raw of
    Left e -> Left ("ollama: invalid JSON (" <> T.pack e <> "): " <> snippet raw)
    Right (Object o)
        | Just (String e) <- KM.lookup "error" o -> Left ("ollama error: " <> e)
        | Just msg@(Object m) <- KM.lookup "message" o ->
            Right
                Turn
                    { turnRaw = msg
                    , turnContent = textField "content" m
                    , turnCalls = parseCalls (KM.lookup "tool_calls" m)
                    }
        | otherwise -> Left ("ollama: no message field: " <> snippet raw)
    Right _ -> Left "ollama: unexpected non-object response"

parseCalls :: Maybe Value -> [ToolCall]
parseCalls (Just (Array a)) = mapMaybe one (toList a)
  where
    one (Object c) = case KM.lookup "function" c of
        Just (Object f) ->
            Just
                ToolCall
                    { tcName = textField "name" f
                    , tcArgs = fromMaybe (object []) (KM.lookup "arguments" f)
                    }
        _ -> Nothing
    one _ = Nothing
parseCalls _ = []

textField :: Text -> KM.KeyMap Value -> Text
textField k m = case KM.lookup (K.fromText k) m of
    Just (String s) -> s
    _ -> ""

snippet :: LBS.ByteString -> Text
snippet = T.take 300 . TE.decodeUtf8Lenient . LBS.toStrict
