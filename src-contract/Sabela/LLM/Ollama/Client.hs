{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Minimal client for Ollama's native @/api/chat@ with tool calling.
module Sabela.LLM.Ollama.Client (
    ToolCall (..),
    Turn (..),
    OllamaReqOpts (..),
    turnThinking,
    ollamaBaseUrl,
    chat,
    chatWith,
    chatSeeded,
    chatRequestBody,
    parseTurn,
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

data ToolCall = ToolCall
    { tcName :: Text
    , tcArgs :: Value
    }
    deriving (Show, Eq)

data Turn = Turn
    { turnRaw :: Value
    , turnContent :: Text
    , turnCalls :: [ToolCall]
    }
    deriving (Show)

{- | The model's reasoning channel, read from 'turnRaw'. gpt-oss sometimes emits
the whole tool-call JSON here instead of a native tool_call; the recovery layer
inspects it. Empty when absent.
-}
turnThinking :: Turn -> Text
turnThinking t = case turnRaw t of
    Object m -> textField "thinking" m
    _ -> ""

{- | Ollama base URL, overridable via @OLLAMA_HOST@ (a full @http://host:port@).
Default is the local daemon. Used for @/api/chat@ and the preflight reachability
probe so both target the same endpoint.
-}
ollamaBaseUrl :: IO String
ollamaBaseUrl = fromMaybe "http://localhost:11434" <$> lookupEnv "OLLAMA_HOST"

chat :: Manager -> Text -> [Value] -> [Value] -> IO (Either Text Turn)
chat = chatSeeded False Nothing

{- | Like 'chat' but with an explicit @num_ctx@ (from workspace config), which
overrides the @OLLAMA_NUM_CTX@ env fallback 'chatSeeded' would otherwise read.
Used by the in-notebook Ollama provider so the setting is per-workspace.
-}
chatWith ::
    Int -> Manager -> Text -> [Value] -> [Value] -> IO (Either Text Turn)
chatWith numCtx = chatCore False Nothing numCtx

{- | Chat once. @think@ asks Ollama for the model's reasoning channel (nested in
@message.thinking@, so it rides along in 'turnRaw'); leave it off for benchmark
runs, since it changes model behaviour and adds latency. Resolves @num_ctx@ from
the @OLLAMA_NUM_CTX@ env var (default 32768) — the harness path.
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
    -- Without an explicit num_ctx, Ollama caps context at its ~4096 default and
    -- silently truncates the OLDEST messages — evicting the system prompt, the
    -- task, and early findings as the tool loop grows, so the model "forgets"
    -- and goes in circles. Hold the whole episode (override via OLLAMA_NUM_CTX).
    numCtx <- fromMaybe 32768 . (>>= readMaybe) <$> lookupEnv "OLLAMA_NUM_CTX"
    chatCore think mseed numCtx mgr model messages tools

{- | The shared @/api/chat@ round-trip with an explicit @num_ctx@. 'chatSeeded'
resolves it from the env; 'chatWith' takes it from config.
-}
chatCore ::
    Bool ->
    Maybe Int ->
    Int ->
    Manager ->
    Text ->
    [Value] ->
    [Value] ->
    IO (Either Text Turn)
chatCore think mseed numCtx mgr model messages tools = do
    keepAlive <- T.pack . fromMaybe "30m" <$> lookupEnv "OLLAMA_KEEP_ALIVE"
    let opts =
            OllamaReqOpts
                { oroThink = think
                , oroSeed = mseed
                , oroKeepAlive = keepAlive
                , oroNumCtx = numCtx
                }
        body = chatRequestBody opts model messages tools
    base <- ollamaBaseUrl
    er <- try (parseRequest (base <> "/api/chat"))
    case er of
        Left (e :: SomeException) -> pure (Left (T.pack (show e)))
        Right req0 -> do
            let req =
                    req0
                        { method = "POST"
                        , requestHeaders = [("content-type", "application/json")]
                        , requestBody = RequestBodyLBS (encode body)
                        , responseTimeout = responseTimeoutMicro 600000000
                        , -- Ollama returns the tool-arg parse error as a 500
                          -- with a JSON body; keep the body instead of throwing
                          -- so the recovery layer can read 'raw=...' from it.
                          checkResponse = \_ _ -> pure ()
                        }
            res <- try (httpLbs req mgr)
            pure $ case res of
                Left (e :: SomeException) -> Left (T.pack (show e))
                Right r -> parseTurn (responseBody r)

{- | The env-derived knobs for one @/api/chat@ request, resolved by 'chatSeeded'
and passed to the pure 'chatRequestBody'.
-}
data OllamaReqOpts = OllamaReqOpts
    { oroThink :: Bool
    , oroSeed :: Maybe Int
    , oroKeepAlive :: Text
    , oroNumCtx :: Int
    }
    deriving (Show, Eq)

{- | Build the @/api/chat@ request body. Pure so the wire shape (num_ctx,
keep_alive, stream:false, options) is pinned by a golden test independent of the
env lookups and the HTTP call in 'chatSeeded'.
-}
chatRequestBody :: OllamaReqOpts -> Text -> [Value] -> [Value] -> Value
chatRequestBody o model messages tools =
    object
        [ "model" .= model
        , "stream" .= False
        , "think" .= oroThink o
        , "messages" .= messages
        , "tools" .= tools
        , "options" .= opts
        , "keep_alive" .= oroKeepAlive o
        ]
  where
    opts =
        object $
            [ "temperature" .= (0.4 :: Double)
            , "num_ctx" .= (oroNumCtx o :: Int)
            ]
                ++ ["seed" .= s | Just s <- [oroSeed o]]

parseTurn :: LBS.ByteString -> Either Text Turn
parseTurn raw = case eitherDecodeStrict' (LBS.toStrict raw) of
    Left e -> Left ("ollama: invalid JSON (" <> T.pack e <> "): " <> snippet raw)
    Right (Object o)
        | Just (String e) <- KM.lookup "error" o ->
            case recoverFromError e of
                Just t -> Right t
                Nothing -> Left ("ollama error: " <> e)
        | Just msg@(Object m) <- KM.lookup "message" o ->
            let calls = parseCalls (KM.lookup "tool_calls" m)
                content = textField "content" m
                thinking = textField "thinking" m
             in Right
                    Turn
                        { turnRaw = msg
                        , turnContent = content
                        , turnCalls =
                            if null calls
                                then recoverCalls content thinking
                                else calls
                        }
        | otherwise -> Left ("ollama: no message field: " <> snippet raw)
    Right _ -> Left "ollama: unexpected non-object response"

{- | Recovery (a): Ollama's @error parsing tool call: raw='<args-json>', err=...@
carries only the arguments object, with an unescaped backslash that broke its
JSON parse (e.g. a lambda's @\\acc@). Pull the @raw=@ payload, repair its
escapes, decode it, and synthesise the tool call — inferring the tool name from
the arg fields, since the error does not name the function.
-}
recoverFromError :: Text -> Maybe Turn
recoverFromError err = do
    payload <- extractRawPayload err
    call <- callFromArgsJson payload
    pure (synthTurn call)

{- | Recovery (b): a turn with no native tool_calls whose content (or thinking)
IS a single JSON object with a code-arg shape (@source@ / @new_source@ / @code@).
gpt-oss sometimes emits the tool-call JSON in a channel instead of as a call.
Only the code-arg shapes are recovered, so ordinary prose is never hijacked.
-}
recoverFromContent :: Text -> Maybe ToolCall
recoverFromContent t = do
    obj <- firstJsonObject t
    callFromArgsJson obj

recoverCalls :: Text -> Text -> [ToolCall]
recoverCalls content thinking =
    case recoverFromContent content of
        Just c -> [c]
        Nothing -> maybeToList (recoverFromContent thinking)

synthTurn :: ToolCall -> Turn
synthTurn call =
    Turn
        { turnRaw = object ["role" .= ("assistant" :: Text), "content" .= ("" :: Text)]
        , turnContent = ""
        , turnCalls = [call]
        }

{- | Pull the @<args-json>@ between @raw='@ and the trailing @', err=@ out of an
Ollama tool-call parse error. 'Nothing' when the marker shape is absent.
-}
extractRawPayload :: Text -> Maybe Text
extractRawPayload err = do
    afterRaw <- T.stripPrefix "raw='" (snd (T.breakOn "raw='" err))
    let (payload, rest) = T.breakOn "', err=" afterRaw
    if T.null rest then Nothing else Just payload

{- | Decode a (possibly unescaped) JSON arguments object into a tool call,
inferring the tool name from its fields: @source@ -> insert_cell,
@new_source@ -> replace_cell_source, @code@ -> scratchpad.
-}
callFromArgsJson :: Text -> Maybe ToolCall
callFromArgsJson payload = do
    Object o <- decodeRepaired payload
    name <- inferToolName o
    pure (ToolCall name (Object o))

-- | The only arg shapes recovery synthesises a call for (the code-bearing tools).
inferToolName :: KM.KeyMap Value -> Maybe Text
inferToolName o
    | has "source" = Just "insert_cell"
    | has "new_source" = Just "replace_cell_source"
    | has "code" = Just "scratchpad"
    | otherwise = Nothing
  where
    has k = KM.member (K.fromText k) o

decodeRepaired :: Text -> Maybe Value
decodeRepaired payload =
    case eitherDecodeStrict' (TE.encodeUtf8 (repairJsonEscapes payload)) of
        Right v -> Just v
        Left _ -> Nothing

{- | The first balanced top-level JSON object in a string (so a fenced or
prose-wrapped @{...}@ is recovered), or 'Nothing' if there is no @{@.
-}
firstJsonObject :: Text -> Maybe Text
firstJsonObject t = case T.breakOn "{" t of
    (_, rest) | T.null rest -> Nothing
    (_, rest) -> Just (T.dropWhileEnd (/= '}') rest)

{- | Escape any backslash NOT starting a valid JSON escape, so a model's
unescaped Haskell backslash (a lambda's @\\acc@, a regex @\\.@) no longer breaks
the JSON parse, while a real @\\n@ / @\\t@ / @\\"@ / @\\uXXXX@ is left intact.

>>> repairJsonEscapes "{\"source\":\"\\acc\"}"  -- the \\a becomes \\\\a
-}
repairJsonEscapes :: Text -> Text
repairJsonEscapes = T.pack . go False . T.unpack
  where
    go _ [] = []
    -- Outside a string, backslashes are not escapes; copy verbatim.
    go False (c : cs)
        | c == '"' = c : go True cs
        | otherwise = c : go False cs
    go True ('\\' : c : cs)
        | c `elem` validEscapes = '\\' : c : go True cs
        | otherwise = '\\' : '\\' : c : go True cs
    -- A trailing lone backslash inside a string: double it so JSON parses.
    go True ['\\'] = "\\\\"
    go True ('"' : cs) = '"' : go False cs
    go True (c : cs) = c : go True cs
    validEscapes = "\"\\/bfnrtu" :: [Char]

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
