{-# LANGUAGE OverloadedStrings #-}

{- | Pins the Anthropic 'ModelProvider' adapter's anti-corruption layer:
'buildRequest' must reproduce the prompt-cache policy that used to live in the
notebook loop (the billing guard), and 'responseToCompletion' must map wire
content/stop/usage onto the neutral kernel.
-}
module Test.ProviderAdapterSpec (spec) where

import Data.Aeson (ToJSON, Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Test.Hspec

import Sabela.AI.Capabilities.ToolName (ToolName, parseToolName)
import Sabela.AI.Types (errOutcome, okOutcome)
import qualified Sabela.Anthropic.Types as A
import Sabela.Ids (ToolCallId (..))
import Sabela.LLM.Anthropic (buildRequest, responseToCompletion)
import Sabela.LLM.Completion (Completion (..), StopCondition (..))
import Sabela.LLM.Message (
    ContentPart (..),
    Message (..),
    Role (..),
    ToolCall (..),
    ToolResult (..),
 )
import Sabela.LLM.Ollama (renderMessage, renderTool)
import Sabela.LLM.Provider (CompletionRequest (..))
import Sabela.LLM.Tool (ToolSpec (..))
import Sabela.LLM.Usage (TokenUsage (..))

toJson :: (ToJSON a) => a -> Value
toJson x = case decode (encode x) of
    Just v -> v
    Nothing -> error "decode"

field :: String -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromString k) o
field _ _ = Nothing

arrayItems :: Value -> [Value]
arrayItems (Array a) = toList a
arrayItems _ = []

ttlOf :: Value -> Maybe Value
ttlOf v = field "cache_control" v >>= field "ttl"

mkName :: Text -> ToolName
mkName s = fromMaybe (error ("bad tool name: " <> show s)) (parseToolName s)

spec :: Spec
spec = describe "provider adapters (ACL)" $ do
    let req =
            CompletionRequest
                { crSystem = ["SYS", "NB"]
                , crMessages = []
                , crTools =
                    [ ToolSpec (mkName "list_cells") "d1" (object [])
                    , ToolSpec (mkName "read_cell") "d2" (object [])
                    ]
                , crMaxTokens = 4096
                }
        body = toJson (buildRequest "claude-x" req)

    describe "buildRequest preserves the cache policy" $ do
        let sys = maybe [] arrayItems (field "system" body)
            tools = maybe [] arrayItems (field "tools" body)
        it "1-hour TTL on the stable prefix, default on the volatile block" $
            case sys of
                (b0 : b1 : _) -> do
                    ttlOf b0 `shouldBe` Just (String "1h")
                    field "cache_control" b1 `shouldNotBe` Nothing
                    ttlOf b1 `shouldBe` Nothing
                _ -> expectationFailure "expected two system blocks"
        it "caches only the last tool" $
            case (tools, reverse tools) of
                (t0 : _, tl : _) -> do
                    ttlOf tl `shouldBe` Just (String "1h")
                    field "cache_control" t0 `shouldBe` Nothing
                _ -> expectationFailure "expected tools"
        it "carries model, max_tokens, stream" $ do
            field "model" body `shouldBe` Just (String "claude-x")
            field "max_tokens" body `shouldBe` Just (toJson (4096 :: Int))
            field "stream" body `shouldBe` Just (Bool True)

    describe "responseToCompletion maps the wire response" $ do
        let resp =
                A.MessageResponse
                    "id"
                    [A.TextBlock "hi", A.ToolUseBlock "tu_1" "insert_cell" (object [])]
                    (Just A.SRToolUse)
                    (Just (A.Usage 10 20 (Just 3) Nothing))
            c = responseToCompletion resp
        it "derives WantsTools from a tool_use stop" $
            compStop c `shouldBe` WantsTools
        it "maps text and tool_use blocks, preserving the tool_use_id" $
            compParts c
                `shouldBe` [ TextPart "hi"
                           , ToolCallPart (ToolCall (ToolCallId "tu_1") "insert_cell" (object []))
                           ]
        it "carries token usage including cache counters" $
            compUsage c `shouldBe` TokenUsage 10 20 (Just 3) Nothing
        it "maps end_turn to Done and max_tokens to Truncated" $ do
            let mk sr = responseToCompletion (A.MessageResponse "i" [] (Just sr) Nothing)
            compStop (mk A.SREndTurn) `shouldBe` Done
            compStop (mk A.SRMaxTokens) `shouldBe` Truncated

    describe "responseToCompletion decodes/repairs streamed tool_use (C1/C2/M1)" $ do
        it "decodes a streamed String tool_use input into an object" $ do
            let resp =
                    A.MessageResponse
                        "i"
                        [A.ToolUseBlock "tu" "insert_cell" (String "{\"source\":\"x = 1\"}")]
                        (Just A.SRToolUse)
                        Nothing
            compParts (responseToCompletion resp)
                `shouldBe` [ ToolCallPart
                                ( ToolCall
                                    (ToolCallId "tu")
                                    "insert_cell"
                                    (object ["source" .= ("x = 1" :: Text)])
                                )
                           ]
        it "surfaces _parseError (not a broken string) for a truncated input" $ do
            let resp =
                    A.MessageResponse
                        "i"
                        [A.ToolUseBlock "tu" "insert_cell" (String "{\"source\":\"x = 1")]
                        Nothing
                        Nothing
            case compParts (responseToCompletion resp) of
                [ToolCallPart tc] -> do
                    isJust (field "_parseError" (tcInput tc)) `shouldBe` True
                    field "raw" (tcInput tc) `shouldBe` Just (String "{\"source\":\"x = 1")
                _ -> expectationFailure "expected one tool call"
        it "drops empty text blocks so they are not re-sent" $ do
            let resp =
                    A.MessageResponse
                        "i"
                        [A.TextBlock "", A.TextBlock "hi"]
                        (Just A.SREndTurn)
                        Nothing
            compParts (responseToCompletion resp) `shouldBe` [TextPart "hi"]

    describe "Anthropic outbound tool-result rendering" $ do
        let msgWith o =
                buildRequest
                    "m"
                    ( CompletionRequest
                        []
                        [Message User [ToolResultPart (ToolResult (ToolCallId "tu9") "insert_cell" o)]]
                        []
                        4096
                    )
            firstBlock req =
                case maybe [] arrayItems (field "messages" (toJson req)) of
                    (m : _) -> case maybe [] arrayItems (field "content" m) of
                        (b : _) -> Just b
                        _ -> Nothing
                    _ -> Nothing
        it "emits a tool_result with the id and is_error on a failure" $ do
            let b = firstBlock (msgWith (errOutcome (object [])))
            (b >>= field "type") `shouldBe` Just (String "tool_result")
            (b >>= field "tool_use_id") `shouldBe` Just (String "tu9")
            (b >>= field "is_error") `shouldBe` Just (Bool True)
        it "omits is_error on success" $ do
            let b = firstBlock (msgWith (okOutcome (object [])))
            (b >>= field "is_error") `shouldBe` Nothing

    describe "Ollama outbound rendering" $ do
        it "fans a user message with text + two results into three messages" $ do
            let m =
                    Message
                        User
                        [ TextPart "note"
                        , ToolResultPart
                            (ToolResult (ToolCallId "a") "list_cells" (okOutcome (object [])))
                        , ToolResultPart
                            (ToolResult (ToolCallId "b") "read_cell" (okOutcome (String "hi")))
                        ]
            length (renderMessage m) `shouldBe` 3
        it "marks a failed tool result with a TOOL ERROR: prefix" $
            case renderMessage
                ( Message
                    User
                    [ ToolResultPart
                        (ToolResult (ToolCallId "a") "list_cells" (errOutcome (String "boom")))
                    ]
                ) of
                [v] -> field "content" v `shouldBe` Just (String "TOOL ERROR: boom")
                _ -> expectationFailure "expected one message"
        it "carries assistant tool calls in tool_calls" $
            case renderMessage
                ( Message
                    Assistant
                    [ToolCallPart (ToolCall (ToolCallId "x") "insert_cell" (object []))]
                ) of
                [v] -> field "tool_calls" v `shouldNotBe` Nothing
                _ -> expectationFailure "expected one message"
        it "renders a ToolSpec into Ollama's function envelope" $ do
            let v = renderTool (ToolSpec (mkName "list_cells") "desc" (object []))
            field "type" v `shouldBe` Just (String "function")
            (field "function" v >>= field "name") `shouldBe` Just (String "list_cells")
