{-# LANGUAGE OverloadedStrings #-}

{- | The recovery layer: code-bearing tools (insert_cell, replace_cell_source,
scratchpad) take their Haskell straight from the (recovered) tool-call args.
gpt-oss usually escapes backslashes correctly, but intermittently (1) emits an
unescaped backslash that makes Ollama return a 500 @error parsing tool call:
raw='...', err=...@, or (2) emits the whole tool-call JSON in the content/thinking
channel instead of a native tool_call. These tests pin 'repairJsonEscapes',
error-body recovery, content-JSON recovery, and that a normal tool_call is
untouched.
-}
module Test.CodePathSpec (spec) where

import Data.Aeson (Value (..), eitherDecodeStrict', encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Eval.Ollama (
    ToolCall (..),
    Turn (..),
    parseTurn,
    recoverFromContent,
    recoverFromError,
    repairJsonEscapes,
 )

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (K.fromText k) o
field _ _ = Nothing

-- | A lambda's backslash gpt-oss intermittently leaves unescaped in tool args.
lambdaSrc :: Text
lambdaSrc = "shortCode = foldl (\\acc b -> acc * 58 + b) 0"

{- | The exact error shape Ollama returns on a 500 (captured from /api/chat):
@error parsing tool call: raw='{"source":"...\acc..."}', err=invalid character 'a' ...@
Note the @\a@ is a single literal backslash in the payload (unescaped).
-}
capturedError :: Text
capturedError =
    "error parsing tool call: raw='{\"source\":\"base58 :: [Int] -> String\\n\
    \base58 = foldl (\\acc b -> acc * 58 + b) 0\\n\"}', \
    \err=invalid character 'a' in string escape code"

spec :: Spec
spec = describe "Recovery layer (code from tool args, repaired)" $ do
    describe "repairJsonEscapes" $ do
        it "doubles a backslash that is not a valid JSON escape (\\acc)" $
            repairJsonEscapes "{\"s\":\"\\acc\"}"
                `shouldBe` "{\"s\":\"\\\\acc\"}"
        it "preserves a valid \\n / \\t / \\\" escape" $
            repairJsonEscapes "{\"s\":\"a\\nb\\tc\\\"d\"}"
                `shouldBe` "{\"s\":\"a\\nb\\tc\\\"d\"}"
        it "preserves an already-escaped backslash (\\\\)" $
            repairJsonEscapes "{\"s\":\"\\\\acc\"}"
                `shouldBe` "{\"s\":\"\\\\acc\"}"
        it "leaves a clean string unchanged" $
            repairJsonEscapes "{\"source\":\"x = 1\"}"
                `shouldBe` "{\"source\":\"x = 1\"}"
        it "does not touch a backslash outside a string" $
            repairJsonEscapes "{\\}" `shouldBe` "{\\}"
        it "makes \\acc parseable, recovering the backslash intact" $ do
            let repaired = repairJsonEscapes "{\"source\":\"f = \\acc\"}"
            case eitherDecodeStrict' (TE.encodeUtf8 repaired) of
                Right v -> field "source" v `shouldBe` Just (String "f = \\acc")
                Left e -> expectationFailure ("still unparseable: " <> e)

    describe "recoverFromError (a)" $ do
        it "recovers insert_cell + source from the captured 500 body" $
            case recoverFromError capturedError of
                Just (Turn _ _ [ToolCall name args]) -> do
                    name `shouldBe` "insert_cell"
                    field "source" args
                        `shouldBe` Just
                            (String "base58 :: [Int] -> String\nbase58 = foldl (\\acc b -> acc * 58 + b) 0\n")
                other -> expectationFailure ("no single recovered call: " <> show other)
        it "infers replace_cell_source from a {new_source, cell_id} payload" $ do
            let e =
                    "error parsing tool call: raw='{\"cell_id\":3,\
                    \\"new_source\":\"g = \\acc\"}', err=invalid character 'a' ..."
            case recoverFromError e of
                Just (Turn _ _ [ToolCall name args]) -> do
                    name `shouldBe` "replace_cell_source"
                    field "cell_id" args `shouldBe` Just (Number 3)
                    field "new_source" args `shouldBe` Just (String "g = \\acc")
                other -> expectationFailure ("unexpected: " <> show other)
        it "infers scratchpad from a {code} payload" $
            case recoverFromError
                "error parsing tool call: raw='{\"code\":\"h = \\x -> x\"}', err=..." of
                Just (Turn _ _ [ToolCall name _]) -> name `shouldBe` "scratchpad"
                other -> expectationFailure ("unexpected: " <> show other)
        it "recovers a tool call whose raw payload is prefixed by reasoning prose" $
            case recoverFromError
                "error parsing tool call: raw='We should insert a cell. \
                \{\"cell_type\":\"CodeCell\",\"source\":\"x = 1\"}', \
                \err=invalid character 'W' looking for beginning of value" of
                Just (Turn _ _ [ToolCall name args]) -> do
                    name `shouldBe` "insert_cell"
                    field "source" args `shouldBe` Just (String "x = 1")
                other -> expectationFailure ("no recovery from prose-prefixed raw: " <> show other)
        it "does not recover an unrelated error" $
            isNothing (recoverFromError "model not found") `shouldBe` True

    describe "recoverFromContent (b)" $ do
        it "recovers a code-arg JSON object emitted as content" $
            case recoverFromContent "{\"source\":\"x = 1\"}" of
                Just (ToolCall name args) -> do
                    name `shouldBe` "insert_cell"
                    field "source" args `shouldBe` Just (String "x = 1")
                Nothing -> expectationFailure "expected a recovered call"
        it "recovers a content-JSON object even with an unescaped backslash" $
            case recoverFromContent "{\"source\":\"f = \\acc\"}" of
                Just (ToolCall _ args) ->
                    field "source" args `shouldBe` Just (String "f = \\acc")
                Nothing -> expectationFailure "expected a recovered call"
        it "ignores ordinary prose (no code-arg JSON shape)" $
            recoverFromContent "I'll define total as a fold." `shouldBe` Nothing
        it "ignores a JSON object without a code-arg key" $
            recoverFromContent "{\"query\":\"linear regression\"}" `shouldBe` Nothing

    describe "parseTurn (the full wire path)" $ do
        it "passes a normal correctly-escaped tool_call through unchanged" $
            case parseTurn (encode (chatBody [callObj "insert_cell" lambdaSrc])) of
                Right (Turn _ _ [ToolCall name args]) -> do
                    name `shouldBe` "insert_cell"
                    field "source" args `shouldBe` Just (String lambdaSrc)
                other -> expectationFailure ("expected a passthrough call: " <> show other)
        it "synthesises a call from a 500 error body" $
            case parseTurn (encode (object ["error" .= capturedError])) of
                Right (Turn _ _ [ToolCall "insert_cell" _]) -> pure ()
                other -> expectationFailure ("expected recovery: " <> show other)
        it "recovers a content-channel tool-call JSON when no native call" $
            case parseTurn (encode (chatContent "{\"source\":\"x = 1\"}")) of
                Right (Turn _ _ [ToolCall "insert_cell" args]) ->
                    field "source" args `shouldBe` Just (String "x = 1")
                other -> expectationFailure ("expected content recovery: " <> show other)
        it "leaves an ordinary prose turn with no calls" $
            case parseTurn (encode (chatContent "All done.")) of
                Right t -> turnCalls t `shouldBe` []
                Left e -> expectationFailure ("unexpected Left: " <> show e)

callObj :: Text -> Text -> Value
callObj name src =
    object
        [ "function"
            .= object ["name" .= name, "arguments" .= object ["source" .= src]]
        ]

chatBody :: [Value] -> Value
chatBody calls =
    object
        [ "message"
            .= object
                [ "role" .= ("assistant" :: Text)
                , "content" .= ("" :: Text)
                , "tool_calls" .= calls
                ]
        ]

chatContent :: Text -> Value
chatContent content =
    object
        [ "message"
            .= object ["role" .= ("assistant" :: Text), "content" .= content]
        ]
