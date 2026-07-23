{-# LANGUAGE OverloadedStrings #-}

{- | Generated corruption-class coverage for the Ollama parse seam. The
payload matrix is schema-driven rather than copied from a benchmark task.
-}
module Test.OllamaParseSafetySpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Sabela.LLM.Ollama (ollamaParseRetries)
import Sabela.LLM.Ollama.Client (
    ParseFailure (..),
    ParseFailureClass (..),
    ToolCall (..),
    Turn (..),
    parseTurnWithTools,
    recoverFromContent,
 )

spec :: Spec
spec = describe "Ollama tool-call parse safety" $ do
    it "bounds parse-error reprompting at two retries" $
        ollamaParseRetries `shouldBe` 2
    it "leaves every clean generated call unchanged" $
        forM_ toolCases $ \(name, args) ->
            case parseTurnWithTools schemas (encode (chatCall name args)) of
                Right t -> turnCalls t `shouldBe` [ToolCall name args]
                Left e -> expectationFailure (show e)

    it "recovers exact-name calls from content and records them" $
        forM_ [(channel, tc) | channel <- ["content", "thinking"], tc <- toolCases] $
            \(channel, (name, args)) ->
                case parseTurnWithTools schemas (encode (channelCall channel name args)) of
                    Right t -> do
                        turnCalls t `shouldBe` [ToolCall name args]
                        show (turnRaw t) `shouldContain` T.unpack name
                    Left e -> expectationFailure (show e)

    it "never dispatches generated garbled names; unique shapes name the fix" $
        forM_ [(garble, tc) | garble <- garbles, tc <- toolCases] $
            \(garble, (name, args)) ->
                case parseTurnWithTools schemas (encode (chatCall (garble name) args)) of
                    Left failure -> do
                        pfClass failure `shouldBe` GarbledToolName
                        pfSuggestedTool failure `shouldBe` Just name
                        pfReprompt failure `shouldSatisfy` T.isInfixOf name
                    Right t -> expectationFailure ("garbage dispatched: " <> show t)

    it "never dispatches truncated, wrapped, malformed, or ballooned payloads" $
        forM_ unsafeBodies $ \body ->
            case parseTurnWithTools schemas body of
                Left failure -> pfClass failure `shouldSatisfy` (`elem` unsafeClasses)
                Right t -> expectationFailure ("unsafe payload dispatched: " <> show t)

    it "recovers a bare code payload as the unified try tool" $
        recoverFromContent "{\"code\":\"1 + 1\"}"
            `shouldBe` Just (ToolCall "try" (object ["code" .= ("1 + 1" :: T.Text)]))

schemas :: [Value]
schemas =
    [ tool "insert_cell" ["source"] ["source"]
    , tool "replace_cell_source" ["cell_id", "new_source"] ["cell_id", "new_source"]
    , tool "try" ["code", "language"] ["code"]
    , tool "discover" ["query", "limit"] ["query"]
    , tool "list_cells" ["full"] []
    ]

toolCases :: [(T.Text, Value)]
toolCases =
    [ ("insert_cell", object ["source" .= ("x = 1" :: T.Text)])
    ,
        ( "replace_cell_source"
        , object ["cell_id" .= (4 :: Int), "new_source" .= ("x = 2" :: T.Text)]
        )
    , ("discover", object ["query" .= ("map" :: T.Text)])
    , ("try", object ["code" .= ("map (+1) [1,2]" :: T.Text)])
    , ("list_cells", object [])
    ]

garbles :: [T.Text -> T.Text]
garbles =
    [ const "lyx????"
    , const "S..??"
    , (<> "?thinking")
    , T.dropEnd 2
    ]

unsafeBodies :: [LBS.ByteString]
unsafeBodies =
    [ "{not-json"
    , encode (chatCall "insert_cell" (String "{\"source\":"))
    , encode
        ( chatCall
            "insert_cell"
            (object ["arguments" .= object ["source" .= ("x = 1" :: T.Text)]])
        )
    , encode (chatCall "insert_cell" (object ["source" .= ballooned]))
    , encode (channelText "content" "{\"name\":\"insert_cell\",\"arguments\":{")
    ]
  where
    ballooned = T.unlines (replicate 595 "S..?? lyx???? ....????")

unsafeClasses :: [ParseFailureClass]
unsafeClasses =
    [ MalformedResponseJson
    , MalformedToolCall
    , InvalidToolArguments
    , UnsafeCellSource
    ]

tool :: T.Text -> [T.Text] -> [T.Text] -> Value
tool name props required =
    object
        [ "type" .= ("function" :: T.Text)
        , "function"
            .= object
                [ "name" .= name
                , "parameters"
                    .= object
                        [ "type" .= ("object" :: T.Text)
                        , "properties"
                            .= object [K.fromText p .= object ["type" .= argType p] | p <- props]
                        , "required" .= required
                        ]
                ]
        ]
  where
    argType "cell_id" = "integer" :: T.Text
    argType _ = "string"

chatCall :: T.Text -> Value -> Value
chatCall name args =
    object
        [ "message"
            .= object
                [ "role" .= ("assistant" :: T.Text)
                , "content" .= ("" :: T.Text)
                , "tool_calls"
                    .= [object ["function" .= object ["name" .= name, "arguments" .= args]]]
                ]
        ]

channelCall :: T.Text -> T.Text -> Value -> Value
channelCall channel name args = channelText channel encoded
  where
    encoded = TE.decodeUtf8 (LBS.toStrict (encode call))
    call = object ["name" .= name, "arguments" .= args]

channelText :: T.Text -> T.Text -> Value
channelText channel payload =
    object
        [ "message"
            .= object
                [ "role" .= ("assistant" :: T.Text)
                , "content" .= if channel == "content" then payload else ""
                , "thinking" .= if channel == "thinking" then payload else ""
                ]
        ]
