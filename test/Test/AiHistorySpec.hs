{-# LANGUAGE OverloadedStrings #-}

module Test.AiHistorySpec (spec) where

import Data.Aeson (Value, object)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Store (trimHistory)
import Sabela.AI.Types (okOutcome)
import Sabela.Ids (ToolCallId (..))
import Sabela.LLM.Message (
    ContentPart (..),
    Message (..),
    Role (..),
    ToolCall (..),
    ToolResult (..),
 )

userText :: T.Text -> Message
userText s = Message User [TextPart s]

assistantText :: T.Text -> Message
assistantText s = Message Assistant [TextPart s]

assistantToolUse :: T.Text -> Message
assistantToolUse tid =
    Message
        Assistant
        [ TextPart "ok"
        , ToolCallPart (ToolCall (ToolCallId tid) "list_cells" emptyObj)
        ]

userToolResult :: T.Text -> Message
userToolResult tid =
    Message
        User
        [ToolResultPart (ToolResult (ToolCallId tid) "list_cells" (okOutcome emptyObj))]

emptyObj :: Value
emptyObj = object []

spec :: Spec
spec = do
    describe "Sabela.AI.Store.trimHistory" $ do
        it "returns the whole list when under the window" $ do
            let msgs = [userText "hi", assistantText "hey"]
            roles (trimHistory 10 msgs) `shouldBe` roles msgs

        it "drops an orphan tool_result at the head" $ do
            let msgs =
                    [ userToolResult "toolu_dropped"
                    , assistantText "ok"
                    , userText "hi"
                    , assistantText "hey"
                    ]
                kept = trimHistory 10 msgs
            roles kept `shouldBe` [User, Assistant]
            case kept of
                (m : _) -> any isToolResult (msgParts m) `shouldBe` False
                [] -> expectationFailure "kept should be non-empty"

        it "drops an assistant message at the head (first must be user)" $ do
            let msgs =
                    [ assistantText "stranded"
                    , userText "hi"
                    , assistantText "hey"
                    ]
                kept = trimHistory 10 msgs
            case kept of
                (m : _) -> msgRole m `shouldBe` User
                [] -> expectationFailure "kept should be non-empty"

        it "head is always role=user after trimming" $ do
            let long =
                    concat
                        [ [userText "old1", assistantText "r1"]
                        , [userText "old2", assistantText "r2"]
                        , [userText "old3", assistantText "r3"]
                        , [userText "curr", assistantToolUse "toolu_abc", userToolResult "toolu_abc"]
                        ]
            let kept = trimHistory 3 long
            case kept of
                (m : _) -> do
                    msgRole m `shouldBe` User
                    any isToolResult (msgParts m) `shouldBe` False
                [] -> expectationFailure "kept should be non-empty"

        it
            "keeps a legitimate user→assistant(tool_use)→user(tool_result) tail intact when it fits"
            $ do
                let msgs =
                        [ userText "q"
                        , assistantToolUse "toolu_1"
                        , userToolResult "toolu_1"
                        ]
                roles (trimHistory 10 msgs) `shouldBe` [User, Assistant, User]

        it "retains the anchoring user text even when it's older than the window" $ do
            let cycles =
                    concatMap
                        ( \i ->
                            [ assistantToolUse ("toolu_" <> T.pack (show i))
                            , userToolResult ("toolu_" <> T.pack (show i))
                            ]
                        )
                        [1 :: Int .. 8]
                msgs = userText "please visualize primes" : cycles
                kept = trimHistory 3 msgs
            case kept of
                [] -> expectationFailure "kept should never be empty"
                (m : _) -> do
                    msgRole m `shouldBe` User
                    firstTextPart (msgParts m) `shouldBe` Just "please visualize primes"

        it "retains multiple prior turns so demonstratives resolve" $ do
            -- Three turns, each with a tool round (so raw message count far
            -- exceeds the turn window). Keeping 3 turns must retain all three
            -- user prompts — regression for the "every prompt forgets the last"
            -- amnesia where a tool-heavy turn evicted all earlier turns.
            let turn s t =
                    [ userText s
                    , assistantToolUse t
                    , userToolResult t
                    ]
                msgs = turn "first" "t1" ++ turn "second" "t2" ++ turn "third" "t3"
                kept = trimHistory 3 msgs
                prompts = [x | Message User ps <- kept, TextPart x <- ps]
            prompts `shouldBe` ["first", "second", "third"]

        it "keeps only the last N turns when there are more" $ do
            let turn s t = [userText s, assistantToolUse t, userToolResult t]
                msgs = turn "a" "t1" ++ turn "b" "t2" ++ turn "c" "t3"
                prompts = [x | Message User ps <- trimHistory 2 msgs, TextPart x <- ps]
            prompts `shouldBe` ["b", "c"]

        it "never returns an empty list when a user-text anchor exists" $ do
            let msgs =
                    [ userText "hello"
                    , assistantToolUse "toolu_a"
                    , userToolResult "toolu_a"
                    , assistantToolUse "toolu_b"
                    , userToolResult "toolu_b"
                    ]
            length (trimHistory 1 msgs) `shouldSatisfy` (> 0)
  where
    roles = map msgRole
    isToolResult ToolResultPart{} = True
    isToolResult _ = False
    firstTextPart parts = case [t | TextPart t <- parts] of
        (t : _) -> Just t
        [] -> Nothing
