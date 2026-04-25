{-# LANGUAGE OverloadedStrings #-}

module Test.AiHistorySpec (spec) where

import Data.Aeson (Value, object)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Store (trimHistory)
import Sabela.Anthropic.Types (
    ContentBlock (..),
    Message (..),
    Role (..),
 )

userText :: T.Text -> Message
userText s = Message RoleUser [TextBlock s]

assistantText :: T.Text -> Message
assistantText s = Message RoleAssistant [TextBlock s]

assistantToolUse :: T.Text -> Message
assistantToolUse tid =
    Message
        RoleAssistant
        [ TextBlock "ok"
        , ToolUseBlock tid "list_cells" emptyObj
        ]

userToolResult :: T.Text -> Message
userToolResult tid =
    Message
        RoleUser
        [ToolResultBlock tid False [TextBlock "{}"]]

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
            roles kept `shouldBe` [RoleUser, RoleAssistant]
            -- head must not carry a tool_result block
            case kept of
                (m : _) -> any isToolResult (msgContent m) `shouldBe` False
                [] -> expectationFailure "kept should be non-empty"

        it "drops an assistant message at the head (first must be user)" $ do
            let msgs =
                    [ assistantText "stranded"
                    , userText "hi"
                    , assistantText "hey"
                    ]
                kept = trimHistory 10 msgs
            case kept of
                (m : _) -> msgRole m `shouldBe` RoleUser
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
                    msgRole m `shouldBe` RoleUser
                    any isToolResult (msgContent m) `shouldBe` False
                [] -> expectationFailure "kept should be non-empty"

        it
            "keeps a legitimate user→assistant(tool_use)→user(tool_result) tail intact when it fits"
            $ do
                let msgs =
                        [ userText "q"
                        , assistantToolUse "toolu_1"
                        , userToolResult "toolu_1"
                        ]
                roles (trimHistory 10 msgs) `shouldBe` [RoleUser, RoleAssistant, RoleUser]

        it "retains the anchoring user text even when it's older than the window" $ do
            -- Mid-turn: many tool cycles after a single user prompt. Even with
            -- a small window, the original user-text anchor must remain so the
            -- request is never empty.
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
                    msgRole m `shouldBe` RoleUser
                    firstTextBlock (msgContent m) `shouldBe` Just "please visualize primes"

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
    isToolResult ToolResultBlock{} = True
    isToolResult _ = False
    firstTextBlock blocks = case [t | TextBlock t <- blocks] of
        (t : _) -> Just t
        [] -> Nothing
