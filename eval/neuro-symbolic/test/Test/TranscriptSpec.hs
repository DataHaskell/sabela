{-# LANGUAGE OverloadedStrings #-}

module Test.TranscriptSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text, isInfixOf)
import Test.Hspec

import Eval.Transcript (renderMessage, renderTranscript)

-- | A tiny episode: system, user, an assistant tool-call turn, a tool result.
msgs :: [Value]
msgs =
    [ object ["role" .= ("system" :: Text), "content" .= ("You are pairing." :: Text)]
    , object ["role" .= ("user" :: Text), "content" .= ("Define double." :: Text)]
    , object
        [ "role" .= ("assistant" :: Text)
        , "content" .= ("" :: Text)
        , "tool_calls"
            .= [ object
                    [ "function"
                        .= object
                            [ "name" .= ("insert_cell" :: Text)
                            , "arguments" .= object ["source" .= ("double x = 2 * x" :: Text)]
                            ]
                    ]
               ]
        ]
    , object
        [ "role" .= ("tool" :: Text)
        , "tool_name" .= ("insert_cell" :: Text)
        , "content" .= ("{\"ok\":true,\"cellId\":1}" :: Text)
        ]
    ]

spec :: Spec
spec = describe "renderTranscript" $ do
    let out = renderTranscript "double" msgs
    it "titles the session with the task id" $
        ("# Session: double" `isInfixOf` out) `shouldBe` True
    it "numbers and labels each message by role" $ do
        ("## 1. system" `isInfixOf` out) `shouldBe` True
        ("## 2. user" `isInfixOf` out) `shouldBe` True
    it "labels a tool result with its tool name" $
        ("tool (insert_cell)" `isInfixOf` out) `shouldBe` True
    it "shows the assistant's tool call and its arguments" $ do
        ("tool calls" `isInfixOf` out) `shouldBe` True
        ("double x = 2 * x" `isInfixOf` out) `shouldBe` True
    it "carries message content through" $
        ("Define double." `isInfixOf` out) `shouldBe` True
    it "renders the model's thinking when the message carries it" $ do
        let m =
                object
                    [ "role" .= ("assistant" :: Text)
                    , "thinking" .= ("weighing the options" :: Text)
                    , "content" .= ("done" :: Text)
                    ]
        ("*thinking:*" `isInfixOf` renderMessage 5 m) `shouldBe` True
        ("weighing the options" `isInfixOf` renderMessage 5 m) `shouldBe` True
