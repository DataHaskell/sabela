{-# LANGUAGE OverloadedStrings #-}

{- | Context is the scarcest resource a weak model has: everything the loop
injects — errors, grammar, notes — spends it. 'contextChars' measures an
episode's total message content so the spend is REPORTED per task, not guessed.
-}
module Test.ContextCharsSpec (contextCharsSpec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Test.Hspec

import Siza.Agent.Transcript (contextChars)

msgs :: [Value]
msgs =
    [ object ["role" .= ("system" :: Text), "content" .= ("abcde" :: Text)]
    , object ["role" .= ("user" :: Text), "content" .= ("xyz" :: Text)]
    , object ["role" .= ("assistant" :: Text)] -- tool-call turn, no content
    ]

contextCharsSpec :: Spec
contextCharsSpec = describe "contextChars (episode context spend)" $ do
    it "sums each message's content length" $
        contextChars msgs `shouldBe` 8
    it "an empty transcript spends nothing" $
        contextChars [] `shouldBe` 0
