{-# LANGUAGE OverloadedStrings #-}

{- | R6-T4 R8.4 lint extension: a verifier-surface answer (verify channel,
scratchpad result) with no decodable verdict is a lint issue — silence cannot
pass review — while marker-bearing, JSON-field-bearing and transport-classed
answers all decode. Red-then-green fixture: the marker-less pre-R6-T4 verify
message shape FAILS, the current producers' shape passes.
-}
module Test.VerdictLintSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Eval.TranscriptLint (LintIssue (..), lintMessages)
import Siza.Agent.Messages (doneSignalMsg, unconfirmedMsgWith, verifyMsgWith)

toolRes :: Text -> Text -> Value
toolRes n c = object ["role" .= ("tool" :: Text), "tool_name" .= n, "content" .= c]

-- | An assistant turn calling @n@, so the result is not a phantom (P10).
call :: Text -> Value
call n =
    object
        [ "role" .= ("assistant" :: Text)
        , "content" .= ("" :: Text)
        , "tool_calls"
            .= [object ["function" .= object ["name" .= n, "arguments" .= object []]]]
        ]

rules :: [Value] -> [Text]
rules = map liRule . lintMessages

spec :: Spec
spec = describe "verdict lint (R8.4 x section 5.3: verifier answers decode)" $ do
    it "RED fixture: the marker-less legacy verify message is flagged" $
        rules
            [ toolRes
                "verify"
                "The task is not done: the deliverable's check still fails."
            ]
            `shouldBe` ["verifier-no-verdict"]
    it "a scratchpad answer with no verdict field is flagged" $
        rules
            [call "scratchpad", toolRes "scratchpad" "{\"stdout\":\"\",\"stderr\":\"\"}"]
            `shouldContain` ["verifier-no-verdict"]
    it "GREEN: every current verify-channel producer passes" $
        lintMessages
            [doneSignalMsg, verifyMsgWith 0 [] Nothing, unconfirmedMsgWith 1 [] Nothing]
            `shouldBe` []
    it "GREEN: a scratchpad payload carrying the verdict field passes" $
        rules
            [ call "scratchpad"
            , toolRes
                "scratchpad"
                "{\"verdict\":\"could-not-run\",\"stdout\":\"\",\"stderr\":\"\"}"
            ]
            `shouldBe` []
    it "a transport-swallowed verifier answer decodes as infra (not flagged)" $
        rules
            [ call "scratchpad"
            , toolRes
                "scratchpad"
                "[infra] no response within 300s. The server is likely STILL WORKING."
            ]
            `shouldBe` []
    it "non-verifier channels are exempt (no verdict demanded of a write ack)" $
        rules [call "insert_cell", toolRes "insert_cell" "{\"cellId\":1,\"ok\":true}"]
            `shouldBe` []
    it "the empty verifier answer (nycTaxiStats shape) can never pass silently" $
        rules [call "scratchpad", toolRes "scratchpad" ""]
            `shouldSatisfy` elem "verifier-no-verdict"
    it "detail names the offending channel" $ do
        let issues = lintMessages [toolRes "verify" "no marker here"]
        map liDetail issues `shouldSatisfy` any (T.isInfixOf "verify")
