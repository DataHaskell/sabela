{-# LANGUAGE OverloadedStrings #-}

module Test.VerdictLintSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Eval.TranscriptLint (LintIssue (..), lintMessages)
import Siza.Agent.Messages (doneSignalMsg, unconfirmedMsgWith, verifyMsgWith)

toolRes :: Text -> Text -> Value
toolRes n c = object ["role" .= ("tool" :: Text), "tool_name" .= n, "content" .= c]

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
    it "a try answer with no verdict field is flagged" $
        rules
            [call "try", toolRes "try" "{\"stdout\":\"\",\"stderr\":\"\"}"]
            `shouldContain` ["verifier-no-verdict"]
    it "GREEN: every current verify-channel producer passes" $
        lintMessages
            [ doneSignalMsg [1] "x == 1"
            , verifyMsgWith 0 [] Nothing
            , unconfirmedMsgWith 1 [] Nothing
            ]
            `shouldBe` []
    it "GREEN: a try payload carrying the verdict field passes" $
        rules
            [ call "try"
            , toolRes
                "try"
                "{\"verdict\":\"could-not-run\",\"stdout\":\"\",\"stderr\":\"\"}"
            ]
            `shouldBe` []
    it "a transport-swallowed verifier answer decodes as infra (not flagged)" $
        rules
            [ call "try"
            , toolRes
                "try"
                "[infra] no response within 300s. The server is likely STILL WORKING."
            ]
            `shouldBe` []
    it "non-verifier channels are exempt (no verdict demanded of a write ack)" $
        rules [call "insert_cell", toolRes "insert_cell" "{\"cellId\":1,\"ok\":true}"]
            `shouldBe` []
    it "the empty verifier answer (nycTaxiStats shape) can never pass silently" $
        rules [call "try", toolRes "try" ""]
            `shouldSatisfy` elem "verifier-no-verdict"
    it "detail names the offending channel" $ do
        let issues = lintMessages [toolRes "verify" "no marker here"]
        map liDetail issues `shouldSatisfy` any (T.isInfixOf "verify")
