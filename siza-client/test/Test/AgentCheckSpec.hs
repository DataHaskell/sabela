{-# LANGUAGE OverloadedStrings #-}

module Test.AgentCheckSpec (agentCheckSpec) where

import Siza.Agent.Check (
    CheckResult (..),
    classifyCheck,
    extractTestExpr,
    interpretConfirm,
 )
import Test.Hspec

agentCheckSpec :: Spec
agentCheckSpec = describe "siza chat covering-check helpers" $ do
    describe "extractTestExpr" $ do
        it "takes a bare expression as-is" $
            extractTestExpr "total == 600" `shouldBe` "total == 600"

        it "unwraps a fenced block" $
            extractTestExpr "```haskell\ntotal == 600\n```" `shouldBe` "total == 600"

        it "pulls an inline-backticked expression out of prose" $
            extractTestExpr "The check is `length xs == 3`." `shouldBe` "length xs == 3"

        it "takes the first non-empty line when the reply has blanks" $
            extractTestExpr "\n\nrevenueTotal == 600\n" `shouldBe` "revenueTotal == 600"

    describe "interpretConfirm" $ do
        it "accepts the proposal on a blank line" $
            interpretConfirm "x == 1" "" `shouldBe` "x == 1"

        it "accepts the proposal on yes" $
            interpretConfirm "x == 1" "y" `shouldBe` "x == 1"

        it "declines a check on skip (empty result)" $
            interpretConfirm "x == 1" "skip" `shouldBe` ""

        it "treats a boolean expression as an edited test" $
            interpretConfirm "x == 1" "y == 2" `shouldBe` "y == 2"

        it "treats prose feedback as no check, not a broken test" $
            interpretConfirm
                "x == 1"
                "This is not markdown and it doesn't print the dataframe."
                `shouldBe` ""

    describe "classifyCheck (covering-check marker output)" $ do
        it "treats GRADE_PASS output as a passed check" $
            classifyCheck "GRADE_PASS\n" `shouldBe` CheckPassed

        it "treats GRADE_FAIL output as a failed check (a wrong answer)" $
            classifyCheck "GRADE_FAIL\n" `shouldBe` CheckFailed

        it "treats neither token (a non-compiling check) as uncheckable" $
            classifyCheck "<interactive>:5:1: error: No instance for (Foldable ByteString)"
                `shouldBe` CheckUncheckable
