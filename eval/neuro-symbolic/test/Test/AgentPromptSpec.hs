{-# LANGUAGE OverloadedStrings #-}

{- | The eval harness prompt shares the product's working-rules core, so the two
experiences stay unified rather than drifting into separate guidance.
-}
module Test.AgentPromptSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Eval.Agent (systemPrompt)
import Sabela.AI.PromptCore (sharedPromptCore)

spec :: Spec
spec = describe "Eval.Agent.systemPrompt (unified)" $
    it "embeds the shared prompt core" $
        (sharedPromptCore `T.isInfixOf` systemPrompt) `shouldBe` True
