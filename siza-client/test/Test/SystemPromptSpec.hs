{-# LANGUAGE OverloadedStrings #-}

module Test.SystemPromptSpec (systemPromptSpec) where

import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Loop (systemPrompt)

systemPromptSpec :: Spec
systemPromptSpec = describe "systemPrompt" $ do
    it "explains that a write only commits code that compiles" $
        systemPrompt `shouldSatisfy` T.isInfixOf "only commit"

    it "says the compiler's diagnostic comes back on rejection, not a bare failure" $
        systemPrompt `shouldSatisfy` T.isInfixOf "diagnostic"

    it "never names a specific dataset, package, or task from a benchmark scenario" $
        mapM_
            (\leak -> systemPrompt `shouldNotSatisfy` T.isInfixOf leak)
            ["wine", "csv", "summarize", "optics", "vectors"]
