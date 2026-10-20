{-# LANGUAGE OverloadedStrings #-}

module Test.ProviderCheckSpec (providerCheckSpec) where

import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.ProviderCheck (
    Preflight (..),
    classifyModel,
    preflightMessage,
 )

providerCheckSpec :: Spec
providerCheckSpec = describe "Siza.Agent.ProviderCheck model check" $ do
    it "accepts an exact tag" $
        classifyModel "qwen3.5:4b" tags `shouldBe` Ready
    it "accepts a bare name for its :latest tag" $
        classifyModel "qwen3.5-fast" tags `shouldBe` Ready
    it "reports a model the server has not pulled" $
        classifyModel "gpt-oss:20b" tags `shouldBe` MissingModel "gpt-oss:20b" tags
    it "names the pull command and the pulled models in the hint" $
        case preflightMessage (classifyModel "gpt-oss:20b" tags) base of
            Nothing -> expectationFailure "missing model passed preflight"
            Just msg -> do
                msg `shouldSatisfy` T.isInfixOf "ollama pull gpt-oss:20b"
                msg `shouldSatisfy` T.isInfixOf "qwen3.5-fast:latest"
    it "flags an empty store, the symptom of a wrong OLLAMA_MODELS" $
        case preflightMessage (classifyModel "qwen3.5-fast" []) base of
            Just msg -> msg `shouldSatisfy` T.isInfixOf "no models at all"
            Nothing -> expectationFailure "empty store passed preflight"
  where
    tags = ["qwen3.5-fast:latest", "qwen3.5:4b"]
    base = "http://localhost:11434"
