{-# LANGUAGE OverloadedStrings #-}

{- | Pure preflight classification: binary-missing vs unreachable vs ready, and
that non-ready states carry an actionable message.
-}
module Test.PreflightSpec (spec) where

import Data.Maybe (isNothing)
import qualified Data.Text as T
import Test.Hspec

import Eval.Preflight (Preflight (..), classifyPreflight, preflightMessage)

spec :: Spec
spec = describe "Ollama preflight classification" $ do
    it "missing binary dominates" $ do
        classifyPreflight False False `shouldBe` MissingBinary
        classifyPreflight False True `shouldBe` MissingBinary
    it "installed but unreachable" $
        classifyPreflight True False `shouldBe` Unreachable
    it "installed and reachable is ready" $
        classifyPreflight True True `shouldBe` Ready

    it "ready has no message" $
        isNothing (preflightMessage Ready "http://localhost:11434") `shouldBe` True
    it "missing-binary message mentions install" $
        fmap (T.isInfixOf "Install") (preflightMessage MissingBinary "x")
            `shouldBe` Just True
    it "unreachable message names the base url" $
        fmap
            (T.isInfixOf "http://host:9999")
            (preflightMessage Unreachable "http://host:9999")
            `shouldBe` Just True
