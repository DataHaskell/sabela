{-# LANGUAGE OverloadedStrings #-}

module Test.PromptBuiltinsSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.PromptCore (builtinNames, sabelaBuiltins)

drawingEntryPoints :: [T.Text]
drawingEntryPoints = ["displayPicture", "plot", "lineChart", "animate"]

spec :: Spec
spec = describe "the builtins block points, it does not enumerate" $ do
    it "prompt-answer-key: names no drawing entry point" $
        mapM_
            (\n -> (n, T.isInfixOf n sabelaBuiltins) `shouldBe` (n, False))
            drawingEntryPoints

    it "does not hand over the composition idiom either" $ do
        T.isInfixOf "mempty" sabelaBuiltins `shouldBe` False
        T.isInfixOf "<>" sabelaBuiltins `shouldBe` False

    it "hands over the canonical wholesale import line (live_gemma2)" $ do
        T.isInfixOf "import Sabela.Notebook" sabelaBuiltins `shouldBe` True
        T.isInfixOf "selective" (T.toLower sabelaBuiltins) `shouldBe` True

    it "still says WHERE they live, so the search has a target" $ do
        T.isInfixOf "Sabela.Notebook" sabelaBuiltins `shouldBe` True
        T.isInfixOf "search" (T.toLower sabelaBuiltins) `shouldBe` True

    it "still says they are internal, so Hackage is not the place to look" $
        T.isInfixOf "internal" (T.toLower sabelaBuiltins) `shouldBe` True

    it "seeds discover with them, so a builtin can never be DENIED" $
        mapM_
            (\n -> (n, n `elem` builtinNames) `shouldBe` (n, True))
            drawingEntryPoints
