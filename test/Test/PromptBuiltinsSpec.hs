{-# LANGUAGE OverloadedStrings #-}

{- | The prompt points at the modules and the search tools; it does NOT
enumerate the drawing vocabulary.

This spec asserted the opposite until 2026-07-25. Naming @plot@,
@displayPicture@, @lineChart@ and @animate@ in the prompt was a deliberate
lever, added when the session index could not surface @Sabela.*@ by keyword
at all (live_test11-14: the model hand-rolled SVG while @plot@ went
unqueried). That is fixed — 'Test.BuiltinSearchLiveSpec' shows
@find_function "chart"@ reaching @lineChart@ and @"Picture"@ reaching
@displayPicture@ against a real kernel — so the enumeration now only teaches
the answer to the very probes that grade this surface. A canary that passes
on prompt recall measures the prompt, not the harness.

The machine-readable 'builtinNames' seed stays: it stops discover DENYING a
builtin (R1.5), which is a different job from telling the model the answer.
-}
module Test.PromptBuiltinsSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.PromptCore (builtinNames, sabelaBuiltins)

-- | The entry points the drawing probes need, which must be FOUND, not read.
drawingEntryPoints :: [T.Text]
drawingEntryPoints = ["displayPicture", "plot", "lineChart", "animate"]

spec :: Spec
spec = describe "the builtins block points, it does not enumerate" $ do
    it "prompt-answer-key: names no drawing entry point" $
        mapM_
            (\n -> (n, T.isInfixOf n sabelaBuiltins) `shouldBe` (n, False))
            drawingEntryPoints

    it "does not hand over the composition idiom either" $ do
        -- live_test20's `group`/`<>`/`mempty` hint was the same lever: it
        -- answers "how do I superimpose two curves" without a search.
        T.isInfixOf "mempty" sabelaBuiltins `shouldBe` False
        T.isInfixOf "<>" sabelaBuiltins `shouldBe` False

    it "still says WHERE they live, so the search has a target" $ do
        T.isInfixOf "Sabela.Notebook" sabelaBuiltins `shouldBe` True
        T.isInfixOf "search" (T.toLower sabelaBuiltins) `shouldBe` True

    it "still says they are internal, so Hackage is not the place to look" $
        T.isInfixOf "internal" (T.toLower sabelaBuiltins) `shouldBe` True

    it "seeds discover with them, so a builtin can never be DENIED" $
        mapM_
            (\n -> (n, n `elem` builtinNames) `shouldBe` (n, True))
            drawingEntryPoints
