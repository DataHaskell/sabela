{-# LANGUAGE OverloadedStrings #-}

{- | The pure core of the module-not-found repair (B1): fuzzy-match a wrong module
name against the real installed-module list, so @Data.DataFrame@ heals to
@DataFrame@ even when GHC offers no "Perhaps you meant".
-}
module Test.ModuleResolveSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import Sabela.AI.ModuleResolve (closestModules)
import Sabela.AI.Similarity (trigramSimilarity)

installed :: [Text]
installed = ["DataFrame", "DataFrame.Functions", "Data.Text", "Control.Monad"]

spec :: Spec
spec = describe "Sabela.AI.ModuleResolve" $ do
    describe "closestModules" $ do
        it "heals Data.DataFrame to DataFrame (best match first)" $
            take 1 (closestModules 3 0.2 "Data.DataFrame" installed)
                `shouldBe` ["DataFrame"]
        it "returns [] when nothing clears the threshold" $
            closestModules 3 0.2 "Zzzzzz" installed `shouldBe` []
        it "excludes the wrong name itself" $
            closestModules 3 0.0 "DataFrame" installed
                `shouldNotContain` ["DataFrame"]
        it "respects the candidate budget k" $
            length (closestModules 1 0.0 "Data.DataFrame" installed) `shouldBe` 1
        it "ranks the real module above a base-module lookalike" $
            take 1 (closestModules 3 0.0 "Data.DataFrame" ["Data.Data", "DataFrame"])
                `shouldBe` ["DataFrame"]

    describe "trigramSimilarity" $ do
        it "is 1 for identical tokens" $
            trigramSimilarity "DataFrame" "DataFrame" `shouldBe` 1.0
        it "is 0 for disjoint tokens" $
            trigramSimilarity "abcdef" "uvwxyz" `shouldBe` 0.0
        it "ranks the exact submodule prefix above an unrelated module" $
            (trigramSimilarity "Data.DataFrame" "DataFrame"
                > trigramSimilarity "Data.DataFrame" "Control.Monad")
                `shouldBe` True
