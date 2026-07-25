{-# LANGUAGE OverloadedStrings #-}

{- | The prompt must NAME the drawing vocabulary, not just its modules. Across
live_test11-14 the model used displaySvg (named in the prompt) and hand-rolled
SVG every time, never once querying @plot@ — which was in scope throughout.
-}
module Test.PromptBuiltinsSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.PromptCore (builtinNames, sabelaBuiltins)

spec :: Spec
spec = describe "the builtins block names the drawing entry points" $ do
    it "names the chart and picture entry points the deliverable needs" $
        mapM_
            (\n -> (n, T.isInfixOf n sabelaBuiltins) `shouldBe` (n, True))
            ["displayPicture", "plot", "lineChart", "animate"]

    it "seeds discover with them too, so they can never be denied" $
        mapM_
            (\n -> (n, n `elem` builtinNames) `shouldBe` (n, True))
            ["displayPicture", "plot", "lineChart", "animate"]

    -- live_test20: asked to superimpose a cosine, the model searched
    -- `pictures` and `overlay`, found neither, and guessed the gloss package.
    -- `group` and the Semigroup instance were both in scope the whole time.
    it "names how pictures COMBINE, not only how they are built" $ do
        T.isInfixOf "group" sabelaBuiltins `shouldBe` True
        "group" `elem` builtinNames `shouldBe` True

    it "states the composition operator, the vocabulary a search cannot guess" $
        T.isInfixOf "<>" sabelaBuiltins `shouldBe` True
