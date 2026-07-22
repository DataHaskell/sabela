{-# LANGUAGE OverloadedStrings #-}

{- | The repair acceptance law over declarations ('Sabela.Parse.Declared'):
a candidate that drops a name the cell declared substituted the deliverable,
not a reference — the run-20260721-005731 topMonth self-heal class.
-}
module Test.DeclaredSpec (spec) where

import qualified Data.Set as S
import Sabela.Parse.Declared (declaredNames, preservesDeclarations)
import Test.Hspec

spec :: Spec
spec = describe "declaredNames / preservesDeclarations (repair acceptance law)" $ do
    it "a signature-only cell still declares its name" $
        declaredNames "topMonth :: String\n"
            `shouldBe` S.fromList ["topMonth"]
    it "declared = defs plus standalone top-level signatures" $
        declaredNames "f :: Int -> Int\nf x = x\ng :: Bool\n"
            `shouldBe` S.fromList ["f", "g"]
    it "rejects a repair that renames the declared binding away" $
        -- The run-20260721-005731 topMonth class: a hole-fit substituted
        -- an in-scope constant for the deliverable's own signature.
        preservesDeclarations
            "topMonth :: String\n"
            "_sabelaWidgetsJs :: String\n"
            `shouldBe` False
    it "accepts a use-site fix that keeps every declared name" $
        preservesDeclarations
            "f :: Int\nf = lengthh [1]"
            "f :: Int\nf = length [1]"
            `shouldBe` True
    it "accepts a line-adding repair (dep/pragma/import)" $
        preservesDeclarations
            "f = maximumBy c xs"
            "import Data.List (maximumBy)\nf = maximumBy c xs"
            `shouldBe` True
    it "rejects a candidate that drops one of several declarations" $
        preservesDeclarations
            "a :: Int\na = 1\nb :: Int\nb = 2"
            "a :: Int\na = 1"
            `shouldBe` False
