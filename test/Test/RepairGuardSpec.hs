{-# LANGUAGE OverloadedStrings #-}

module Test.RepairGuardSpec (spec) where

import Test.Hspec

import Sabela.AI.Capabilities.Edit.HoleSearch (vacuousFit)
import Sabela.AI.HoogleResolve (isNoiseModule)
import Sabela.AI.ModuleResolve (isOutOfScopePackage)

spec :: Spec
spec = describe "repair correctness guards" $ do
    describe "vacuousFit (hole-fit filler blocklist)" $ do
        it "rejects semantically-empty fills" $
            all vacuousFit ["mempty", "undefined", "[]", "Nothing", "mzero", "empty"]
                `shouldBe` True
        it "keeps real names a typo could heal to" $
            any vacuousFit ["customers", "levenshtein", "divvy", "makeExprParser"]
                `shouldBe` False

    describe "isNoiseModule (resolver / ranker drop-list)" $ do
        it "drops example / demo / tutorial / internal / doc modules" $
            all
                isNoiseModule
                [ "Numeric.Probability.Example.Barber"
                , "Documentation.SBV.Examples.TP.Peano"
                , "Data.Vector.Internal"
                , "Foo.Demo.Bar"
                , "Foo.Tutorial"
                ]
                `shouldBe` True
        it "keeps ordinary library modules" $
            any
                isNoiseModule
                ["Data.List.Split", "DataFrame", "Data.Text.Metrics", "Granite.Svg"]
                `shouldBe` False

    describe "isOutOfScopePackage (G2 scope guard)" $ do
        it "excludes the compiler's own toolchain packages" $
            all
                isOutOfScopePackage
                ["ghc", "ghc-boot", "ghc-boot-th", "ghci", "ghc-heap", "ghc-prim"]
                `shouldBe` True
        it "keeps ordinary library packages, including GHC.*-namespaced ones from base" $
            any
                isOutOfScopePackage
                ["base", "containers", "text", "dataframe", "sabela-notebook"]
                `shouldBe` False
