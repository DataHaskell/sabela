{-# LANGUAGE OverloadedStrings #-}

{- | Guards for the two silent-corruption repair bugs the gate transcripts
surfaced: hole-fit must not empty a free name to @mempty@/@[]@, and the resolver
must not resolve a name to an example / demo / internal module.
-}
module Test.RepairGuardSpec (spec) where

import Test.Hspec

import Sabela.AI.Capabilities.Edit.HoleSearch (vacuousFit)
import Sabela.AI.HoogleResolve (isNoiseModule)

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
