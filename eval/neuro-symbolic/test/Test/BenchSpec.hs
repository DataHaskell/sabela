{-# LANGUAGE OverloadedStrings #-}

module Test.BenchSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Eval.Agent (GrammarMode (..))
import Eval.Bench (
    ArmCost (..),
    ArmResult (..),
    Comparison (..),
    RunStat (..),
    armCost,
    byTask,
    costByTask,
    passRate,
    renderComparison,
    renderReport,
    renderReportFull,
    summariseRuns,
    twoProportionZ,
 )

spec :: Spec
spec = describe "Step-5 benchmark stats" $ do
    describe "passRate" $ do
        it "is passes over runs" $
            passRate (ArmResult 3 6) `shouldBe` 0.5
        it "is zero for no runs" $
            passRate (ArmResult 0 0) `shouldBe` 0

    describe "summariseRuns" $
        it "tallies each arm and the B - A difference" $ do
            let cmp =
                    summariseRuns
                        [ (GrammarOff, False)
                        , (GrammarOff, False)
                        , (GrammarOn, True)
                        , (GrammarOn, True)
                        ]
            cmpArmA cmp `shouldBe` ArmResult 0 2
            cmpArmB cmp `shouldBe` ArmResult 2 2
            cmpDiff cmp `shouldBe` 1.0

    describe "twoProportionZ" $ do
        it "is positive when B beats A" $
            (twoProportionZ (ArmResult 1 10) (ArmResult 9 10) > 0) `shouldBe` True
        it "is zero for equal rates" $
            twoProportionZ (ArmResult 5 10) (ArmResult 5 10) `shouldBe` 0
        it "is zero when an arm has no runs" $
            twoProportionZ (ArmResult 0 0) (ArmResult 3 3) `shouldBe` 0

    describe "renderComparison" $
        it "names both arms and the significance verdict" $ do
            let r = renderComparison (summariseRuns [(GrammarOn, True), (GrammarOff, False)])
            ("Arm A" `T.isInfixOf` r) `shouldBe` True
            ("Arm B" `T.isInfixOf` r) `shouldBe` True
            ("significant at 5%" `T.isInfixOf` r) `shouldBe` True

    describe "byTask" $
        it "groups outcomes per task in first-seen order with per-arm tallies" $ do
            let outs =
                    [ ("t1", GrammarOff, False)
                    , ("t1", GrammarOn, True)
                    , ("t2", GrammarOff, True)
                    , ("t2", GrammarOn, True)
                    ]
                bt = byTask outs
            map fst bt `shouldBe` ["t1", "t2"]
            (cmpArmA <$> lookup "t1" bt) `shouldBe` Just (ArmResult 0 1)
            (cmpArmB <$> lookup "t1" bt) `shouldBe` Just (ArmResult 1 1)

    describe "renderReport" $
        it "shows a per-task table and the overall comparison" $ do
            let r = renderReport [("t1", GrammarOn, True), ("t1", GrammarOff, False)]
            ("Per task" `T.isInfixOf` r) `shouldBe` True
            ("t1" `T.isInfixOf` r) `shouldBe` True
            ("Overall" `T.isInfixOf` r) `shouldBe` True

    describe "armCost" $ do
        it "averages tool calls and turns over passing runs only" $ do
            let c = armCost [RunStat True 3 5, RunStat True 1 3, RunStat False 99 99]
            acPassN c `shouldBe` 2
            acMeanCalls c `shouldBe` 4.0
            acMeanTurns c `shouldBe` 2.0
        it "is zero cost when nothing passed" $ do
            let c = armCost [RunStat False 9 9]
            acPassN c `shouldBe` 0
            acMeanCalls c `shouldBe` 0

    describe "costByTask" $
        it "reports each arm's mean cost-to-pass per task" $ do
            let outs =
                    [ ("t1", GrammarOff, RunStat True 8 6)
                    , ("t1", GrammarOn, RunStat True 3 2)
                    ]
                ct = lookup "t1" (costByTask outs)
            (acMeanCalls . fst <$> ct) `shouldBe` Just 6.0
            (acMeanCalls . snd <$> ct) `shouldBe` Just 2.0

    describe "renderReportFull" $
        it "appends the cost-to-pass section to the pass-rate report" $ do
            let r =
                    renderReportFull
                        [("t1", GrammarOn, RunStat True 2 3), ("t1", GrammarOff, RunStat False 9 9)]
            ("Per task" `T.isInfixOf` r) `shouldBe` True
            ("Cost to pass" `T.isInfixOf` r) `shouldBe` True
