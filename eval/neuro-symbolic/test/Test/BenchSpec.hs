{-# LANGUAGE OverloadedStrings #-}

module Test.BenchSpec (spec) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import Test.Hspec

import Eval.Agent (AgentRun (..), GrammarMode (..), defaultBudget)
import Eval.Bench (
    ArmCost (..),
    ArmResult (..),
    BenchConfig (..),
    Comparison (..),
    RunStat (..),
    armCost,
    byTask,
    costByTask,
    passRate,
    renderComparison,
    renderReport,
    renderReportFlagged,
    renderReportFull,
    saveTranscript,
    summariseRuns,
    twoProportionZ,
    waitHealthN,
 )
import Eval.Episode (EpisodeMeta (..), parseEpisodeMeta)
import Eval.Provenance (RunProvenance (..))
import Eval.Task (Grader (..), Task (..))
import Siza.Transport (newConn)

spec :: Spec
spec = describe "Step-5 benchmark stats" $ do
    describe "saveTranscript (R8.1/R8.3 through the real path)" $
        it "stamps provenance, endpoint, arm, levers, seed and stop reason" $ do
            mgr <- newManager defaultManagerSettings
            conn <- newConn
            tmp <- getTemporaryDirectory
            let dir = tmp </> "siza-eval-bench-save"
            removePathForcibly dir
            let prov =
                    RunProvenance
                        "run-20260719-1"
                        "cafe123"
                        "2026-07-19T00:00:00Z"
                        "ok: binary fresh"
                cfg =
                    BenchConfig mgr conn "test-model" defaultBudget 12 "bin" 3100 dir prov
                run =
                    AgentRun
                        { arTurns = 3
                        , arToolCalls = 2
                        , arFinal = "final answer"
                        , arStopped = "done"
                        , arTranscript =
                            [ object
                                ["role" .= ("user" :: Text), "content" .= ("hi" :: Text)]
                            ]
                        }
                task = Task "fixture" "prompt" (ByValue "True")
            saveTranscript cfg "http://localhost:3999" task 1 GrammarOff run
            txt <- T.pack <$> readFile (dir </> "fixture-s1-off.md")
            let m = parseEpisodeMeta txt
            fmap emRunId m `shouldBe` Just "run-20260719-1"
            fmap emCommit m `shouldBe` Just "cafe123"
            fmap emBuildTime m `shouldBe` Just "2026-07-19T00:00:00Z"
            fmap emEndpoint m `shouldBe` Just "http://localhost:3999"
            fmap emStopped m `shouldBe` Just "done"
            fmap emFinal m `shouldBe` Just "final answer"
            fmap emArm m `shouldBe` Just "off"
            fmap emLevers m `shouldBe` Just [("grammar", "off")]
            fmap emSeed m `shouldBe` Just 1
            fmap (T.null . emRunTime) m `shouldBe` Just False
            fmap emRelinkProbe m `shouldBe` Just "ok: binary fresh"

    describe "saveTranscript wires stopIssues into the lint header (R8.4)" $
        it "a max_turns run with an empty final saves lint: FAIL empty-final" $ do
            mgr <- newManager defaultManagerSettings
            conn <- newConn
            tmp <- getTemporaryDirectory
            let dir = tmp </> "siza-eval-bench-save-emptyfinal"
            removePathForcibly dir
            let prov =
                    RunProvenance
                        "run-20260720-2"
                        "cafe123"
                        "2026-07-20T00:00:00Z"
                        "ok: binary fresh"
                cfg =
                    BenchConfig mgr conn "test-model" defaultBudget 12 "bin" 3100 dir prov
                run =
                    AgentRun
                        { arTurns = 12
                        , arToolCalls = 9
                        , arFinal = ""
                        , arStopped = "max_turns"
                        , arTranscript =
                            [ object
                                ["role" .= ("user" :: Text), "content" .= ("hi" :: Text)]
                            ]
                        }
                task = Task "fixture" "prompt" (ByValue "True")
            saveTranscript cfg "http://localhost:3999" task 1 GrammarOff run
            txt <- T.pack <$> readFile (dir </> "fixture-s1-off.md")
            fmap emLint (parseEpisodeMeta txt) `shouldBe` Just "FAIL empty-final"

    describe "waitHealthN (M8: no episode against a dead endpoint)" $
        it "an unreachable endpoint yields Left naming the base, never Right" $ do
            conn <- newConn
            r <- waitHealthN 1 conn "http://localhost:39999"
            case r of
                Right () -> expectationFailure "dead endpoint reported healthy"
                Left e -> do
                    e `shouldSatisfy` T.isInfixOf "http://localhost:39999"
                    e `shouldSatisfy` T.isInfixOf "aborting the episode"
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

    describe "renderReportFlagged (R8.2: NA/saturated graded, never measured)" $ do
        let row t s m ok = (t, s, m, RunStat ok 2 3)
            rows =
                [ row "soundT" 3 GrammarOff True
                , row "soundT" 3 GrammarOn False
                , row "naT" 1 GrammarOff True
                , row "naT" 1 GrammarOn True
                , row "voidT" 2 GrammarOff False
                , row "voidT" 2 GrammarOn True
                , row "satT" 4 GrammarOff True
                , row "satT" 4 GrammarOn True
                ]
            r =
                renderReportFlagged
                    [("voidT", 2)]
                    [("naT", 1)]
                    [("satT", 4)]
                    rows
        it "prints BOTH per-arm outcomes for the NA pair in the Per task table" $ do
            let naLines = filter ("naT" `T.isInfixOf`) (T.lines r)
            length naLines `shouldBe` 1
            head naLines `shouldSatisfy` T.isInfixOf "A pass"
            head naLines `shouldSatisfy` T.isInfixOf "B pass"
            head naLines `shouldSatisfy` T.isInfixOf "not applicable"
        it "prints the saturated pair per-arm with the lever-fired label" $ do
            let satLines = filter ("satT" `T.isInfixOf`) (T.lines r)
            length satLines `shouldBe` 1
            head satLines `shouldSatisfy` T.isInfixOf "A pass"
            head satLines `shouldSatisfy` T.isInfixOf "B pass"
            head satLines `shouldSatisfy` T.isInfixOf "lever-saturated"
            head satLines `shouldSatisfy` T.isInfixOf "lever fired"
        it "excludes NA and saturated pairs from the lever numbers" $ do
            r `shouldSatisfy` T.isInfixOf "Arm A (GrammarOff): 1/1"
            r `shouldSatisfy` T.isInfixOf "Arm B (GrammarOn):  0/1"
        it "a VOID pair appears in no row at all" $
            filter ("voidT" `T.isInfixOf`) (T.lines r) `shouldBe` []
        it "grid law: flagged pairs are printed per-arm, never measured" $
            sequence_
                [ do
                    let flaggedNa = [("naT", 1) | naFlag]
                        flaggedVoid = [("voidT", 2) | voidFlag]
                        flaggedSat = [("satT", 4) | satFlag]
                        out = renderReportFlagged flaggedVoid flaggedNa flaggedSat rows
                        perTask = takeWhile (/= "Overall:") (T.lines out)
                        naLines = filter ("naT" `T.isInfixOf`) perTask
                        satLines = filter ("satT" `T.isInfixOf`) perTask
                    length naLines `shouldBe` 1
                    length satLines `shouldBe` 1
                    if naFlag
                        then do
                            head naLines `shouldSatisfy` T.isInfixOf "A pass"
                            head naLines `shouldSatisfy` T.isInfixOf "B pass"
                        else head naLines `shouldSatisfy` T.isInfixOf "1/1"
                    if satFlag
                        then head satLines `shouldSatisfy` T.isInfixOf "lever-saturated"
                        else head satLines `shouldSatisfy` T.isInfixOf "1/1"
                    ( "Arm A (GrammarOff): "
                            <> armA naFlag voidFlag satFlag
                        )
                        `T.isInfixOf` out
                        `shouldBe` True
                | naFlag <- [False, True]
                , voidFlag <- [False, True]
                , satFlag <- [False, True]
                ]
  where
    -- Sound-row A tallies for the grid: soundT always measured (pass);
    -- each of naT/satT adds a pass when unflagged; voidT a fail when unflagged.
    armA naFlag voidFlag satFlag =
        tshow passes <> "/" <> tshow runs
      where
        passes = 1 + fromEnum (not naFlag) + fromEnum (not satFlag)
        runs = passes + fromEnum (not voidFlag)
    tshow :: Int -> Text
    tshow = T.pack . show
