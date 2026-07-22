{-# LANGUAGE OverloadedStrings #-}

{- | The R8.4 general invariant over the GENERATED header-state grid:
'guardReport' withholds the numbers iff any defect is present — missing
provenance, a run stamp predating the binary build (a stale directory
re-graded), a missing arm label, or a lint failure — and names each defect
class. Split from "Test.EpisodeSpec" (module size cap).
-}
module Test.GuardGridSpec (spec) where

import Control.Monad (filterM, forM_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (
    createDirectoryIfMissing,
    getTemporaryDirectory,
    removePathForcibly,
 )
import System.FilePath ((</>))
import Test.Hspec

import Eval.Episode (EpisodeMeta (..), renderEpisodeMeta)
import Eval.ReportGuard (guardReport, guardReportDirFor, guardReportRun)
import Test.EpisodeSpec (sampleMeta)

{- | The defect grid 'guardReport' is generated over: each defect mutates one
provenance/header axis; a subset applies them all.
-}
data HeaderDefect = NoProvenance | StaleRun | NoArm | LintFail
    deriving (Eq, Show)

applyDefect :: EpisodeMeta -> HeaderDefect -> EpisodeMeta
applyDefect m NoProvenance = m{emCommit = ""}
applyDefect m StaleRun =
    m
        { emRunTime = "2026-07-18T09:00:00Z"
        , emBuildTime = "2026-07-19T10:00:00Z"
        }
applyDefect m NoArm = m{emArm = ""}
applyDefect m LintFail = m{emLint = "FAIL raw-exception"}

-- | Every subset of the defect axes, generated mechanically.
defectSubsets :: [[HeaderDefect]]
defectSubsets = filterM (const [False, True]) [NoProvenance, StaleRun, NoArm, LintFail]

report :: Text
report = "Overall: A 3/6 B 4/6"

okFile :: (String, Text)
okFile = ("a-s1-off.md", renderEpisodeMeta sampleMeta <> "body")

spec :: Spec
spec = describe "guardReport over the generated header grid (R8.4)" $ do
    it "withholds iff any defect is present, over every defect subset" $
        forM_ defectSubsets $ \defects -> do
            let m = foldl applyDefect sampleMeta defects
                file = ("g-s1-off.md", renderEpisodeMeta m <> "body")
                out = guardReport [okFile, file] report
            if null defects
                then out `shouldBe` report
                else do
                    out `shouldSatisfy` T.isInfixOf "WITHHELD"
                    out `shouldSatisfy` (not . T.isInfixOf "3/6")
    it "names each defect class in the withheld report" $ do
        let expects =
                [ (NoProvenance, "missing provenance")
                , (StaleRun, "stale transcript")
                , (NoArm, "missing arm label")
                , (LintFail, "transcript lint FAIL")
                ]
        forM_ expects $ \(d, needle) ->
            guardReport
                [("d.md", renderEpisodeMeta (applyDefect sampleMeta d))]
                report
                `shouldSatisfy` T.isInfixOf needle
    it "a pre-provenance header (the stale Jul-18 baseline shape) is unsound" $ do
        let legacy =
                sampleMeta
                    { emRunId = ""
                    , emCommit = ""
                    , emBuildTime = ""
                    , emRunTime = ""
                    , emEndpoint = ""
                    }
            out = guardReport [("old-s1-off.md", renderEpisodeMeta legacy)] report
        out `shouldSatisfy` T.isInfixOf "WITHHELD"
        out `shouldSatisfy` T.isInfixOf "missing provenance"

    describe "guardReportRun (withholding scoped to the reported run-id)" $ do
        let soundId = "run-20260720-222222"
            failId = "run-20260720-111111"
            soundFile =
                ( "s-s1-off.md"
                , renderEpisodeMeta sampleMeta{emRunId = soundId} <> "body"
                )
            failFile =
                ( "barChart-s1-off.md"
                , renderEpisodeMeta
                    sampleMeta{emRunId = failId, emLint = "FAIL empty-final"}
                    <> "body"
                )
        it "releases a sound run's report despite a failing sibling run" $ do
            let out = guardReportRun soundId [soundFile, failFile] report
            out `shouldSatisfy` T.isInfixOf "3/6"
            out `shouldSatisfy` (not . T.isInfixOf "WITHHELD")
        it "demotes the sibling run's defects to a named warning" $ do
            let out = guardReportRun soundId [soundFile, failFile] report
            out `shouldSatisfy` T.isInfixOf "WARNING"
            out `shouldSatisfy` T.isInfixOf "empty-final"
        it "still withholds the failing run's own report" $ do
            let out = guardReportRun failId [soundFile, failFile] report
            out `shouldSatisfy` T.isInfixOf "WITHHELD"
            out `shouldSatisfy` (not . T.isInfixOf "3/6")
        it "attributes a header-less transcript to the reported run (withheld)" $ do
            let out =
                    guardReportRun soundId [soundFile, ("b.md", "# Session: b\n")] report
            out `shouldSatisfy` T.isInfixOf "WITHHELD"
        it "adds no warning when every sibling is sound" $
            guardReportRun soundId [soundFile] report `shouldBe` report

    describe "guardReportDirFor (the run-132942 over-withholding shape)" $
        it "one failing pair no longer suppresses a sound sibling run's report" $ do
            tmp <- getTemporaryDirectory
            let dir = tmp </> "siza-eval-guard-scope"
                soundId = "run-20260720-222222"
                failId = "run-20260720-111111"
            removePathForcibly dir
            createDirectoryIfMissing True dir
            TIO.writeFile
                (dir </> "barChart-s1-off.md")
                ( renderEpisodeMeta
                    sampleMeta{emRunId = failId, emLint = "FAIL empty-final"}
                    <> "body"
                )
            TIO.writeFile
                (dir </> "jsonSum-s1-off.md")
                (renderEpisodeMeta sampleMeta{emRunId = soundId} <> "body")
            released <- guardReportDirFor dir soundId report
            released `shouldSatisfy` T.isInfixOf "3/6"
            released `shouldSatisfy` T.isInfixOf "WARNING"
            withheld <- guardReportDirFor dir failId report
            withheld `shouldSatisfy` T.isInfixOf "WITHHELD"
            withheld `shouldSatisfy` (not . T.isInfixOf "3/6")
