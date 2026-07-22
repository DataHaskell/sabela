{-# LANGUAGE OverloadedStrings #-}

{- | Report withholding over the measurement substrate (R8.4), scoped to the
reported run: only defects in the REPORTED run-id's transcripts withhold its
numbers; defects in sibling runs sharing the directory are demoted to a named
warning (the run-132942 over-withholding shape). A transcript with no header
cannot be attributed to any run and withholds conservatively.
-}
module Eval.ReportGuard (
    guardReport,
    guardReportRun,
    guardReportDir,
    guardReportDirFor,
    metaProblems,
) where

import Data.List (isSuffixOf, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

import Eval.Episode (EpisodeMeta (..), parseEpisodeMeta)

{- | Withhold a report when ANY transcript given is an unsound measurement:
no config header, missing provenance, a run stamp predating the binary's
build, a missing arm label, or a failed lint. The dir-wide (unscoped) sweep.
-}
guardReport :: [(String, Text)] -> Text -> Text
guardReport files report
    | null problems = report
    | otherwise = withheldBlock problems
  where
    problems = map snd (fileProblems files)

{- | 'guardReport' scoped to one run: the reported run-id's defects (plus any
unattributable header-less transcript) withhold; sibling runs' defects append
a warning instead of suppressing a sound run's numbers.
-}
guardReportRun :: Text -> [(String, Text)] -> Text -> Text
guardReportRun runId files report
    | not (null own) = withheldBlock own
    | null sibling = report
    | otherwise = report <> warningBlock sibling
  where
    (own, sibling) = attribute (fileProblems files)
    attribute ps =
        ( [p | (rid, p) <- ps, maybe True (== runId) rid]
        , [p | (rid, p) <- ps, maybe False (/= runId) rid]
        )

{- | Each defect with the run-id it belongs to ('Nothing' = header-less, so
unattributable), named by file.
-}
fileProblems :: [(String, Text)] -> [(Maybe Text, Text)]
fileProblems files = concatMap check files
  where
    check (name, content) = case parseEpisodeMeta content of
        Nothing ->
            [
                ( Nothing
                , T.pack name <> ": missing episode-config header (arm unrecorded)"
                )
            ]
        Just m ->
            [ (Just (emRunId m), T.pack name <> ": " <> p)
            | p <- metaProblems m
            ]

-- | Every way one parsed header can be unsound; empty means sound.
metaProblems :: EpisodeMeta -> [Text]
metaProblems m =
    [ "missing provenance (commit/run-id/build-time/run-time/endpoint/relink-probe)"
    | any
        T.null
        [ emCommit m
        , emRunId m
        , emBuildTime m
        , emRunTime m
        , emEndpoint m
        , emRelinkProbe m
        ]
    ]
        <> [ "run-time "
                <> emRunTime m
                <> " predates binary build "
                <> emBuildTime m
                <> " (stale transcript)"
           | not (T.null (emRunTime m))
           , not (T.null (emBuildTime m))
           , emRunTime m < emBuildTime m
           ]
        <> ["missing arm label" | T.null (emArm m)]
        <> ["transcript lint " <> emLint m | emLint m /= "ok"]

withheldBlock :: [Text] -> Text
withheldBlock problems =
    T.unlines
        ( "REPORT WITHHELD — measurement substrate unsound (fix and re-run):"
            : map ("  " <>) problems
        )

warningBlock :: [Text] -> Text
warningBlock problems =
    T.unlines
        ( "\nWARNING — sibling run(s) in this directory carry unsound \
          \transcripts (not this run's measurement):"
            : map ("  " <>) problems
        )

-- | The unscoped 'guardReport' over every @.md@ transcript in a directory.
guardReportDir :: FilePath -> Text -> IO Text
guardReportDir dir report = withDirFiles dir report (`guardReport` report)

-- | 'guardReportRun' over every @.md@ transcript in a directory.
guardReportDirFor :: FilePath -> Text -> Text -> IO Text
guardReportDirFor dir runId report =
    withDirFiles dir report (\fs -> guardReportRun runId fs report)

withDirFiles :: FilePath -> Text -> ([(String, Text)] -> Text) -> IO Text
withDirFiles dir fallback k = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure fallback
        else do
            names <- filter (".md" `isSuffixOf`) . sort <$> listDirectory dir
            files <- mapM (\n -> (,) n <$> TIO.readFile (dir </> n)) names
            pure (k files)
