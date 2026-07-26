{-# LANGUAGE OverloadedStrings #-}

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

guardReport :: [(String, Text)] -> Text -> Text
guardReport files report
    | null problems = report
    | otherwise = withheldBlock problems
  where
    problems = map snd (fileProblems files)

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

fileProblems :: [(String, Text)] -> [(Maybe Text, Text)]
fileProblems = concatMap check
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

guardReportDir :: FilePath -> Text -> IO Text
guardReportDir dir report = withDirFiles dir report (`guardReport` report)

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
