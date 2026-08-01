{-# LANGUAGE OverloadedStrings #-}

{- | What a disposable materialization produced, and whose code a failure
belongs to. One classifier, shared by every payload builder, so the gate and
`try` cannot drift on the question of what was actually checked.
-}
module Sabela.Session.MaterializeStage (
    DisposableVerdict (..),
    MaterializeStage (..),
    MaterializeFailure (..),
    SkippedCell (..),
    DisposableResult (..),
    materializeStageText,
    materializeStages,
    stageReachedCandidate,
    stageNamesCell,
    stageUsedCandidateMetadata,
    stageFailure,
    reachedCandidate,
    Attribution (..),
    attributionOf,
    attributionText,
    blamedCell,
    blockedRemedy,
    failureFor,
    resultVerdictClass,
) where

import Data.List (uncons)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Verdict (VerdictClass (..))

data DisposableVerdict
    = DisposableOk
    | DisposableCompileError
    | DisposableRuntimeError
    | DisposableTimedOut
    | DisposableUnavailable
    deriving (Eq, Show)

data MaterializeStage
    = StagePlan
    | StageSnapshot
    | StageProject
    | StageSession
    | StageCompiled
    | StagePrelude
    | StageCellReplay
    | StageSafety
    | StageCandidateSetup
    | StageCandidateTypecheck
    | StageCandidateRun
    deriving (Bounded, Enum, Eq, Show)

data MaterializeFailure = MaterializeFailure
    { failureStage :: MaterializeStage
    , failureCellId :: Maybe Int
    , failureMessage :: Text
    }
    deriving (Eq, Show)

data SkippedCell = SkippedCell
    { skippedCellId :: Int
    , skippedReason :: Text
    }
    deriving (Eq, Show)

data DisposableResult = DisposableResult
    { disposableRoute :: Text
    , disposableVerdict :: DisposableVerdict
    , disposableType :: Maybe Text
    , disposableStdout :: Text
    , disposableStderr :: Text
    , disposableFailure :: Maybe MaterializeFailure
    , disposableReplayedCells :: [Int]
    , disposableSkippedCells :: [SkippedCell]
    , disposableDependencies :: [Text]
    }
    deriving (Eq, Show)

materializeStageText :: MaterializeStage -> Text
materializeStageText stage = case stage of
    StagePlan -> "plan"
    StageProject -> "project"
    StageSession -> "session"
    StageCompiled -> "compiled"
    StagePrelude -> "prelude"
    StageCellReplay -> "cell_replay"
    StageSnapshot -> "snapshot"
    StageSafety -> "safety"
    StageCandidateSetup -> "candidate_setup"
    StageCandidateTypecheck -> "candidate_typecheck"
    StageCandidateRun -> "candidate_run"

materializeStages :: [MaterializeStage]
materializeStages = [minBound .. maxBound]

{- | Whether the candidate's own source was ever put to the compiler. Every
earlier stage is the harness rebuilding the context around it, so a failure
there says nothing about the candidate.
-}
stageReachedCandidate :: MaterializeStage -> Bool
stageReachedCandidate stage = case stage of
    StageCandidateSetup -> True
    StageCandidateTypecheck -> True
    StageCandidateRun -> True
    _ -> False

{- | Whether the stage ran one notebook cell's own source. Only such a stage
can name the cell that failed; everywhere else a cell id is context the
harness derived, not a cell the stage put to the compiler.
-}
stageNamesCell :: MaterializeStage -> Bool
stageNamesCell stage = case stage of
    StagePlan -> True
    StageCellReplay -> True
    _ -> False

{- | Whether the stage's input included the candidate's own dependency
metadata. The disposable project is built from the notebook's metadata merged
with the candidate's, and one session runs every later stage inside it, so
only planning and the snapshot check are beyond the candidate's reach.
-}
stageUsedCandidateMetadata :: MaterializeStage -> Bool
stageUsedCandidateMetadata stage = case stage of
    StagePlan -> False
    StageSnapshot -> False
    _ -> True

{- | The failure a stage records. The blame slot is reserved for a cell the
stage itself ran; a cell id offered by any other stage is dropped, because
'blamedCell' is where derived context belongs and the two must not be read
as the same claim.
-}
stageFailure :: MaterializeStage -> Maybe Int -> Text -> MaterializeFailure
stageFailure stage cid =
    MaterializeFailure stage (if stageNamesCell stage then cid else Nothing)

{- | An absent failure means no stage was recorded, which is not evidence the
candidate compiled; only a recorded candidate stage is.
-}
reachedCandidate :: DisposableResult -> Bool
reachedCandidate =
    maybe False (stageReachedCandidate . failureStage) . disposableFailure

{- | The failure a finished stage records. A green run has none: whatever it
wrote to stderr is GHC talking about code that compiled. A run that was not
green records the stage it reached whether or not GHC wrote anything, so a
silent rejection is never read back as the candidate having gone uncompiled.
-}
failureFor ::
    DisposableVerdict -> MaterializeStage -> Text -> Maybe MaterializeFailure
failureFor verdict stage err
    | verdict == DisposableOk = Nothing
    | otherwise = Just (stageFailure stage Nothing err)

{- | The class a trial's result belongs to. One reading, so `try` and the gate
cannot answer differently about the same record: a diagnostic requires a
candidate the compiler read, and a trial that never got that far reports the
absence rather than a verdict.
-}
resultVerdictClass :: DisposableResult -> VerdictClass
resultVerdictClass result = case disposableVerdict result of
    DisposableOk -> VerdictOk
    DisposableTimedOut -> VerdictInfra
    DisposableUnavailable -> VerdictInfra
    _
        | reachedCandidate result -> VerdictDiagnostic
        | otherwise -> VerdictCouldNotRun

-- | Whose code a diagnostic describes.
data Attribution
    = -- | The source the caller submitted.
      AttributedCandidate
    | -- | A committed notebook cell the stage itself named.
      AttributedCell Int
    | -- | Harness scaffolding, run after this cell — the last one that ran.
      AttributedAfterCell Int
    | -- | Harness scaffolding, with no cell behind it.
      AttributedHarness
    deriving (Eq, Show)

{- | Whose code was running. A stage that named a cell is that cell's; a stage
that named none was the harness's own, and the most the record supports is
which cell ran last before it.
-}
attributionOf :: DisposableResult -> Attribution
attributionOf result
    | reachedCandidate result = AttributedCandidate
    | Just cid <- disposableFailure result >>= failureCellId = AttributedCell cid
    | otherwise =
        maybe AttributedHarness AttributedAfterCell (blamedCell result)

attributionText :: Attribution -> Text
attributionText AttributedCandidate = "your candidate"
attributionText (AttributedCell cid) = "notebook cell " <> T.pack (show cid)
attributionText (AttributedAfterCell cid) =
    "the harness's own setup code, run after notebook cell "
        <> T.pack (show cid)
        <> " (the last cell that ran)"
attributionText AttributedHarness = "the harness's own setup code"

{- | What the caller can do about a failure at a stage its candidate never
reached. Only a stage none of the candidate's input reached may be called
unclearable; elsewhere the metadata it contributed is a lever it has.
-}
blockedRemedy :: MaterializeStage -> Attribution -> Text
blockedRemedy stage attribution = case attribution of
    AttributedCandidate -> "Retry, or state the blocker."
    AttributedCell cid ->
        "Fix or remove cell "
            <> tShow cid
            <> " (or skip it by making it compile), then retry."
    AttributedAfterCell cid ->
        "Cell "
            <> tShow cid
            <> " is the last that ran before it; start with what that cell \
               \brings into scope, then retry."
    AttributedHarness
        | stageUsedCandidateMetadata stage -> metadataRemedy
        | otherwise ->
            "Nothing you write in the candidate can clear it; retry, or say so."
  where
    metadataRemedy =
        "This stage used the notebook's dependency metadata merged with any \
        \dependencies your candidate declares, so a package your candidate \
        \names can break it. Retry, or state the blocker."

tShow :: Int -> Text
tShow = T.pack . show

{- | The notebook cell a non-candidate failure belongs to: the one the stage
named, else the last cell that actually ran before the failure.
-}
blamedCell :: DisposableResult -> Maybe Int
blamedCell result
    | reachedCandidate result = Nothing
    | otherwise = case disposableFailure result >>= failureCellId of
        Just cid -> Just cid
        Nothing -> lastOf (disposableReplayedCells result)
  where
    lastOf = fmap fst . uncons . reverse
