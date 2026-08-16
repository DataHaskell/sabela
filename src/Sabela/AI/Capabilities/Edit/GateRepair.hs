{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.GateRepair (
    gatedCandidate,
    repairArmed,
    acceptGreen,
    frontierRejectionJson,
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.Admission (
    Admission (..),
    admitted,
    withRepairs,
 )
import Sabela.AI.Capabilities.Edit.CompileGate (
    GateSource (..),
    compileGateSpec,
    gateDefaultingRejection,
 )
import Sabela.AI.Capabilities.Edit.GateFrontier (
    Frontier (..),
    Step (..),
    disposableDiagnostic,
    probeBudget,
    repairRounds,
    startFrontier,
    stepFrontier,
 )
import Sabela.AI.Capabilities.Edit.GateRepair.Candidates (gateCandidates)
import Sabela.AI.Capabilities.Edit.GateRepair.Reject (
    frontierRejection,
    frontierRejectionJson,
    frontierSource,
 )
import Sabela.AI.Capabilities.Edit.Submission (
    Submission,
    compiledText,
    submittedText,
 )
import Sabela.AI.Capabilities.Try.Payload.Checked (checkedNotes)
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.PathGate (pathGateCheck)
import Sabela.AI.WriteAck (refusalAck)
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.Session.Materialize (CandidateSpec, runDisposableTry)
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    DisposableVerdict (..),
    reachedCandidate,
 )
import Sabela.SessionTypes (CellLang (..))
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (readNotebook)

import Data.Aeson (Value)

{- | The gate stack a programmatic write passes before it may commit:
compile first (with repair), then the paths the surviving source names.
-}
gatedCandidate ::
    App ->
    Maybe Int ->
    CellLang ->
    CellType ->
    Submission ->
    IO (Either Value Admission)
gatedCandidate app mReplaces lang ty sub
    | ty /= CodeCell || lang /= Haskell = pure (Right (admitted (compiledText sub)))
    | otherwise = do
        compiled <- compileGatedCandidate app mReplaces sub
        case compiled of
            Left rejection -> pure (Left rejection)
            Right admission ->
                keepingClaims admission <$> pathGated app mReplaces admission

{- | The path gate may repair the source under it, so its own admission is the
one that ships — carrying forward what the compile gate had already settled.
-}
keepingClaims :: Admission -> Either Value Admission -> Either Value Admission
keepingClaims earlier =
    fmap
        ( \later ->
            (withRepairs (admittedRepairs earlier) later)
                { admittedChecked = admittedChecked earlier <> admittedChecked later
                }
        )

{- | Rejects a write whose paths are not there, and silently corrects one
whose only fault is the directory. Runs on compile-green source so the
compiler's diagnostic always wins the turn when there is one.
-}
pathGated :: App -> Maybe Int -> Admission -> IO (Either Value Admission)
pathGated app mReplaces admission = do
    others <- otherCellSources app mReplaces
    result <-
        pathGateCheck (envWorkDir (appEnv app)) others (admittedSource admission)
    pure
        ( either
            (Left . refusalAck "path-gate" mReplaces)
            (Right . uncurry repaired)
            result
        )
  where
    repaired src notes = withRepairs notes (admitted src)

otherCellSources :: App -> Maybe Int -> IO [Text]
otherCellSources app mReplaces = do
    nb <- readNotebook (appNotebook app)
    pure [cellSource c | c <- nbCells nb, Just (cellId c) /= mReplaces]

compileGatedCandidate ::
    App ->
    Maybe Int ->
    Submission ->
    IO (Either Value Admission)
compileGatedCandidate app mReplaces sub = do
    result <- runDisposableTry app spec
    case disposableVerdict result of
        DisposableOk ->
            pure (acceptGreen spec mReplaces [] (submissionSource sub) result)
        verdict -> do
            enabled <- featureEnabled "SABELA_GATE_REPAIR"
            let start = startFrontier (submittedText sub) src result
            searched <-
                if repairArmed enabled verdict result
                    then searchRepair app mReplaces start
                    else pure (Left start)
            case searched of
                Right (frontier, ranSpec) ->
                    pure
                        ( acceptGreen
                            ranSpec
                            mReplaces
                            [disclosure frontier]
                            (frontierSource frontier)
                            (frontierResult frontier)
                        )
                Left frontier -> Left <$> frontierRejection app mReplaces frontier
  where
    src = compiledText sub
    spec = compileGateSpec mReplaces src

-- | What the caller submitted against what the gate put to the compiler.
submissionSource :: Submission -> GateSource
submissionSource sub =
    GateSource
        { gateSubmitted = submittedText sub
        , gateCompiled = compiledText sub
        }

{- | A green candidate is admitted only once the defaulting check clears it.
The property the gate proved is read off the spec the trial was run against,
never off one rebuilt from the admitted text.
-}
acceptGreen ::
    CandidateSpec ->
    Maybe Int ->
    [Text] ->
    GateSource ->
    DisposableResult ->
    Either Value Admission
acceptGreen ranSpec mReplaces notes gsrc result =
    maybe
        (Right admission)
        Left
        (gateDefaultingRejection mReplaces notes gsrc result)
  where
    admission =
        Admission
            { admittedSource = gateCompiled gsrc
            , admittedRepairs = notes
            , admittedChecked = checkedNotes ranSpec result
            }

{- | Repair may only run on a fault in the candidate itself. A timeout, or a
failure at a stage the candidate never reached, is not the model's to fix and
not something a rewrite of its cell could clear.
-}
repairArmed :: Bool -> DisposableVerdict -> DisposableResult -> Bool
repairArmed enabled verdict result =
    enabled && verdict == DisposableCompileError && reachedCandidate result

disclosure :: Frontier -> Text
disclosure frontier =
    "Applied GHC's suggested fix before committing: "
        <> T.intercalate "; " (frontierFixes frontier)
        <> "."

{- | Walks the frontier forward one proven fix at a time, re-reading the
diagnostic each round. @Right@ commits, carrying the spec its winning probe
ran; @Left@ is as far as the search got.
-}
searchRepair ::
    App ->
    Maybe Int ->
    Frontier ->
    IO (Either Frontier (Frontier, CandidateSpec))
searchRepair app mReplaces = rounds repairRounds probeBudget Set.empty
  where
    rounds ::
        Int ->
        Int ->
        Set Text ->
        Frontier ->
        IO (Either Frontier (Frontier, CandidateSpec))
    rounds left budget seen frontier
        | left <= 0 || budget <= 0 = pure (Left frontier)
        | otherwise = do
            cands <-
                gateCandidates
                    app
                    (disposableDiagnostic (frontierResult frontier))
                    (frontierSrc frontier)
            probe left budget seen frontier [c | c <- cands, unseen seen c]

    unseen seen (candidate, _) = not (candidate `Set.member` seen)

    probe ::
        Int ->
        Int ->
        Set Text ->
        Frontier ->
        [(Text, [Text])] ->
        IO (Either Frontier (Frontier, CandidateSpec))
    probe _ _ _ frontier [] = pure (Left frontier)
    probe left budget seen frontier ((candidate, fixes) : rest)
        | budget <= 0 = pure (Left frontier)
        | otherwise = do
            let spec = compileGateSpec mReplaces candidate
            result <- runDisposableTry app spec
            let seen' = Set.insert candidate seen
            case stepFrontier frontier (candidate, fixes) result of
                Commit reached -> pure (Right (reached, spec))
                Advance reached -> rounds (left - 1) (budget - 1) seen' reached
                Skip -> probe left (budget - 1) seen' frontier rest
