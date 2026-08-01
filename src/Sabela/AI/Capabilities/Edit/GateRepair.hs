{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.GateRepair (
    gatedCandidate,
    repairArmed,
    acceptGreen,
    frontierRejectionJson,
) where

import Data.Bifunctor (second)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.CompileGate (
    GateSource (..),
    compileGateSpec,
    exposingPackage,
    gateDefaultingRejection,
    gateHoleNudge,
    prevDefinedNames,
    rejectionJson,
 )
import Sabela.AI.Capabilities.Edit.GateFrontier (
    Frontier (..),
    Step (..),
    disposableDiagnostic,
    frontierPairs,
    probeBudget,
    repairRounds,
    startFrontier,
    stepFrontier,
 )
import Sabela.AI.Capabilities.Edit.GateRepair.Candidates (gateCandidates)
import Sabela.AI.Capabilities.Edit.HoleNudge (attachPairs)
import Sabela.AI.Capabilities.Edit.Submission (
    Submission,
    compiledText,
    submittedText,
 )
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.PathGate (pathGateCheck)
import Sabela.AI.WriteAck (refusalAck)
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.Session.Materialize (runDisposableTry)
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
import Data.Aeson.Types (Pair)

{- | The gate stack a programmatic write passes before it may commit:
compile first (with repair), then the paths the surviving source names.
-}
gatedCandidate ::
    App ->
    Maybe Int ->
    CellLang ->
    CellType ->
    Submission ->
    IO (Either Value (Text, [Text]))
gatedCandidate app mReplaces lang ty sub
    | ty /= CodeCell || lang /= Haskell = pure (Right (compiledText sub, []))
    | otherwise = do
        compiled <- compileGatedCandidate app mReplaces sub
        case compiled of
            Left rejection -> pure (Left rejection)
            Right (src', notes) ->
                keepingNotes notes <$> pathGated app mReplaces src'

keepingNotes ::
    [Text] -> Either Value (Text, [Text]) -> Either Value (Text, [Text])
keepingNotes earlier = fmap (second (earlier <>))

{- | Rejects a write whose paths are not there, and silently corrects one
whose only fault is the directory. Runs on compile-green source so the
compiler's diagnostic always wins the turn when there is one.
-}
pathGated :: App -> Maybe Int -> Text -> IO (Either Value (Text, [Text]))
pathGated app mReplaces src = do
    others <- otherCellSources app mReplaces
    result <- pathGateCheck (envWorkDir (appEnv app)) others src
    pure (either (Left . refusalAck "path-gate" mReplaces) Right result)

otherCellSources :: App -> Maybe Int -> IO [Text]
otherCellSources app mReplaces = do
    nb <- readNotebook (appNotebook app)
    pure [cellSource c | c <- nbCells nb, Just (cellId c) /= mReplaces]

compileGatedCandidate ::
    App ->
    Maybe Int ->
    Submission ->
    IO (Either Value (Text, [Text]))
compileGatedCandidate app mReplaces sub = do
    result <- runDisposableTry app (compileGateSpec mReplaces src)
    case disposableVerdict result of
        DisposableOk -> pure (acceptGreen mReplaces [] (submissionSource sub) result)
        verdict -> do
            enabled <- featureEnabled "SABELA_GATE_REPAIR"
            let start = startFrontier (submittedText sub) src result
            searched <-
                if repairArmed enabled verdict result
                    then searchRepair app mReplaces start
                    else pure (Left start)
            case searched of
                Right frontier ->
                    pure
                        ( acceptGreen
                            mReplaces
                            [disclosure frontier]
                            (frontierSource frontier)
                            (frontierResult frontier)
                        )
                Left frontier -> Left <$> frontierRejection app mReplaces frontier
  where
    src = compiledText sub

-- | What the caller submitted against what the gate put to the compiler.
submissionSource :: Submission -> GateSource
submissionSource sub =
    GateSource
        { gateSubmitted = submittedText sub
        , gateCompiled = compiledText sub
        }

frontierSource :: Frontier -> GateSource
frontierSource frontier =
    GateSource
        { gateSubmitted = frontierSubmitted frontier
        , gateCompiled = frontierSrc frontier
        }

{- | A green candidate is admitted only once the defaulting check clears it,
whether it arrived green or went green under repair. A refusal carries the
notes the commit would have carried.
-}
acceptGreen ::
    Maybe Int ->
    [Text] ->
    GateSource ->
    DisposableResult ->
    Either Value (Text, [Text])
acceptGreen mReplaces notes gsrc result =
    maybe
        (Right (gateCompiled gsrc, notes))
        Left
        (gateDefaultingRejection mReplaces notes gsrc result)

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

{- | Rejects on the frontier rather than on the submission: the errors the
model still has to answer are the ones left after every proven fix, not the
first one that stopped the compiler.
-}
frontierRejection :: App -> Maybe Int -> Frontier -> IO Value
frontierRejection app mReplaces frontier = do
    prevDefined <- prevDefinedNames app mReplaces
    nudge <- gateHoleNudge app mReplaces verdict diagnostic (frontierSrc frontier)
    exposedBy <- exposingPackage diagnostic
    pure (frontierRejectionJson exposedBy mReplaces prevDefined nudge frontier)
  where
    verdict = disposableVerdict (frontierResult frontier)
    diagnostic = disposableDiagnostic (frontierResult frontier)

{- | The rejection a frontier produces, given the facts the IO around it went
and fetched. Pure, so the payload the model reads can be pinned without a
session standing behind it.
-}
frontierRejectionJson ::
    Maybe Text -> Maybe Int -> [Text] -> [Pair] -> Frontier -> Value
frontierRejectionJson exposedBy mReplaces prevDefined nudge frontier =
    attachPairs (nudge <> frontierPairs frontier) $
        rejectionJson
            exposedBy
            mReplaces
            (frontierSource frontier)
            prevDefined
            (frontierResult frontier)

{- | Walks the frontier forward one proven fix at a time, re-reading the
diagnostic each round so a fix that only uncovers the next error still earns
its round. @Right@ is green and commits; @Left@ is as far as the search got.
-}
searchRepair :: App -> Maybe Int -> Frontier -> IO (Either Frontier Frontier)
searchRepair app mReplaces = rounds repairRounds probeBudget Set.empty
  where
    rounds :: Int -> Int -> Set Text -> Frontier -> IO (Either Frontier Frontier)
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
        IO (Either Frontier Frontier)
    probe _ _ _ frontier [] = pure (Left frontier)
    probe left budget seen frontier ((candidate, fixes) : rest)
        | budget <= 0 = pure (Left frontier)
        | otherwise = do
            result <- runDisposableTry app (compileGateSpec mReplaces candidate)
            let seen' = Set.insert candidate seen
            case stepFrontier frontier (candidate, fixes) result of
                Commit reached -> pure (Right reached)
                Advance reached -> rounds (left - 1) (budget - 1) seen' reached
                Skip -> probe left (budget - 1) seen' frontier rest
