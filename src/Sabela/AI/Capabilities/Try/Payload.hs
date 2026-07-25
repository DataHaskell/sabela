{-# LANGUAGE OverloadedStrings #-}

{- | Pure JSON-envelope construction for 'Sabela.AI.Capabilities.Try': one
function per verdict class, split out of that module for the module-size
cap. No IO, no App — only the typed verdicts 'Try' has already computed.
-}
module Sabela.AI.Capabilities.Try.Payload (
    purePayload,
    unrestrictedIOPayload,
    disposablePayload,
    planErrorPayload,
    invariantPayload,
    inputErrorPayload,
    trialPlanErrorText,
) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.TryPlan (TrialPlanError (..))
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)
import Sabela.Session.Materialize (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    MaterializeStage (..),
    SkippedCell (..),
    materializeStageText,
 )
import qualified Sabela.SessionTypes as ST

purePayload :: ST.PureEvalResult -> Value
purePayload result =
    object
        [ "route" .= ("pure_live" :: Text)
        , "verdict" .= verdictTag (pureVerdictClass (ST.pureEvalVerdict result))
        , "outcome" .= pureOutcomeText (ST.pureEvalVerdict result)
        , "type" .= ST.pureEvalInferredType result
        , "stdout" .= ST.pureEvalOutput result
        , "stderr" .= ST.pureEvalError result
        , "purityAssurance" .= ("type_only" :: Text)
        , "pollutionContract" .= ("semantic_read_only" :: Text)
        , "generation" .= ST.pureEvalGeneration result
        , "bindingsUnchanged" .= ST.pureEvalBindingsUnchanged result
        , "itUnchanged" .= ST.pureEvalItUnchanged result
        , "recovery" .= pureRecoveryText (ST.pureEvalRecovery result)
        ]

unrestrictedIOPayload :: ST.PureEvalResult -> Value
unrestrictedIOPayload result =
    object
        [ "route" .= ("unavailable" :: Text)
        , "verdict" .= verdictTag VerdictCouldNotRun
        , "outcome" .= ("unavailable" :: Text)
        , "reason"
            .= ( "The candidate has an unrestricted IO type and no qualified containment backend is available; no candidate code ran." ::
                    Text
               )
        , "type" .= ST.pureEvalInferredType result
        , "purityAssurance" .= ("type_only" :: Text)
        , "generation" .= ST.pureEvalGeneration result
        , "bindingsUnchanged" .= ST.pureEvalBindingsUnchanged result
        , "itUnchanged" .= ST.pureEvalItUnchanged result
        ]

disposablePayload :: DisposableResult -> Value
disposablePayload result =
    case disposableFailure result >>= attributableCell of
        Just cid -> replayBlockedPayload result cid
        Nothing ->
            object
                ( [ "route" .= disposableRoute result
                  , "verdict" .= verdictTag (disposableVerdictClass (disposableVerdict result))
                  , "outcome" .= disposableOutcomeText (disposableVerdict result)
                  , "type" .= disposableType result
                  , "stdout" .= disposableStdout result
                  , "stderr" .= disposableStderr result
                  , "purityAssurance" .= ("type_only" :: Text)
                  , "pollutionContract" .= ("disposable_session" :: Text)
                  , "replayedCells" .= disposableReplayedCells result
                  , "skippedCells" .= map skippedCellValue (disposableSkippedCells result)
                  , "dependencies" .= disposableDependencies result
                  ]
                    <> failurePairs (disposableFailure result)
                    <> silentSuccessPairs result
                )

skippedCellValue :: SkippedCell -> Value
skippedCellValue skip =
    object
        [ "cellId" .= skippedCellId skip
        , "reason" .= skippedReason skip
        ]

{- | The cell a failure is attributable to: a failure that happened WHILE
replaying the notebook prefix (a prior cell, never the candidate). Keyed on a
present cell id at a replay stage so it never captures a candidate-stage error.
-}
attributableCell :: MaterializeFailure -> Maybe Int
attributableCell failure = case (failureStage failure, failureCellId failure) of
    (StageCellReplay, Just cid) -> Just cid
    (StagePrelude, Just cid) -> Just cid
    _ -> Nothing

{- | G4: a replay-stage failure is another cell's fault, not the candidate's.
Lead with the attribution and withhold the raw diagnostic from @stderr@ (it is
kept, cell-labelled, under @failure@) so the model never reads a prior cell's
error as a verdict on its own code and abandons a correct candidate.
-}
replayBlockedPayload :: DisposableResult -> Int -> Value
replayBlockedPayload result cid =
    object
        ( [ "route" .= disposableRoute result
          , "verdict" .= verdictTag VerdictCouldNotRun
          , "outcome" .= ("replay_blocked" :: Text)
          , "attribution" .= attributionText cid
          , "reason" .= attributionText cid
          , "type" .= (Nothing :: Maybe Text)
          , "stdout" .= ("" :: Text)
          , "stderr" .= ("" :: Text)
          , "purityAssurance" .= ("type_only" :: Text)
          , "pollutionContract" .= ("disposable_session" :: Text)
          , "candidateReached" .= False
          , "replayedCells" .= disposableReplayedCells result
          , "skippedCells" .= map skippedCellValue (disposableSkippedCells result)
          , "dependencies" .= disposableDependencies result
          ]
            <> failurePairs (disposableFailure result)
        )

attributionText :: Int -> Text
attributionText cid =
    "Notebook cell "
        <> tShow cid
        <> " failed while rebuilding the trial context, so your candidate was \
           \never reached. This is that cell's own error, not a verdict on your \
           \code — see the failure field for its diagnostic. Fix or remove cell "
        <> tShow cid
        <> " (or skip it by making it compile), then retry."

tShow :: Int -> Text
tShow = T.pack . show

silentSuccessPairs :: DisposableResult -> [Pair]
silentSuccessPairs result
    | disposableVerdict result == DisposableOk
        && T.null (T.strip (disposableStdout result))
        && T.null (T.strip (disposableStderr result)) =
        [ "diagnostic"
            .= ( "Candidate declarations compiled successfully; no expression was executed." ::
                    Text
               )
        ]
    | otherwise = []

failurePairs :: Maybe MaterializeFailure -> [Pair]
failurePairs Nothing = []
failurePairs (Just failure) =
    [ "failure"
        .= object
            [ "stage" .= materializeStageText (failureStage failure)
            , "cellId" .= failureCellId failure
            , "message" .= failureMessage failure
            ]
    ]

planErrorPayload :: TrialPlanError -> Value
planErrorPayload planErr =
    object
        [ "route" .= ("unavailable" :: Text)
        , "verdict" .= verdictTag VerdictDiagnostic
        , "outcome" .= ("rejected" :: Text)
        , "reason" .= trialPlanErrorText planErr
        ]

invariantPayload :: Text -> Value
invariantPayload reason =
    object
        [ "route" .= ("unavailable" :: Text)
        , "verdict" .= verdictTag VerdictCouldNotRun
        , "outcome" .= ("invariant_failed" :: Text)
        , "reason" .= reason
        ]

inputErrorPayload :: Text -> Value
inputErrorPayload reason =
    object
        [ "route" .= ("unavailable" :: Text)
        , "verdict" .= verdictTag VerdictDiagnostic
        , "outcome" .= ("invalid_input" :: Text)
        , "reason" .= reason
        ]

{- | G7 admission-message parity (docs/discover/implementation-plan.md task
4): where a committed cell genuinely accepts more than a trial does — any
number of statements, any effect, a compile-time escape — the rejection
names that difference explicitly rather than leaving a bare "no code ran"
for the model to theorize about (live_test5 turns 11-16). 'TrialMetaCommand'
has no such gap: no cell source can hold a GHCi meta-command either, so its
message stays a plain refusal.
-}
trialPlanErrorText :: TrialPlanError -> Text
trialPlanErrorText planErr = case planErr of
    TrialEmpty -> "code required"
    TrialMultipleExpressions ->
        cellsAcceptTryDoesNot
            "a trial previews exactly one result and cannot follow more than \
            \one final expression; a committed cell may run as many statements \
            \as it likes"
    TrialExpressionNotFinal ->
        cellsAcceptTryDoesNot
            "a trial's expression must come last so its value is unambiguous; \
            \a committed cell has no such ordering restriction"
    TrialEffectfulStatement stmt ->
        cellsAcceptTryDoesNot
            ("a trial cannot bind an effect it cannot safely undo: " <> stmt)
    TrialMetaCommand cmd ->
        "GHCi meta-commands are not admitted by try; no command ran: " <> cmd
    TrialUnsafeSyntax reason ->
        cellsAcceptTryDoesNot
            ("a trial must stay safely discardable and cannot own a \
             \compile-time or purity escape: " <> reason)

-- | The task-4 parity clause, shared by every rejection that names a genuine
-- cell-vs-try policy gap rather than a universal safety restriction.
cellsAcceptTryDoesNot :: Text -> Text
cellsAcceptTryDoesNot reason =
    "cells accept this; try does not, because " <> reason <> "; no code ran"

pureOutcomeText :: ST.PureEvalVerdict -> Text
pureOutcomeText verdict = case verdict of
    ST.PureEvalSucceeded -> "ok"
    ST.PureEvalRejected -> "compile_error"
    ST.PureEvalRuntimeError -> "runtime_error"
    ST.PureEvalTimedOut -> "timed_out"
    ST.PureEvalStale -> "stale"
    ST.PureEvalInvariantFailed -> "invariant_failed"
    ST.PureEvalUnavailable -> "unavailable"

pureRecoveryText :: ST.PureEvalRecovery -> Text
pureRecoveryText recovery = case recovery of
    ST.PureEvalNoRecovery -> "none"
    ST.PureEvalInterrupted -> "interrupted"
    ST.PureEvalKernelDestroyed -> "restarted_kernel"

pureVerdictClass :: ST.PureEvalVerdict -> VerdictClass
pureVerdictClass verdict = case verdict of
    ST.PureEvalSucceeded -> VerdictOk
    ST.PureEvalRejected -> VerdictDiagnostic
    ST.PureEvalRuntimeError -> VerdictDiagnostic
    ST.PureEvalTimedOut -> VerdictDiagnostic
    ST.PureEvalStale -> VerdictCouldNotRun
    ST.PureEvalInvariantFailed -> VerdictCouldNotRun
    ST.PureEvalUnavailable -> VerdictCouldNotRun

disposableOutcomeText :: DisposableVerdict -> Text
disposableOutcomeText verdict = case verdict of
    DisposableOk -> "ok"
    DisposableCompileError -> "compile_error"
    DisposableRuntimeError -> "runtime_error"
    DisposableTimedOut -> "timed_out"
    DisposableUnavailable -> "unavailable"

disposableVerdictClass :: DisposableVerdict -> VerdictClass
disposableVerdictClass verdict = case verdict of
    DisposableOk -> VerdictOk
    DisposableCompileError -> VerdictDiagnostic
    DisposableRuntimeError -> VerdictDiagnostic
    DisposableTimedOut -> VerdictDiagnostic
    DisposableUnavailable -> VerdictCouldNotRun
