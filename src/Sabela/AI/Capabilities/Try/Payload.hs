{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Sabela.AI.Capabilities.Try.Payload (
    purePayload,
    unshowablePayload,
    pureOutcomeText,
    pureVerdictClass,
    disposablePayload,
    executionPairs,
    planErrorPayload,
    invariantPayload,
    inputErrorPayload,
    skippedCellHint,
    trialPlanErrorText,
    trialRefusalClauses,
    refusalClauseFor,
) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Text (Text)

import Sabela.AI.Capabilities.Try.Payload.Disposable (
    disposablePayload,
    skippedCellHint,
 )
import Sabela.AI.Capabilities.Try.Tier (
    disposableEvaluated,
    liveEvaluated,
    tierPairs,
 )
import Sabela.AI.Capabilities.TryPlan (MetaQuery (..), TrialPlanError (..))
import Sabela.AI.FitRule (holeFitsJson)
import Sabela.AI.ToolDoc (
    refusalEmpty,
    refusalExpressionNotFinal,
    refusalMetaCommand,
    refusalMetaQuery,
    refusalMultipleExpressions,
    refusalUnsafeSyntax,
 )
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)
import Sabela.Session.Materialize (
    CandidateSpec (..),
    DisposableResult (..),
    unrestrictedIOError,
 )
import qualified Sabela.SessionTypes as ST

{- | Whether the trial evaluated anything, and the tier that fact settles. Only
a run whose record answers the question reports it, so nothing here is stated
about a run that never reached the candidate.
-}
executionPairs :: CandidateSpec -> DisposableResult -> [Pair]
executionPairs spec result = case disposableEvaluated spec result of
    Nothing -> []
    evaluated -> tierPairs evaluated <> note
  where
    note
        | Nothing <- candidateExpression spec = ["executionNote" .= compiledOnlyNote]
        | unrestrictedIOError (disposableStderr result) =
            ["executionNote" .= ioNotRunNote]
        | otherwise = []

compiledOnlyNote :: Text
compiledOnlyNote =
    "No expression was submitted for evaluation, and any statement in the \
    \candidate was compiled as a binding rather than performed, so none of the \
    \candidate ran. insert_cell and execute_cell are the tools that run code."

ioNotRunNote :: Text
ioNotRunNote =
    "The session inferred an IO type for the candidate expression and did not \
    \evaluate it; the type is what this trial establishes. insert_cell and \
    \execute_cell are the tools that run code."

{- | A bare @_@ is not a typed hole to the router, so its candidate runs on the
live session and GHC's fits arrive inside stderr. Surfacing them costs nothing:
the diagnostic already carries them.
-}
pureHoleFitPairs :: ST.PureEvalResult -> [Pair]
pureHoleFitPairs result = case holeFitsJson pureFitCap (ST.pureEvalError result) of
    [] -> []
    fits -> ["holeFits" .= fits]

pureFitCap :: Int
pureFitCap = 8

purePayload :: ST.PureEvalResult -> Value
purePayload result =
    object $
        pureHoleFitPairs result
            <> tierPairs (liveEvaluated result)
            <> [ "route" .= ("pure_live" :: Text)
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

{- | The candidate typechecked and the harness could not echo its value. That is
the harness's limit, so it reports what it learned rather than a diagnostic.
-}
unshowablePayload :: ST.PureEvalResult -> Value
unshowablePayload result =
    object $
        tierPairs (liveEvaluated result)
            <> [ "route" .= ("pure_live" :: Text)
               , "verdict" .= verdictTag VerdictOk
               , "outcome" .= ("ok" :: Text)
               , "type" .= inferred
               , "valueShown" .= False
               , "reason"
                    .= ( "The candidate typechecks as "
                            <> inferred
                            <> "; its value is not shown because "
                            <> inferred
                            <> " has no Show instance. Nothing ran."
                       )
               , "purityAssurance" .= ("type_only" :: Text)
               , "pollutionContract" .= ("semantic_read_only" :: Text)
               , "generation" .= ST.pureEvalGeneration result
               , "bindingsUnchanged" .= ST.pureEvalBindingsUnchanged result
               , "itUnchanged" .= ST.pureEvalItUnchanged result
               ]
  where
    inferred = ST.pureEvalInferredType result

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

{- | The refusal the caller reads, stated in the published grammar's own words
so a clause the surface advertises is the clause the tool returns.
-}
trialPlanErrorText :: TrialPlanError -> Text
trialPlanErrorText planErr = case planErr of
    TrialEmpty -> refusalEmpty
    TrialMultipleExpressions ->
        cellsAcceptTryDoesNot
            ( "a trial previews exactly one result and "
                <> refusalMultipleExpressions
                <> "; a committed cell may run as many statements as it likes"
            )
    TrialExpressionNotFinal ->
        cellsAcceptTryDoesNot
            ( refusalExpressionNotFinal
                <> " so its value is unambiguous; a committed cell has no such \
                   \ordering restriction"
            )
    TrialMetaCommand cmd -> metaCommandRefusal <> "; no command ran: " <> cmd
    TrialMetaQuery q ->
        mqCommand q <> " " <> refusalMetaQuery <> "; no command ran"
    TrialUnsafeSyntax reason ->
        cellsAcceptTryDoesNot
            ( "a trial must stay safely discardable and "
                <> refusalUnsafeSyntax
                <> ": "
                <> reason
            )

metaCommandRefusal :: Text
metaCommandRefusal =
    "GHCi "
        <> refusalMetaCommand
        <> " by try; check_type answers type and info questions and discover \
           \answers module and search questions"

{- | The classes of source try refuses. 'refusalClassOf' is a total case over
'TrialPlanError' and this module builds incomplete patterns as errors, so a new
constructor without a class here does not compile.
-}
data RefusalClass
    = RCEmpty
    | RCMultipleExpressions
    | RCExpressionNotFinal
    | RCMetaCommand
    | RCMetaQuery
    | RCUnsafeSyntax
    deriving (Bounded, Enum, Eq, Show)

refusalClassOf :: TrialPlanError -> RefusalClass
refusalClassOf planErr = case planErr of
    TrialEmpty -> RCEmpty
    TrialMultipleExpressions -> RCMultipleExpressions
    TrialExpressionNotFinal -> RCExpressionNotFinal
    TrialMetaCommand _ -> RCMetaCommand
    TrialMetaQuery _ -> RCMetaQuery
    TrialUnsafeSyntax _ -> RCUnsafeSyntax

{- | What try will not accept, as the surface that recommends it must state it.
One clause per refusal class, so a class the surface does not mention cannot be
added without this list changing.
-}
trialRefusalClauses :: [Text]
trialRefusalClauses = map refusalClause [minBound .. maxBound]

-- | The clause the published grammar carries for a refusal the planner returned.
refusalClauseFor :: TrialPlanError -> Text
refusalClauseFor = refusalClause . refusalClassOf

-- | Each class under the clause the published grammar states for it.
refusalClause :: RefusalClass -> Text
refusalClause rc = case rc of
    RCEmpty -> refusalEmpty
    RCMultipleExpressions -> refusalMultipleExpressions
    RCExpressionNotFinal -> refusalExpressionNotFinal
    RCMetaCommand -> refusalMetaCommand
    RCMetaQuery -> refusalMetaQuery
    RCUnsafeSyntax -> refusalUnsafeSyntax

{- | The parity clause every policy-only refusal carries. It names the tool this
source is addressed to and says nothing about the verdict that tool will reach,
which try did not run and cannot know.
-}
cellsAcceptTryDoesNot :: Text -> Text
cellsAcceptTryDoesNot reason =
    "cells accept this; try does not, because "
        <> reason
        <> "; no code ran. insert_cell is the tool this source goes to."

pureOutcomeText :: ST.PureEvalVerdict -> Text
pureOutcomeText verdict = case verdict of
    ST.PureEvalSucceeded -> "ok"
    ST.PureEvalUnshowable -> "ok"
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
    ST.PureEvalUnshowable -> VerdictOk
    ST.PureEvalRejected -> VerdictDiagnostic
    ST.PureEvalRuntimeError -> VerdictDiagnostic
    ST.PureEvalTimedOut -> VerdictDiagnostic
    ST.PureEvalStale -> VerdictCouldNotRun
    ST.PureEvalInvariantFailed -> VerdictCouldNotRun
    ST.PureEvalUnavailable -> VerdictCouldNotRun
