{-# LANGUAGE OverloadedStrings #-}

{- | Which of the two published tiers an answer was reached at. Read from the
record of what the run did, never from the plan: a candidate the plan proposed
evaluating and the session then declined as IO was not evaluated.
-}
module Sabela.AI.Capabilities.Try.Tier (
    tierText,
    tierPairs,
    liveEvaluated,
    disposableEvaluated,
    reachedCandidate,
) where

import Data.Aeson ((.=))
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.TryPlan (TrialTier (..))
import Sabela.AI.ToolDoc (tierEvaluatePure, tierTypecheckOnly)
import Sabela.Session.Materialize (
    CandidateSpec (..),
    DisposableResult (..),
    MaterializeFailure (..),
    unrestrictedIOError,
 )
import Sabela.Session.MaterializeStage (
    DisposableVerdict (..),
    stageReachedCandidate,
 )
import qualified Sabela.SessionTypes as ST

-- | The published name of a tier, so the field and the surface use one word.
tierText :: TrialTier -> Text
tierText TierPure = tierEvaluatePure
tierText TierContained = tierTypecheckOnly

{- | The two facts an answer states about performance, emitted together from one
measurement so the tier cannot contradict the execution it is read from. A run
whose record settles neither states neither.
-}
tierPairs :: Maybe Bool -> [Pair]
tierPairs Nothing = []
tierPairs (Just evaluated) =
    [ "executed" .= evaluated
    , "tier" .= tierText (if evaluated then TierPure else TierContained)
    ]

{- | Whether the live probe evaluated the candidate's value. Only the verdicts
that settle it answer: a probe that timed out, went stale or broke its
invariant did not measure whether anything ran.
-}
liveEvaluated :: ST.PureEvalResult -> Maybe Bool
liveEvaluated result = case ST.pureEvalVerdict result of
    ST.PureEvalSucceeded -> Just True
    ST.PureEvalRuntimeError -> Just True
    ST.PureEvalUnshowable -> Just False
    ST.PureEvalRejected -> Just False
    _ -> Nothing

{- | Whether the disposable run evaluated the candidate's value. A run that
never reached the candidate settles nothing, and neither does a green run that
returned no value: the record does not distinguish it from one left unshown.
-}
disposableEvaluated :: CandidateSpec -> DisposableResult -> Maybe Bool
disposableEvaluated spec result
    | not (reachedCandidate result) = Nothing
    | Nothing <- candidateExpression spec = Just False
    | unrestrictedIOError (disposableStderr result) = Just False
    | otherwise = case disposableVerdict result of
        DisposableCompileError -> Just False
        DisposableRuntimeError -> Just True
        DisposableOk
            | not (T.null (T.strip (disposableStdout result))) -> Just True
        _ -> Nothing

{- | Whether the trial reached the candidate at all. A run that stopped
rebuilding the context around it measured nothing about the candidate.
-}
reachedCandidate :: DisposableResult -> Bool
reachedCandidate result =
    maybe True (stageReachedCandidate . failureStage) (disposableFailure result)
