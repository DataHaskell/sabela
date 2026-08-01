{-# LANGUAGE OverloadedStrings #-}

{- | How a planned trial becomes something the disposable session can compile,
and how a rejected one is cut back to the largest prefix that compiles.
-}
module Sabela.AI.Capabilities.Try.Candidate (
    ContainedRun,
    candidateSpec,
    localiseTrial,
    probePrefix,
) where

import Data.Aeson.Types (Pair)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)

import Sabela.AI.Capabilities.Edit.CompileGate.Render (renderNonExecuting)
import Sabela.AI.Capabilities.Edit.GateFrontier (probeBudget, probeDeadlineUs)
import Sabela.AI.Capabilities.Edit.GateFrontier.Localise (
    localisationPairs,
    localiseCandidate,
 )
import Sabela.AI.Capabilities.Try.Tier (reachedCandidate)
import Sabela.AI.Capabilities.TryPlan (
    TrialPlan (..),
    TrialTier (..),
    planTrial,
 )
import Sabela.Session.Materialize (
    CandidateSpec (..),
    DisposableResult (..),
 )
import Sabela.Session.MaterializeStage (DisposableVerdict (..))

{- | How a trial reaches the contained session. Named so the route a trial takes
can be observed without a materialisation.
-}
type ContainedRun = CandidateSpec -> IO DisposableResult

{- | The localisation a rejected trial carries, or nothing when the candidate
was never put to the compiler as a whole. Bounded by both a probe count and a
clock, so the search costs at most one probe past its deadline.
-}
localiseTrial :: ContainedRun -> TrialPlan -> DisposableResult -> IO [Pair]
localiseTrial run plan result
    | not (reachedCandidate result) = pure []
    | otherwise = do
        deadline <- (+ probeDeadlineSeconds) <$> getMonotonicTime
        maybe [] localisationPairs
            <$> localiseCandidate
                probeBudget
                (probePrefix run deadline)
                (trialSource plan)
                result

probeDeadlineSeconds :: Double
probeDeadlineSeconds = fromIntegral probeDeadlineUs / 1000000

{- | Compile one prefix of a rejected candidate. A prefix the planner will not
take, or one asked for past the deadline, is unavailable rather than failed, so
the bisection gives up instead of blaming a component for the harness's limit.
-}
probePrefix :: ContainedRun -> Double -> Text -> IO DisposableResult
probePrefix run deadline src = do
    now <- getMonotonicTime
    if now >= deadline
        then pure (unprobedResult "localisation reached its time budget before" src)
        else case planTrial src of
            Right plan -> run (candidateSpec plan)
            Left _ -> pure (unprobedResult "try cannot plan this prefix:" src)

unprobedResult :: Text -> Text -> DisposableResult
unprobedResult reason src =
    DisposableResult
        { disposableRoute = "disposable_scratch"
        , disposableVerdict = DisposableUnavailable
        , disposableType = Nothing
        , disposableStdout = ""
        , disposableStderr = reason <> " " <> src
        , disposableFailure = Nothing
        , disposableReplayedCells = []
        , disposableSkippedCells = []
        , disposableDependencies = []
        }

{- | How a trial is put to the compiler. A contained candidate is rendered the
way the write gate renders a cell — every statement becomes a binding and no
expression is submitted — so it is type-checked and none of it is performed.
-}
candidateSpec :: TrialPlan -> CandidateSpec
candidateSpec plan
    | trialTier plan == TierContained =
        CandidateSpec
            { candidateMetadataSource = trialSource plan
            , candidateSetup = renderNonExecuting (trialSource plan)
            , candidateExpression = Nothing
            , candidateReplacesCellId = Nothing
            , candidateDeliberate = False
            }
candidateSpec plan =
    case trialExpression plan of
        Just expression
            | T.any (`elem` ['\n', '\r']) expression ->
                CandidateSpec
                    { candidateMetadataSource = trialSource plan
                    , candidateSetup =
                        trialSetup plan
                            <> "\n"
                            <> hiddenExpressionBinding expression
                    , candidateExpression = Just "_sabelaTryCandidate"
                    , candidateReplacesCellId = Nothing
                    , candidateDeliberate = False
                    }
        expression ->
            CandidateSpec
                { candidateMetadataSource = trialSource plan
                , candidateSetup = trialSetup plan
                , candidateExpression = expression
                , candidateReplacesCellId = Nothing
                , candidateDeliberate = False
                }

hiddenExpressionBinding :: Text -> Text
hiddenExpressionBinding expression =
    T.unlines
        [ ":{"
        , "_sabelaTryCandidate ="
        , T.unlines ["    " <> line | line <- T.lines expression]
        , ":}"
        ]
