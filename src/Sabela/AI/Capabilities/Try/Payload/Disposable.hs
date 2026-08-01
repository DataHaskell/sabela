{-# LANGUAGE OverloadedStrings #-}

{- | What a disposable materialization is reported as. Reads the shared stage
classifier, so `try` and the compile gate cannot drift on the question of
whose code a failure belongs to.
-}
module Sabela.AI.Capabilities.Try.Payload.Disposable (
    disposablePayload,
    skippedCellHint,
    disposableOutcomeText,
) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Verdict (verdictTag)
import Sabela.Session.MaterializeStage (
    Attribution (..),
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    MaterializeStage (..),
    SkippedCell (..),
    attributionOf,
    attributionText,
    blamedCell,
    blockedRemedy,
    materializeStageText,
    resultVerdictClass,
    stageReachedCandidate,
 )

{- | A trial's result. A failure at a stage the candidate was never put to is
the harness rebuilding the context around it, so it is reported as such rather
than as a verdict on the submitted code.
-}
disposablePayload :: DisposableResult -> Value
disposablePayload result =
    case disposableFailure result of
        Just failure
            | not (stageReachedCandidate (failureStage failure)) ->
                contextBlockedPayload result failure
        _ ->
            object
                ( [ "route" .= disposableRoute result
                  , "verdict" .= verdictTag (resultVerdictClass result)
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
        , "hint" .= skippedCellHint (skippedCellId skip)
        ]

skippedCellHint :: Int -> Text
skippedCellHint cid =
    "cell "
        <> tShow cid
        <> " was not replayed because of its own unresolved error (the reason \
           \field), so nothing it imports or defines is in scope for your \
           \candidate. Declare what you need in the candidate itself, or fix \
           \that cell first. Its names being not in scope here is not a \
           \verdict on your code."

contextBlockedPayload :: DisposableResult -> MaterializeFailure -> Value
contextBlockedPayload result failure =
    object
        ( [ "route" .= disposableRoute result
          , "verdict" .= verdictTag (resultVerdictClass result)
          , "outcome" .= blockedOutcome attribution
          , "attribution" .= reason
          , "reason" .= reason
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
            <> ["blamedCell" .= cid | Just cid <- [blamedCell result]]
            <> failurePairs (disposableFailure result)
        )
  where
    attribution = attributionOf result
    reason = blockedReason (failureStage failure) attribution

{- | Whose code was running, as one machine-readable word. Only a cell the
stage itself ran was blocked in its replay; scaffolding that ran after a cell
is still scaffolding, and the reason field says so.
-}
blockedOutcome :: Attribution -> Text
blockedOutcome (AttributedCell _) = "replay_blocked"
blockedOutcome _ = "harness_blocked"

{- | Where the trial stopped and whose code was running there. Says only what
the stage and the replay record support, so a failure the harness owns is
never handed back as the caller's.
-}
blockedReason :: MaterializeStage -> Attribution -> Text
blockedReason stage attribution =
    "Your candidate was never reached: the trial run stopped at the "
        <> materializeStageText stage
        <> " stage, in "
        <> attributionText attribution
        <> ". This is not a verdict on your code — see the failure field for \
           \the diagnostic. "
        <> blockedRemedy stage attribution

tShow :: Int -> Text
tShow = T.pack . show

{- | A green trial that printed nothing and inferred no type. States the two
facts the record holds rather than concluding from them that no expression
ran, which the record does not say.
-}
silentSuccessPairs :: DisposableResult -> [Pair]
silentSuccessPairs result
    | disposableVerdict result == DisposableOk
        && isNothing (disposableType result)
        && T.null (T.strip (disposableStdout result)) =
        [ "diagnostic"
            .= ( "Candidate compiled successfully; it printed nothing and no \
                 \expression type was inferred." ::
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

disposableOutcomeText :: DisposableVerdict -> Text
disposableOutcomeText verdict = case verdict of
    DisposableOk -> "ok"
    DisposableCompileError -> "compile_error"
    DisposableRuntimeError -> "runtime_error"
    DisposableTimedOut -> "timed_out"
    DisposableUnavailable -> "unavailable"
