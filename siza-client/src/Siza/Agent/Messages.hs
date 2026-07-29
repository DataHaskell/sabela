{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Messages (
    doneSignal,
    doneSignalMsg,
    noCheckSignal,
    noCheckSignalMsg,
    toolMsg,
    reenterMsg,
    reenterMessage,
    reenterAlarmMsg,
    reenterAlarmMessage,
    streakMsg,
    unconfirmedMessage,
    unconfirmedMsgWith,
    verifyMessage',
    VerifyFailure (..),
    verifyFailureOf,
    verifyHeadline,
    verifyMsg,
    verifyMsgWith,
) where

import Data.Aeson (Value, object, (.=))
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.CellResult (CellId)
import Sabela.AI.SelfHeal (contrastLine, diagQuoteLine)
import Sabela.AI.Verdict (VerdictClass (..), verdictMarker)
import Sabela.LLM.Ollama.Client (ToolCall (..))

toolMsg :: ToolCall -> Text -> Value
toolMsg tc result =
    object
        [ "role" .= ("tool" :: Text)
        , "tool_name" .= tcName tc
        , "content" .= result
        ]

reenterMsg :: [(CellId, Text)] -> Value
reenterMsg reds =
    object
        [ "role" .= ("tool" :: Text)
        , "tool_name" .= ("health_gate" :: Text)
        , "content" .= reenterMessage reds
        ]

reenterMessage :: [(CellId, Text)] -> Text
reenterMessage reds =
    "Not done yet: cell(s) "
        <> T.pack (intercalate ", " (map (show . fst) reds))
        <> " that you wrote still have errors. Read each cell's error, fix it with \
           \replace_cell_source, and do not stop until every cell you wrote runs \
           \without error."
        <> T.concat ["\n" <> diagQuoteLine d | (_, d) <- reds]
        <> T.concat ["\n" <> c | Just c <- map (contrastLine . snd) reds]

reenterAlarmMsg :: [(CellId, Text, Bool)] -> Value
reenterAlarmMsg reds =
    object
        [ "role" .= ("tool" :: Text)
        , "tool_name" .= ("health_gate" :: Text)
        , "content" .= reenterAlarmMessage reds
        ]

reenterAlarmMessage :: [(CellId, Text, Bool)] -> Text
reenterAlarmMessage reds
    | null alarmed = reenterMessage pairs
    | otherwise = alarmLine <> "\n" <> reenterMessage pairs
  where
    pairs = [(cid, diag) | (cid, diag, _) <- reds]
    alarmed = [cid | (cid, _, True) <- reds]
    alarmLine =
        "INVARIANT ALARM: cell(s) "
            <> T.pack (intercalate ", " (map show alarmed))
            <> " committed with a compiler-rejected definition even though \
               \compile-gated writes (G1) should make that impossible — this \
               \is a bug in the write gate, not something a cell edit fixes. \
               \Report it (state the blocker) once the immediate task is handled."

streakMsg :: Text -> Value
streakMsg contrast =
    object
        [ "role" .= ("tool" :: Text)
        , "tool_name" .= ("health_gate" :: Text)
        , "content" .= contrast
        ]

verifyMsgWith :: Int -> [Text] -> Maybe Text -> Value
verifyMsgWith ownedCount missing counterexample =
    verifyChannel
        VerdictDiagnostic
        (verifyMessage' ownedCount missing counterexample)

verifyMessage' :: Int -> [Text] -> Maybe Text -> Text
verifyMessage' ownedCount missing counterexample =
    verifyHeadline cls <> " " <> diagnosis <> " Do not stop until the check passes."
  where
    cls = verifyFailureOf ownedCount missing
    diagnosis = case cls of
        NoDeliverable ->
            "You have written no cell yet — the deliverable must be DEFINED in \
            \a cell. Write it now with insert_cell."
        DeliverableUndefined ns ->
            T.intercalate ", " (map tick ns)
                <> " is not defined in any cell. Define it with insert_cell or \
                   \replace_cell_source."
        CheckFailed
            | Just ce <- counterexample -> ce <> " Fix the logic so it holds."
            | otherwise ->
                "Your cells run, but the computed value does not satisfy the \
                \check — re-read the task's examples and fix the logic."
    tick n = "`" <> n <> "`"

data VerifyFailure
    = NoDeliverable
    | DeliverableUndefined [Text]
    | CheckFailed
    deriving (Eq, Show)

verifyFailureOf :: Int -> [Text] -> VerifyFailure
verifyFailureOf ownedCount missing
    | ownedCount == 0 = NoDeliverable
    | not (null missing) = DeliverableUndefined missing
    | otherwise = CheckFailed

verifyHeadline :: VerifyFailure -> Text
verifyHeadline NoDeliverable =
    "The task is not done: no deliverable cell exists yet (the check has not \
    \run — this is not a check failure)."
verifyHeadline (DeliverableUndefined _) =
    "The task is not done: the deliverable is not defined in any cell (the \
    \check has not run — this is not a check failure)."
verifyHeadline CheckFailed =
    "The task is not done: the deliverable's check still fails."

unconfirmedMsgWith :: Int -> [Text] -> Maybe Text -> Value
unconfirmedMsgWith ownedCount missing guidance =
    verifyChannel
        VerdictCouldNotRun
        (unconfirmedMessage ownedCount missing guidance)

unconfirmedMessage :: Int -> [Text] -> Maybe Text -> Text
unconfirmedMessage ownedCount missing guidance =
    "The deliverable is not yet confirmed — the covering check reached no \
    \verdict. "
        <> diagnosis
        <> " Then finish with a one-line summary."
  where
    diagnosis
        | ownedCount == 0 =
            "You have written no cell yet — the deliverable must be DEFINED in \
            \a cell. Write it now with insert_cell."
        | not (null missing) =
            T.intercalate ", " (map tick missing)
                <> " is not defined in any cell. Define it with insert_cell or \
                   \replace_cell_source."
        | Just g <- guidance = g
        | otherwise =
            "Make the deliverable's value observable (print it or bind it in \
            \a cell), so the check can reach a verdict."
    tick n = "`" <> n <> "`"

doneSignal :: Text
doneSignal =
    "Deliverable confirmed: the covering check passes against the live \
    \notebook. Reply with a one-line summary and stop."

doneSignalMsg :: Value
doneSignalMsg = verifyChannel VerdictOk doneSignal

noCheckSignal :: Text
noCheckSignal =
    "Cells ran clean and the deliverable is committed; no machine check \
    \applies, so none was run. Reply with a one-line summary and stop."

noCheckSignalMsg :: Value
noCheckSignalMsg = verifyChannel VerdictOk noCheckSignal

verifyMsg :: Value
verifyMsg =
    verifyChannel
        VerdictDiagnostic
        "Your cells run, but the task's deliverable does not pass its check \
        \yet. Keep working until the requested binding or plot is correct, \
        \then stop."

verifyChannel :: VerdictClass -> Text -> Value
verifyChannel verdict body =
    object
        [ "role" .= ("tool" :: Text)
        , "tool_name" .= ("verify" :: Text)
        , "content" .= (verdictMarker verdict <> " " <> body)
        ]
