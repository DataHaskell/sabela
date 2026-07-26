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
import Sabela.AI.SelfHeal (contrastLine)
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

{- | Each red cell's id plus its diagnostic; a diagnostic carrying a GHC
did-you-mean gains a wrong-vs-real 'contrastLine' so the model stops
re-submitting the phantom name.
-}
reenterMessage :: [(CellId, Text)] -> Text
reenterMessage reds =
    "Not done yet: cell(s) "
        <> T.pack (intercalate ", " (map (show . fst) reds))
        <> " that you wrote still have errors. Read each cell's error, fix it with \
           \replace_cell_source, and do not stop until every cell you wrote runs \
           \without error."
        <> T.concat ["\n" <> c | Just c <- map (contrastLine . snd) reds]

{- | 'reenterMsg', with each red cell flagged for a compile-class ('Rejected')
outcome — structurally impossible once G1 lands, so a flagged cell is an
invariant alarm ('reenterAlarmMessage' leads with it), not a routine nudge.
-}
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

{- | A mid-loop wrong-vs-real contrast, injected when the same red diagnostic
persists ('Siza.Agent.Streak') — same rail identity as the re-enter gate.
-}
streakMsg :: Text -> Value
streakMsg contrast =
    object
        [ "role" .= ("tool" :: Text)
        , "tool_name" .= ("health_gate" :: Text)
        , "content" .= contrast
        ]

{- | The DIAGNOSTIC verify re-prompt: a giving-up model is told what is
actually missing — no cell written, a requested deliverable undefined — not
only that the check fails.
-}
verifyMsgWith :: Int -> [Text] -> Maybe Text -> Value
verifyMsgWith ownedCount missing counterexample =
    verifyChannel
        VerdictDiagnostic
        (verifyMessage' ownedCount missing counterexample)

{- | The re-prompt text for 'verifyMsgWith'. A named counterexample (the
failing example, with the wrong value when probed) replaces the generic
wrong-value sentence — a giving-up model is shown the exact contradiction.
-}
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

{- | C2.7: what actually went wrong. "No deliverable cell exists" is a
PRECONDITION failure, not a failing check — live_test8 reported it as one
and sent the model to inspect a check that was fine.
-}
data VerifyFailure
    = NoDeliverable
    | DeliverableUndefined [Text]
    | CheckFailed
    deriving (Eq, Show)

-- | Classify a verify failure from the loop's own owned/missing counts.
verifyFailureOf :: Int -> [Text] -> VerifyFailure
verifyFailureOf ownedCount missing
    | ownedCount == 0 = NoDeliverable
    | not (null missing) = DeliverableUndefined missing
    | otherwise = CheckFailed

{- | One headline per class, so the verdict never calls a precondition
failure a check failure.
-}
verifyHeadline :: VerifyFailure -> Text
verifyHeadline NoDeliverable =
    "The task is not done: no deliverable cell exists yet (the check has not \
    \run — this is not a check failure)."
verifyHeadline (DeliverableUndefined _) =
    "The task is not done: the deliverable is not defined in any cell (the \
    \check has not run — this is not a check failure)."
verifyHeadline CheckFailed =
    "The task is not done: the deliverable's check still fails."

{- | The NOT-YET-CONFIRMED verify re-prompt (R5-T5): the check reached no
verdict, so this must never claim failure — it says what is structurally
missing or what to run, then asks the model to finish once confirmed.
-}
unconfirmedMsgWith :: Int -> [Text] -> Maybe Text -> Value
unconfirmedMsgWith ownedCount missing guidance =
    verifyChannel
        VerdictCouldNotRun
        (unconfirmedMessage ownedCount missing guidance)

-- | The re-prompt text for 'unconfirmedMsgWith'; never contains \"fail\".
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

{- | The deliverable-green stop line (R5-T4 consumes this signal): emitted at
most once, only on a recomputed passing check — positive evidence, never a
guess.
-}
doneSignal :: Text
doneSignal =
    "Deliverable confirmed: the covering check passes against the live \
    \notebook. Reply with a one-line summary and stop."

-- | 'doneSignal' on the verify channel.
doneSignalMsg :: Value
doneSignalMsg = verifyChannel VerdictOk doneSignal

{- | The stop line when the cells ran clean but no machine check applies — the
user skipped it, or no proposal survived vetting. It rests the verdict on the
committed artifact and never claims a check that did not run.
-}
noCheckSignal :: Text
noCheckSignal =
    "Cells ran clean and the deliverable is committed; no machine check \
    \applies, so none was run. Reply with a one-line summary and stop."

-- | 'noCheckSignal' on the verify channel.
noCheckSignalMsg :: Value
noCheckSignalMsg = verifyChannel VerdictOk noCheckSignal

verifyMsg :: Value
verifyMsg =
    verifyChannel
        VerdictDiagnostic
        "Your cells run, but the task's deliverable does not pass its check \
        \yet. Keep working until the requested binding or plot is correct, \
        \then stop."

{- | The one verify-channel envelope: every answer opens with its section-5.3
verdict marker, so a marker-less verify message is unrepresentable here.
-}
verifyChannel :: VerdictClass -> Text -> Value
verifyChannel verdict body =
    object
        [ "role" .= ("tool" :: Text)
        , "tool_name" .= ("verify" :: Text)
        , "content" .= (verdictMarker verdict <> " " <> body)
        ]
