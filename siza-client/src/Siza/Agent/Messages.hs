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
        <> " that you wrote still have errors. replace_cell_source edits a \
           \committed cell."
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
    verifyHeadline cls <> " " <> diagnosis
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

{- | Headlines for the channel a FAILED check is reported on. None of them may
say whether the check ran: the routing already settled that, and two of these
used to deny it on a channel only a run check reaches.
-}
verifyHeadline :: VerifyFailure -> Text
verifyHeadline NoDeliverable =
    "The task is not done: no deliverable cell exists yet."
verifyHeadline (DeliverableUndefined _) =
    "The task is not done: the deliverable is not defined in any cell."
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
  where
    diagnosis
        | ownedCount == 0 =
            "You have written no cell yet — the deliverable must be DEFINED in \
            \a cell. Write it now with insert_cell."
        | not (null missing) =
            T.intercalate ", " (map tick missing)
                <> " is not defined in any cell. Define it with insert_cell or \
                   \replace_cell_source."
        | Just g <- guidance = "The reason: " <> g <> "."
        | otherwise =
            "Make the deliverable's value observable (print it or bind it in \
            \a cell), so the check can reach a verdict."
    tick n = "`" <> n <> "`"

{- | The evidence an ok verdict rests on: the check that ran, and the cells
this turn committed. The check ran against the whole live notebook, so the
cells are named as what was written, not as what the check read.
-}
doneSignal :: [CellId] -> Text -> Text
doneSignal cids check =
    "The covering check `"
        <> check
        <> "` ran against the live notebook and passed."
        <> cellPhrase cids

cellPhrase :: [CellId] -> Text
cellPhrase [] = ""
cellPhrase cids =
    " Cell(s) "
        <> T.pack (intercalate ", " (map show cids))
        <> " were committed this episode."

doneSignalMsg :: [CellId] -> Text -> Value
doneSignalMsg cids check = verifyChannel VerdictOk (doneSignal cids check)

-- | No verdict was reached, and why. Never rendered as a confirmation.
noCheckSignal :: Maybe Text -> Text
noCheckSignal mWhy =
    "No covering check reached a verdict this turn, so nothing was verified"
        <> maybe "" (" — " <>) mWhy
        <> "."

noCheckSignalMsg :: Maybe Text -> Value
noCheckSignalMsg mWhy = verifyChannel VerdictCouldNotRun (noCheckSignal mWhy)

{- | Verdicts the harness computed itself. They are stamped with the gate that
produced them, never with the name of a tool the model did not call.
-}
verifyChannel :: VerdictClass -> Text -> Value
verifyChannel verdict body =
    object
        [ "role" .= ("tool" :: Text)
        , "tool_name" .= ("health_gate" :: Text)
        , "content" .= (verdictMarker verdict <> " " <> body)
        ]
