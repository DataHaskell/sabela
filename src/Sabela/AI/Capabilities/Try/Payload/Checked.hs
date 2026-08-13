{-# LANGUAGE OverloadedStrings #-}

{- | Which property a disposable trial proved about a candidate, read off the
trial's own record, and how far that finding may be read given what the
payload carrying it settles beside it.
-}
module Sabela.AI.Capabilities.Try.Payload.Checked (
    RunRecord (..),
    checkedNotes,
    compiledNotRunNote,
    runRecordOf,
    runRecordNote,
) where

import Data.Aeson (Result (..), Value (..), fromJSON)
import Data.Text (Text)
import Sabela.AI.CellResult (CellOutcome (..), CellResult (..))

import Sabela.AI.Capabilities.Try.Tier (disposableEvaluated)
import Sabela.Session.Materialize (CandidateSpec, DisposableResult)

{- | The disclosure a trial owes about the property it settled. Emitted only
when the record says the candidate was not run; a record that settles nothing
says nothing. The candidate's own text is never read, so no call it makes can
strengthen or weaken the claim.
-}
checkedNotes :: CandidateSpec -> DisposableResult -> [Text]
checkedNotes spec result = case disposableEvaluated spec result of
    Just False -> [compiledNotRunNote]
    _ -> []

{- | The gate's finding, scoped to the gate. It says what the disposable trial
did and stops there; what running the candidate does is settled, or left open,
by the payload this finding ships in, never by this sentence.
-}
compiledNotRunNote :: Text
compiledNotRunNote =
    "The compile gate compiled this candidate in a disposable session and did \
    \not run it there."

{- | What the payload carrying the gate's finding settles about running the
candidate. Computed from that payload, never assumed by the gate.
-}
data RunRecord = RunNotAttempted | RunUnderway | RunRecorded
    deriving (Eq, Show)

{- | A payload's execution field, read as a run record: null and a Deferred
summary are both a cell nothing ran; anything else is the record of a run.
-}
runRecordOf :: Value -> RunRecord
runRecordOf Null = RunNotAttempted
runRecordOf v = case fromJSON v of
    Success cr | crOutcome cr == Deferred -> RunNotAttempted
    _ -> RunRecorded

{- | What is left open once the gate's finding is scoped to the gate. Each
answer is a different sentence, so no payload can be read as claiming the
candidate is unrun when the same payload reports a run of it.
-}
runRecordNote :: RunRecord -> Text
runRecordNote RunNotAttempted =
    "Nothing has run it, so whether running it finishes, and whether it \
    \leaves the kernel alive, is unchecked."
runRecordNote RunUnderway =
    "It is running now: whether that finishes, and whether it leaves the \
    \kernel alive, is not settled yet."
runRecordNote RunRecorded =
    "Running it is not the gate's finding; this write ran the cell after \
    \committing it and reports that run beside this one."
