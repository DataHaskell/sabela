{-# LANGUAGE OverloadedStrings #-}

{- | Escalation in kind for the act nudge (search-api.md 8.1, R8-T3): a nudge
whose predecessor produced no write never repeats — rung 2+ carries the
typed-hole candidate cell, propose-and-compile with the compiler as verifier.
-}
module Siza.Agent.Loop.Escalate (
    escalateNudge,
    forceWriteMsgWith,
) where

import Data.Aeson (Value, object, (.=))
import Data.IORef (IORef, atomicModifyIORef')
import Data.Text (Text)

import Siza.Agent.Discover.Candidate (candidateClauseFrom)
import Siza.Agent.Loop.Support (factsBlock, forceActMsgWith)

{- | Rung 1 echoes the ranked held facts; rung 2+ hands over the candidate,
seeded from the model's own most recent draft when one is held (R3.8) — the
seed nearest the proposer, never a list-order accident.
-}
escalateNudge :: IORef Int -> Maybe Text -> [Text] -> Text -> IO Value
escalateNudge ref mDraft facts remaining = do
    n <- atomicModifyIORef' ref (\k -> (k + 1, k + 1))
    pure $
        if n <= 1
            then forceActMsgWith facts remaining
            else forceWriteMsgWith mDraft facts remaining

{- | The rung-2+ nudge: a candidate write whose typed holes make the compiler
enumerate producers — the cost of trying is one turn and the failure output
is itself the answer. Never search advice (R5.7).

The candidate clause rides on its OWN blank-separated block so the emit ledger
('Siza.Agent.EmitLedger') dedups a byte-identical re-emission to a
back-reference — the volatile budget preamble no longer glues into it (R9-T3).

The candidate is seeded from the model's own most recent draft (R3.8) when one
is held, so the nudge hands back the proposer's closest source, not a
list-order record stub.
-}
forceWriteMsgWith :: Maybe Text -> [Text] -> Text -> Value
forceWriteMsgWith mDraft facts remaining =
    object
        [ "role" .= ("user" :: Text)
        , "content"
            .= ( "Repetition has not produced a write. "
                    <> remaining
                    <> "\n\n"
                    <> body
                    <> factsBlock facts
               )
        ]
  where
    body = case candidateClauseFrom mDraft facts of
        "" ->
            "Make one write now (insert_cell / replace_cell_source), or \
            \state the blocker plainly."
        clause -> clause
