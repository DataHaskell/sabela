{- | The client-seam provenance log (redesign Part 7).

A 'SessionEvent' is not a new schema: it serialises the R2 contract types
('ToolName', 'ToolOutcome', 'KernelState', 'Diagnostic') append-only, one
JSON line per tool call. The client seam is the agent's intent layer — it is
the only seam that sees the pre-flight verdict the server never receives. The
server log under the same @sessions/@ tree is the authoritative record; the
two correlate by @(seSession, seGen)@.

This is the umbrella: the 'SessionEvent' record and its wire live in
'Siza.Provenance.Event'; the append-only file and the opt-in hash chain live in
'Siza.Provenance.Log'. Logging is best-effort — 'recordEvent' guards every
write, so a log failure can never fail the tool call it records.
-}
module Siza.Provenance (
    SessionEvent (..),
    Actor (..),
    Preflight (..),
    actorWire,
    parseActor,
    sessionLogPath,
    recordEvent,
    appendEvent,
    eventHash,
    chainEvents,
    verifyChain,
) where

import Siza.Provenance.Event (
    Actor (..),
    Preflight (..),
    SessionEvent (..),
    actorWire,
    parseActor,
 )
import Siza.Provenance.Log (
    appendEvent,
    chainEvents,
    eventHash,
    recordEvent,
    sessionLogPath,
    verifyChain,
 )
