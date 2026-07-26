{-# LANGUAGE OverloadedStrings #-}

{- | What a repeated miss reports back: the shapes already tried, the facts
already held, the sources already consulted. A RECORD, not a ladder.

The rungs used to escalate into instructions — steer to another facet by
rung 2, give up and act by rung 3, hand over a harness-authored candidate by
rung 3+. Every one of those rests on an inference the harness cannot make:
that the search so far was sufficient, that a miss streak means the question
is malformed rather than the hunt hard, that facts held add up to the goal.
When they were wrong they were expensively wrong (the prescribed-tidal
class). The truthful part — you asked this before, here is what came back —
is kept.
-}
module Siza.Agent.Discover.MissLadder (
    missAdvice,
    withCandidate,
) where

import Data.Aeson (Value)
import Data.Set (Set)
import Data.Text (Text)

import Siza.Agent.Discover.Advice (
    duplicateEnvelope,
    factsClause,
    setNext,
    stripTried,
    tShow,
    topText,
 )
import Siza.Agent.Discover.Closure (giveUpLine)

{- | The miss record for rung @n@: the backend's own @next@ minus shapes
already tried, then the facts already held, then the held hit or the sources
consulted. No advice, no candidate, no verdict on whether to keep searching.
-}
missAdvice ::
    Set Text ->
    [Text] ->
    Maybe Value ->
    [Text] ->
    Int ->
    Text ->
    Value ->
    Value
missAdvice tried facts bestHeld consulted n qn v
    | n <= 1 = setNext next0 v
    | n == 2 = setNext (next0 <> " Already held" <> factsClause facts <> ".") v
    | n == 3 = setNext record v
    | otherwise = duplicateEnvelope qn ("miss " <> tShow n) record
  where
    next0 = stripTried tried (topText "next" v)
    record = giveUpLine bestHeld consulted <> " Already held" <> factsClause facts <> "."

{- | Formerly attached a harness-authored candidate to a close\/gate
envelope. It now attaches nothing: see 'missAdvice' for why the harness does
not author code. Kept as the identity so the ledger's call sites stay
readable while the surrounding advice layer is retired.
-}
withCandidate :: Set Text -> [Text] -> Value -> Value
withCandidate _ _ v = v
