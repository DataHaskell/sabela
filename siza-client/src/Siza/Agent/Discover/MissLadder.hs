{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Discover.MissLadder (
    MissOutcome (..),
    missAdvice,
    withCandidate,
) where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Advice (
    duplicateEnvelope,
    factsClause,
    hitsOf,
    scopedFacts,
    setNext,
    stripTried,
    tShow,
    topText,
 )
import Siza.Agent.Discover.Beside (restatesHit)
import Siza.Agent.Discover.Closure (giveUpLine)
import Siza.Agent.Discover.Types (StandingGoal)

{- | What a rung decided. 'EscalateType' carries the same payload 'Advise'
would have carried: the rung asks for a type query, it does not claim one ran.
-}
data MissOutcome
    = Advise Value
    | EscalateType StandingGoal Value
    deriving (Eq, Show)

{- | The rungs of a repeated miss. The last one stops re-consulting backends
and scopes the held facts to the question it answers; the middle rungs are
where a still-unsatisfied goal is worth re-asking by type.
-}
missAdvice ::
    [Value] ->
    Set Text ->
    [Text] ->
    Maybe Value ->
    [Text] ->
    Maybe StandingGoal ->
    Int ->
    Text ->
    Value ->
    MissOutcome
missAdvice held tried facts bestHeld consulted mGoal n qn v
    | n <= 1 = Advise (advise next0 v)
    | n == 2 = escalatable (advise (sentences [next0, heldProse]) v)
    | n == 3 = escalatable (advise record v)
    | otherwise = Advise (duplicateEnvelope qn ("miss " <> tShow n) record held)
  where
    carried = hitsOf v
    next0 = stripTried tried (topText "next" v)
    heldProse = heldNotCarried carried qn facts
    record = sentences [giveUpLine carried bestHeld consulted, heldProse]
    escalatable out = maybe (Advise out) (`EscalateType` out) mGoal

sentences :: [Text] -> Text
sentences = T.unwords . filter (not . T.null)

{- | Advice is written only when there is any: an empty next is a field the
reader parses to learn nothing, and it would overwrite the step already there.
-}
advise :: Text -> Value -> Value
advise t v
    | T.null t = v
    | otherwise = setNext t v

{- | What is held that the answer's own hits do not already state. A fact
those hits carry is stated by them, in fields; repeating it in prose is a
second copy of the payload. With nothing held at all, the clause says so.
-}
heldNotCarried :: [Value] -> Text -> [Text] -> Text
heldNotCarried carried qn facts
    | null scoped = clause []
    | null kept = ""
    | otherwise = clause kept
  where
    scoped = scopedFacts qn facts
    kept = filter (not . restatesHit carried) scoped
    clause fs = "Already held" <> factsClause fs <> "."

{- | The hard stop states what is held under the scope of the question it is
answering, so a fact the caller's scope excludes is not replayed here either.
With no question to scope by it states nothing.
-}
withCandidate :: Map Text Text -> [Text] -> Value -> Value
withCandidate _refuted facts v = case topText "query" v of
    "" -> v
    qn -> advise (heldNotCarried (hitsOf v) qn facts) v
