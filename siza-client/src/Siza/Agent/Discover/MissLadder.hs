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

import Siza.Agent.Discover.Advice (
    duplicateEnvelope,
    factsClause,
    scopedFacts,
    setNext,
    stripTried,
    tShow,
    topText,
 )
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
    | n <= 1 = Advise (setNext next0 v)
    | n == 2 =
        escalatable (setNext (next0 <> " Already held" <> heldClause <> ".") v)
    | n == 3 = escalatable (setNext record v)
    | otherwise = Advise (duplicateEnvelope qn ("miss " <> tShow n) record held)
  where
    next0 = stripTried tried (topText "next" v)
    heldClause = factsClause (scopedFacts qn facts)
    record = giveUpLine bestHeld consulted <> " Already held" <> heldClause <> "."
    escalatable out = maybe (Advise out) (`EscalateType` out) mGoal

{- | The hard stop states what is held under the scope of the question it is
answering, so a fact the caller's scope excludes is not replayed here either.
With no question to scope by it states nothing.
-}
withCandidate :: Map Text Text -> [Text] -> Value -> Value
withCandidate _refuted facts v = case topText "query" v of
    "" -> v
    qn -> setNext ("Already held" <> factsClause (scopedFacts qn facts) <> ".") v
