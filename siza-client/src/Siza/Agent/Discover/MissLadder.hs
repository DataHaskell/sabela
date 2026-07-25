{-# LANGUAGE OverloadedStrings #-}

{- | The miss-escalation ladder rungs (search-api.md 8.1-8.3, R5.6): tried
shapes stripped, held facts and steer by rung 2, denial-legal give-up plus
the goal-gated typed-hole candidate at rung 3+. Split out of Advice.
-}
module Siza.Agent.Discover.MissLadder (
    attachTypeSteer,
    missAdvice,
    withCandidate,
) where

import Data.Aeson (Value)
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Advice (
    duplicateEnvelope,
    factsClause,
    resolvedTarget,
    setField,
    setNext,
    stripTried,
    tShow,
    topText,
 )
import Siza.Agent.Discover.Candidate (candidateClauseAgainst)
import Siza.Agent.Discover.Closure (giveUpLine, heldHitLine)
import Siza.Agent.Discover.Envelope (envelopeCharBudget, envelopeChars)
import Siza.Agent.Discover.Steer (constructSteer, steerFor, typeQuestionSteer)

{- | Rewrite a miss envelope for rung @n@ of the ladder (R5.6). Give-up rungs
pass denial legality (8.2): @bestHeld@ leads the close, @consulted@ names the
sources when nothing was held; @mGoal@'s steer overrides the spelled one.
-}
missAdvice ::
    Maybe Text ->
    Set Text ->
    Set Text ->
    [Text] ->
    Maybe Value ->
    [Text] ->
    Int ->
    Text ->
    Value ->
    Value
missAdvice mGoal tried refuted facts bestHeld consulted n qn v
    | n <= 1 = setNext (stripTried tried (topText "next" v)) v
    | n == 2 =
        setNext
            ( stripTried tried (topText "next" v)
                <> " Already held"
                <> factsClause facts
                <> "."
                <> steerClause
            )
            v
    | n == 3 =
        preferWithin (setNext (giveUp <> candidateNext) v) (setNext giveUp v)
    | otherwise =
        attachGate . duplicateEnvelope qn ("miss " <> tShow n) $
            maybe "" (\h -> "you already hold: " <> heldHitLine h <> "; ") bestHeld
                <> "still no match; act on held facts"
                <> factsClause facts
                <> ", or state the blocker."
  where
    -- Rung 2 only: post-close and give-up rungs never advise more search
    -- (R5.7), so the construct steer is confined to the pre-close ladder.
    steerClause = case mGoal of
        Just g -> steerFor g
        Nothing -> fromMaybe "" (constructSteer (resolvedTarget v qn))
    giveUp =
        giveUpLine bestHeld consulted
            <> " Act on what is held"
            <> factsClause facts
            <> " — or state the blocker plainly."
    -- The section 8.3 hard gate: a goal-bearing cluster's give-up rungs
    -- carry the typed-hole candidate; goalless clusters stay terse.
    candidateNext = case (mGoal, candidateClauseAgainst refuted Nothing facts) of
        (Just _, c) | not (T.null c) -> " " <> c
        _ -> ""
    attachGate
        | isJust mGoal = withCandidate refuted facts
        | otherwise = id

{- | Teach the type question once several DISTINCT names have missed. Keyed on
distinct clusters rather than the per-cluster rung, because three different
concept words each sit at rung 1 and the ladder never escalates (live_test20).
Suppressed post-close, where no further searching is advised at all (R5.7).
-}
attachTypeSteer :: Bool -> Value -> Value
attachTypeSteer False v = v
attachTypeSteer True v =
    preferWithin (setNext (topText "next" v <> typeQuestionSteer) v) v

{- | Attach the section 8.1 typed-hole candidate to a close\/gate envelope
under @next@ — a load-bearing key: never clamped, never deduped (section 10).
The R3.9 budget still binds: an unattachable candidate is dropped whole.
-}
withCandidate :: Set Text -> [Text] -> Value -> Value
withCandidate refuted facts v = case candidateClauseAgainst refuted Nothing facts of
    "" -> v
    c -> preferWithin (setField "next" c v) v

-- | Prefer the candidate-carrying form while it fits the R3.9 budget.
preferWithin :: Value -> Value -> Value
preferWithin rich plain
    | envelopeChars rich <= envelopeCharBudget = rich
    | otherwise = plain
