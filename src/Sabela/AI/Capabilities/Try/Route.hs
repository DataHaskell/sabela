{-# LANGUAGE OverloadedStrings #-}

{- | What the live probe's answer means. Separated from the IO around it so the
one question that decides whether a candidate is answered here or escalated to
the contained session is a pure function of the probe's result.
-}
module Sabela.AI.Capabilities.Try.Route (
    LiveDecision (..),
    liveDecision,
    isUnrestrictedIO,
) where

import qualified Data.Text as T

import Sabela.AI.Capabilities.Try.Payload (
    invariantPayload,
    purePayload,
    unshowablePayload,
 )
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import qualified Sabela.SessionTypes as ST

{- | Either the live probe answered the question, or it could not and the
contained session must. Escalation is not a verdict on the candidate.
-}
data LiveDecision
    = LiveAnswer ToolOutcome
    | LiveEscalateContained
    deriving (Eq, Show)

liveDecision :: Bool -> ST.PureEvalResult -> LiveDecision
liveDecision invariant result = case ST.pureEvalVerdict result of
    ST.PureEvalUnshowable
        | invariant -> LiveAnswer (okOutcome (unshowablePayload result))
        | otherwise -> changed "live state changed during try"
    ST.PureEvalSucceeded
        | invariant -> LiveAnswer (okOutcome (purePayload result))
        | otherwise -> changed "live state changed during try"
    ST.PureEvalRejected
        | isUnrestrictedIO result -> LiveEscalateContained
        | invariant -> LiveAnswer (errOutcome (purePayload result))
        | otherwise -> changed "live state changed while checking try"
    _ -> LiveAnswer (errOutcome (purePayload result))
  where
    changed = LiveAnswer . errOutcome . invariantPayload

isUnrestrictedIO :: ST.PureEvalResult -> Bool
isUnrestrictedIO result =
    let lower = T.toLower (ST.pureEvalError result)
     in "scratch candidate is io" `T.isInfixOf` lower
            || "sabela_unrestricted_io" `T.isInfixOf` lower
