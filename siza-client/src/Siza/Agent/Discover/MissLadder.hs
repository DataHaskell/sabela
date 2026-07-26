{-# LANGUAGE OverloadedStrings #-}

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

withCandidate :: Set Text -> [Text] -> Value -> Value
withCandidate _ _ v = v
