{-# LANGUAGE OverloadedStrings #-}

{- | Goal classification by name SHAPE alone (section 7.1): @LegendPos@ and
@defaultPlot@ are value-of-type hunts, @col@ is not. Never keyed on a library
name. This module also held the steering text those classes fed; that advice
is gone (see "Siza.Agent.Discover.MissLadder").
-}
module Siza.Agent.Discover.Steer (
    goalTypeOf,
    producerPrefixes,
) where

import Data.Char (isAlphaNum, isLower, isUpper)
import Data.Text (Text)
import qualified Data.Text as T

{- | The goal type a missed name's shape hunts a value of: the name itself
when it is a bare upper-headed identifier, the upper-headed remainder after a
producer prefix. 'Nothing' for every other shape (qualified, prose, value).
-}
goalTypeOf :: Text -> Maybe Text
goalTypeOf name
    | not plainIdent = Nothing
    | upperHead name = Just name
    | prefix `elem` producerPrefixes && upperHead rest = Just rest
    | otherwise = Nothing
  where
    plainIdent =
        not (T.null name) && T.all (\c -> isAlphaNum c || c == '\'') name
    (prefix, rest) = T.span isLower name
    upperHead t = maybe False (isUpper . fst) (T.uncons t)

-- | The producer-shaped name prefixes of the needs-a-value hunt vocabulary.
producerPrefixes :: [Text]
producerPrefixes =
    ["default", "mk", "make", "new", "empty", "init", "initial", "create"]
