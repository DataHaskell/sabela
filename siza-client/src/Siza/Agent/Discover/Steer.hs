{-# LANGUAGE OverloadedStrings #-}

{- | Construct-facet steering (R5.6, section 7.1): a miss whose target's SHAPE
is a value-of-type hunt (@LegendPos@, @defaultPlot@) is steered to the shipped
@mode="construct"@ facet. Keyed on name shape alone, never a library name.
-}
module Siza.Agent.Discover.Steer (
    constructSteer,
    goalTypeOf,
    producerPrefixes,
    steerFor,
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

{- | The rung-2 steering clause for a value-of-type miss: names the exact
shipped call (query + @mode="construct"@) so a literal-minded caller can
follow it verbatim.
-}
constructSteer :: Text -> Maybe Text
constructSteer name = steerFor <$> goalTypeOf name

-- | The steering clause for a KNOWN goal type (the standing-goal path).
steerFor :: Text -> Text
steerFor t =
    " For a value of type "
        <> t
        <> ", call discover with query \""
        <> t
        <> "\" and mode=\"construct\" — it ranks ready-made producers of "
        <> t
        <> " first."
