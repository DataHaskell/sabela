{-# LANGUAGE OverloadedStrings #-}

{- | The card-emission invariant (9.1, R5.8\/R9.7): useful-or-absent — a card
emits iff its body is non-empty AND every listed name is verified (browse,
clean @check_type@, landed compile) AND the framing is descriptive.
-}
module Sabela.AI.Grammar.Card (
    cardHasBody,
    cardSigNames,
    emittableCard,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Grammar.Synth (exclusivityViolations)

-- | The qualifier-free name of each @name :: type@ line a card shows.
cardSigNames :: Text -> [Text]
cardSigNames card =
    [ lastSeg (T.strip name)
    | l <- T.lines card
    , let (name, rest) = T.breakOn " :: " l
    , not (T.null rest)
    , not (T.null (T.strip name))
    ]

-- | A card with at least one signature line has a body worth an envelope.
cardHasBody :: Text -> Bool
cardHasBody = not . null . cardSigNames

{- | The emission gate: body non-empty, every listed name verified by the
given evidence predicate, and no exclusivity framing.
-}
emittableCard :: (Text -> Bool) -> Text -> Bool
emittableCard verified card =
    cardHasBody card
        && all verified (cardSigNames card)
        && null (exclusivityViolations card)

lastSeg :: Text -> Text
lastSeg = last . T.splitOn "."
