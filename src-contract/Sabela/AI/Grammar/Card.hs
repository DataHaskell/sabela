{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Grammar.Card (
    cardHasBody,
    cardSigNames,
    emittableCard,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Grammar.Synth (exclusivityViolations)

cardSigNames :: Text -> [Text]
cardSigNames card =
    [ lastSeg (T.strip name)
    | l <- T.lines card
    , let (name, rest) = T.breakOn " :: " l
    , not (T.null rest)
    , not (T.null (T.strip name))
    ]

cardHasBody :: Text -> Bool
cardHasBody = not . null . cardSigNames

emittableCard :: (Text -> Bool) -> Text -> Bool
emittableCard verified card =
    cardHasBody card
        && all verified (cardSigNames card)
        && null (exclusivityViolations card)

lastSeg :: Text -> Text
lastSeg = last . T.splitOn "."
