{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.NoteLedger (
    askConsistent,
    assertedLive,
    declaredCellCount,
    noteLedgerOk,
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Deliverable (backtickSegments, isIdentName, requestedNames)

assertionPhrases :: [Text]
assertionPhrases =
    [ "is already"
    , "already loaded"
    , "in scope"
    , "is loaded"
    , "is defined"
    , "is installed"
    , "ran successfully"
    , "is live"
    ]

assertedLive :: Text -> [Text]
assertedLive note =
    [ name
    | s <- sentences note
    , any (`T.isInfixOf` s) assertionPhrases
    , name <- filter isIdentName (backtickSegments s)
    ]

noteLedgerOk :: Set Text -> Text -> Bool
noteLedgerOk live note = all (`Set.member` live) (assertedLive note)

askConsistent :: Set Text -> Text -> Text -> Bool
askConsistent verified prompt note =
    all verifiedIfAsserted (requestedNames prompt)
        && (not (singularAsk note) || outstanding <= 1)
  where
    asserted = assertedLive note
    verifiedIfAsserted n = n `notElem` asserted || n `Set.member` verified
    outstanding =
        declaredCellCount prompt
            - length (filter (`Set.member` verified) (requestedNames prompt))

declaredCellCount :: Text -> Int
declaredCellCount prompt =
    maximum [1, length (requestedNames prompt), ordinals]
  where
    low = T.toLower prompt
    ordinals = length (filter (`T.isInfixOf` low) ordinalCellPhrases)

ordinalCellPhrases :: [Text]
ordinalCellPhrases =
    ["in one cell", "in a second cell", "in a third cell", "in a fourth cell"]

singularAsk :: Text -> Bool
singularAsk = ("write the cell " `T.isInfixOf`) . T.toLower

sentences :: Text -> [Text]
sentences t = concatMap (T.splitOn ". ") (T.lines t)
