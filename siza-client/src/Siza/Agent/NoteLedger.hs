{-# LANGUAGE OverloadedStrings #-}

{- | Truthfulness checks for harness-injected sentences (search-api.md section
9.2, M16): a note may assert a binding live only when a verified event made it
so, and may never shrink the prompt's stated ask. One validator over every
injected note, applied by evidence class, never by which subsystem wrote it.
-}
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

-- | Sentence fragments that turn a backticked name into a live-state claim.
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

{- | The binding names a note asserts as live: backticked lone identifiers in a
sentence carrying an assertion phrase. Files and expressions are never claims.
-}
assertedLive :: Text -> [Text]
assertedLive note =
    [ name
    | s <- sentences note
    , any (`T.isInfixOf` s) assertionPhrases
    , name <- filter isIdentName (backtickSegments s)
    ]

-- | Ledger check: every live-state claim in the note names a live binding.
noteLedgerOk :: Set Text -> Text -> Bool
noteLedgerOk live note = all (`Set.member` live) (assertedLive note)

{- | Deliverable consistency (the M16 class): no requested deliverable is
claimed live without verification, and a singular "write the cell" directive
is illegal while more than one declared cell is outstanding.
-}
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

{- | How many separate cells the prompt itself declares: its typed
deliverables, or its ordinal "in one cell … in a second cell" phrasing.
-}
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
