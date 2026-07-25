{-# LANGUAGE OverloadedStrings #-}

{- | The compile-ready candidate cell (search-api.md 8.1/8.3), synthesised
from ledger-held facts ONLY: cabal line, import, and the consumer applied to
its arguments — each slot filled either with a canonical literal or with a
producer a harness hole probe established ("Sabela.AI.HoleProbe"). Nothing is
invented, and (G3) nothing incomplete is ever handed back: a slot with no
literal and no probed producer yields no candidate at all.

The seed is re-ranked by proximity to the proposer (R9-T3): the model's own
most recent writable draft when one is held (generator input, never a fact),
else the held consumer minimising its genuine-gap count.
-}
module Siza.Agent.Discover.Candidate (
    candidateCell,
    candidateCellFrom,
    candidateClause,
    candidateClauseAgainst,
    candidateClauseFrom,
    candidateGaps,
    candidateNames,
    writableDraft,
) where

import Data.List (minimumBy, nub)
import Data.Maybe (isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoleProbe (probedProducer)
import Sabela.AI.TypedHole (containsTypedHole)
import Siza.Agent.Discover.Facts (installFactKey)
import Siza.Agent.Discover.Goal (argTypesOf, genuineGaps, literalFill)
import Siza.Agent.Discover.Ledger (normaliseSource)

{- | A held consumer: name, signature, defining module, owning package —
parsed from the fact shape 'Siza.Agent.Discover.Advice.harvestFacts' emits.
-}
consumerOf :: Text -> Maybe (Text, Text, Text, Text)
consumerOf f = do
    body <- T.stripPrefix "`" f
    let (name, rest) = T.breakOn "` :: " body
    sigProv <- T.stripPrefix "` :: " rest
    let (sig0, prov) = T.breakOn " — found in " sigProv
    provBody <- T.stripPrefix " — found in " prov
    let (m, pkgPart) = T.breakOn " (" provBody
        pkg = T.takeWhile (/= ')') (T.drop 2 pkgPart)
    if T.null name || T.null (T.strip sig0)
        then Nothing
        else Just (name, T.strip sig0, T.strip m, T.strip pkg)

{- | The seed consumer: of the held consumers, the one whose signature has the
FEWEST genuine gaps (nominal argument types no held fact produces). 'minimumBy'
keeps the first on a tie, so harvest order breaks ties — never re-ordered noise.
-}
seedConsumer :: [Text] -> Maybe (Text, Text, Text, Text)
seedConsumer facts = case mapMaybe consumerOf facts of
    [] -> Nothing
    cs -> Just (minimumBy (comparing gapCount) cs)
  where
    heldSigs = [sig | (_, sig, _, _) <- mapMaybe consumerOf facts]
    gapCount (_, sig, _, _) = length (genuineGaps heldSigs sig)

-- | The held cabal line for a package, its provides tag stripped.
cabalFor :: Text -> [Text] -> Maybe Text
cabalFor pkg facts =
    listToMaybe
        [ T.strip (T.takeWhile (/= '—') rest)
        | f <- facts
        , installFactKey f == Just pkg
        , let (_, rest) = T.breakOn "-- cabal:" f
        , not (T.null rest)
        ]

{- | A model draft is a usable candidate seed when it is non-blank AND
hole-free: the harness answers holes ("Sabela.AI.HoleProbe"), it never hands
one back to be transcribed.
-}
writableDraft :: Text -> Bool
writableDraft d = not (T.null (T.strip d)) && not (containsTypedHole d)

{- | The candidate source: the held cabal line (when one is held), the seed
consumer's import, and the consumer applied to its arguments. 'Nothing'
without a held consumer signature, or when any argument slot is still a gap.
-}
candidateCell :: [Text] -> Maybe Text
candidateCell = candidateCellFrom Nothing

{- | The candidate, seeded first from the model's own most recent writable
draft (R9-T3) when one is held, else synthesised from the ranked seed consumer.
-}
candidateCellFrom :: Maybe Text -> [Text] -> Maybe Text
candidateCellFrom (Just draft) _ | writableDraft draft = Just (T.stripEnd draft)
candidateCellFrom _ facts = do
    (name, sig, m, pkg) <- seedConsumer facts
    args <- traverse (fillArg facts) (argTypesOf sig)
    let importLine = ["import " <> m | not (T.null m)]
        cabalLine = maybeToList (cabalFor pkg facts)
    pure
        ( T.intercalate
            "\n"
            (cabalLine ++ importLine ++ [T.unwords (name : args)])
        )

{- | An argument slot: a canonical literal when the type is constructible from
literals already writable in a cell, else a producer a hole probe established.
'Nothing' is a genuine gap — the harness must probe it before any candidate
can be proposed (G3).
-}
fillArg :: [Text] -> Text -> Maybe Text
fillArg facts t = case literalFill t of
    Just lit -> Just lit
    Nothing -> probedProducer facts t

{- | The distinct argument types the seed consumer still needs a producer
for: what a harness hole probe must answer before this ledger can propose
anything.
-}
candidateGaps :: [Text] -> [Text]
candidateGaps facts = case seedConsumer facts of
    Nothing -> []
    Just (_, sig, _, _) ->
        nub [t | t <- argTypesOf sig, isNothing (fillArg facts t)]

{- | The names a synthesised candidate rests on (consumer, module, package):
each must be discover-findable on the same catalogue (R7.6). A draft seed rests
on no ledger name, so it carries none.
-}
candidateNames :: [Text] -> [Text]
candidateNames facts = case seedConsumer facts of
    Nothing -> []
    Just (name, _, m, pkg) -> [x | x <- [name, m, pkg], not (T.null x)]

-- | The candidate framed for a nudge body or a close envelope's @next@.
candidateClause :: [Text] -> Text
candidateClause = candidateClauseFrom Nothing

-- | 'candidateClause' with an optional model-draft seed (R9-T3).
candidateClauseFrom :: Maybe Text -> [Text] -> Text
candidateClauseFrom = candidateClauseAgainst Set.empty

{- | 'candidateClauseFrom' that retires a source the gate already rejected
(G5.4): the compiler's verdict outranks the ledger, so a candidate it refused
is never recommended again — live_test8 re-injected one after two failures.
-}
candidateClauseAgainst :: Set Text -> Maybe Text -> [Text] -> Text
candidateClauseAgainst refuted mDraft facts =
    case candidateCellFrom mDraft facts of
        Just src
            | normaliseSource src `Set.notMember` refuted -> framing <> src
        _ -> ""
  where
    framing =
        "A candidate assembled from facts held this session. It has NOT been \
        \compiled, so treat it as a proposal, not an answer: write it with \
        \insert_cell if it fits the goal, and let the compiler decide.\n"
