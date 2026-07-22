{-# LANGUAGE OverloadedStrings #-}

{- | The compile-ready typed-hole candidate cell (search-api.md 8.1/8.3),
synthesised from ledger-held facts ONLY: cabal line, import, and the consumer
applied to its arguments — each literal-constructible slot filled with a
canonical literal, each genuine gap left a @(_ :: T)@ hole the compiler
enumerates producers for (R3.10); nothing invented.

The seed is re-ranked by proximity to the proposer (R9-T3): the model's own most
recent writable draft when one is held (generator input, never a fact), else the
held consumer minimising its genuine-gap count — the argument types no held fact
produces (fewest holes wins, so 1-hole @bars@ beats an 8-hole record stub).
-}
module Siza.Agent.Discover.Candidate (
    candidateCell,
    candidateCellFrom,
    candidateClause,
    candidateClauseFrom,
    candidateNames,
    writableDraft,
) where

import Data.List (minimumBy)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Goal (argTypesOf, genuineGaps, literalFill)
import Siza.Agent.Discover.Ledger (installFactKey)

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

{- | A model draft is a usable candidate seed when it is non-blank: it is mined
as generator input verbatim, never trusted as a fact, so the only vet the
harness applies is that there is source to hand back.
-}
writableDraft :: Text -> Bool
writableDraft = not . T.null . T.strip

{- | The candidate source: the held cabal line (when one is held), the seed
consumer's import, and the consumer applied to its arguments (literals filled,
genuine gaps holed). 'Nothing' without a held consumer signature.
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
    let args = map fillArg (argTypesOf sig)
        importLine = ["import " <> m | not (T.null m)]
        cabalLine = maybeToList (cabalFor pkg facts)
    pure
        ( T.intercalate
            "\n"
            (cabalLine ++ importLine ++ [T.unwords (name : args)])
        )

{- | An argument slot: a canonical literal when the type is constructible from
literals already writable in a cell, else a typed hole so the compiler
enumerates that gap's producers (R3.10).
-}
fillArg :: Text -> Text
fillArg t = fromMaybe ("(_ :: " <> t <> ")") (literalFill t)

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
candidateClauseFrom mDraft facts = case candidateCellFrom mDraft facts of
    Nothing -> ""
    Just src -> framing src <> src
  where
    -- A holed synthesis leans on the compiler's hole-fit reply; a hole-free
    -- source (a draft, or an all-literal synthesis) is just re-submitted.
    framing src
        | "(_ :: " `T.isInfixOf` src =
            "Write this candidate cell verbatim (insert_cell); the compiler's \
            \hole-fit reply lists the producers of each `_ :: T` hole:\n"
        | otherwise =
            "Write this candidate cell verbatim (insert_cell) — it is the \
            \closest source to the deliverable:\n"
