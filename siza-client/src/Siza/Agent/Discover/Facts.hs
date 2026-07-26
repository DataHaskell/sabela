{-# LANGUAGE OverloadedStrings #-}

{- | The held-fact list and its one fold (search-api.md section 8): bounded
size, first-seen order, and the install-state replacement rule. Shared by the
harvest path ('Siza.Agent.Discover.Advice') and the ledger's own writers, so
every fact — harvested or probed — enters the list the same way.
-}
module Siza.Agent.Discover.Facts (
    factPackages,
    foldFacts,
    installFactKey,
    maxHeldFacts,
    compilerFact,
    compilerFactMark,
    isCompilerFact,
) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Types (InstallState, installText)

maxHeldFacts :: Int
maxHeldFacts = 8

{- | The provenance marker on a compiler-confirmed fact (G5.6), symmetric
with G3's @via: hole-probe@. One source, so the producer and the ranker
that promotes it cannot drift apart.
-}
compilerFactMark :: Text
compilerFactMark = " — confirmed by the compiler (check_type)"

{- | A green @check_type@ as a held fact. live_test9 confirmed
@Sabela.Notebook.plot :: [(Double, Double)] -> Picture@ at turn 11 and the
ledger never held it, while a lexical @plot@ card that merely shared the
name was admitted.
-}
compilerFact :: Text -> Text -> Text
compilerFact name sig = "`" <> name <> "` :: " <> sig <> compilerFactMark

-- | Does this held fact carry the compiler's own confirmation?
isCompilerFact :: Text -> Bool
isCompilerFact = T.isInfixOf compilerFactMark

{- | Fold new facts into a bounded held list. A fresh install-state fact
REPLACES the package's earlier one — one package never holds two at once.
-}
foldFacts :: [Text] -> [Text] -> [Text]
foldFacts new facts = take maxHeldFacts (foldl addFact facts new)
  where
    addFact acc f
        | f `elem` acc || T.null f = acc
        | Just p <- installFactKey f =
            [g | g <- acc, installFactKey g /= Just p] ++ [f]
        | otherwise = acc ++ [f]

{- | The package of an install-state fact (@"pkg (state): …"@ as
'Siza.Agent.Discover.Advice.harvestFacts' shapes them); 'Nothing' for any
other held fact. The world-change wipe keys its fact reset on it.
-}
installFactKey :: Text -> Maybe Text
installFactKey f = case T.words f of
    (p : st : _)
        | "(" `T.isPrefixOf` st
        , T.dropAround (`elem` ("():" :: String)) st `elem` states ->
            Just p
    _ -> Nothing
  where
    states = map installText [minBound .. maxBound :: InstallState]

{- | The packages the held facts establish, install facts and signature
provenance alike — the session's own footprint, for REFINEMENT: a later
search ranks hits from these packages ahead of equal-tier strangers, so each
search narrows rather than starting blind. Ranking only; a package absent
from this list is never filtered.
-}
factPackages :: [Text] -> [Text]
factPackages facts =
    nubKeep
        ( mapMaybe installFactKey facts
            <> mapMaybe provenancePackage facts
        )
  where
    provenancePackage f = case T.breakOn " — found in " f of
        (_, rest)
            | T.null rest -> Nothing
            | otherwise -> case T.breakOn "(" rest of
                (_, pkgPart)
                    | T.null pkgPart -> Nothing
                    | otherwise ->
                        Just (T.takeWhile (/= ')') (T.drop 1 pkgPart))
    nubKeep = foldr (\x acc -> x : filter (/= x) acc) []
