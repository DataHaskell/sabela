{- | The discover feedback protocol (docs/discover/search-api.md section 8):
every miss moves the caller closer to acting — consulted scope, disclosed
incompleteness, nearest held names — and a found answer that still needs an
action states it once. No retry boilerplate, no invent ban (R5.8).
-}
module Siza.Agent.Discover.Guidance (
    actionNext,
    missNext,
    nearestNames,
    editDistance,
    cabalLine,
) where

import Data.List (nub, sortOn)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    Interpreted (..),
    NotebookEnv (..),
    Scope (..),
    SourceAnswer (..),
 )

-- | The action a found answer still needs (expose or install), stated once.
actionNext :: [DHit] -> Maybe Text
actionNext hits = case hits of
    (h : _)
        | dhInstall h == InstHidden
        , Just c <- dhCabal h ->
            Just
                ( "installed but hidden — expose it by making a cell's first line: "
                    <> c
                )
        | dhInstall h == InstAbsentKnown
        , Just c <- dhCabal h ->
            Just
                ( "not installed; exists on Hackage — install it by making a "
                    <> "cell's first line: "
                    <> c
                    <> " (the compiler then verifies it)"
                )
    _ -> Nothing

{- | A miss that converges (R5): a miss scoped to an imported package routes
to write-and-observe FIRST (section 8, round 7 — the compiler is the one
verifier that cannot be wrong about an imported package), then consulted
scope, nearest held names, disclosed incompleteness — never retry
boilerplate, never an invent ban (R5.8).
-}
missNext ::
    NotebookEnv -> Interpreted -> Scope -> [SourceAnswer] -> HackageInfo -> Text
missNext env interp scope answers hk =
    T.intercalate
        " "
        ( filter
            (not . T.null)
            [writeObserveLine, scopeLine, downLine, nearLine, inventoryLine]
        )
  where
    importTargets = map snd (neAliases env) ++ neImports env
    importedScope =
        listToMaybe
            ( [m | Just m <- [iScope interp], m `elem` importTargets]
                ++ [m | Just m <- [scModule scope], m `elem` importTargets]
            )
    writeObserveLine = case importedScope of
        Nothing -> ""
        Just m ->
            "The notebook imports "
                <> m
                <> aliasOf m
                <> "; if "
                <> iName interp
                <> " is real the compiler will accept it — write the cell \
                   \and observe; a red cell is one replace away."
    aliasOf m = case [a | (a, m') <- neAliases env, m' == m] of
        (a : _) -> " (as " <> a <> ")"
        [] -> ""
    consultedNames =
        [saSource a | a <- answers, saOk a] ++ ["hackage" | hiAvailable hk]
    downNames =
        [saSource a | a <- answers, not (saOk a)]
            ++ ["hackage" | not (hiAvailable hk)]
    scopeLine =
        "No match for '"
            <> iName interp
            <> "' in: "
            <> T.intercalate ", " (nub consultedNames)
            <> "."
    downLine
        | null downNames = ""
        | otherwise =
            "Search incomplete — unavailable: "
                <> T.intercalate ", " (nub downNames)
                <> "."
    nearNames = nearestNames env (iName interp)
    nearLine = case nearNames of
        [] -> ""
        ns -> "Nearest held names: " <> T.intercalate ", " ns <> "."
    -- The R5.3 clean-miss pointer: with no neighbours, redirect the search to
    -- the inventory question instead of padding with substring noise.
    inventoryLine
        | null nearNames =
            "For 'what is available for a topic', call discover with \
            \mode=\"inventory\"."
        | otherwise = ""

-- | Up to three environment names within edit distance 2 (R5.1).
nearestNames :: NotebookEnv -> Text -> [Text]
nearestNames env q =
    take 3 . map fst . sortOn snd $
        [ (n, d)
        | n <- nub (neBuiltins env ++ neBindings env)
        , let d = editDistance (T.toLower q) (T.toLower n)
        , d <= 2
        ]

-- | Small Levenshtein distance (names are short; no bounds needed).
editDistance :: Text -> Text -> Int
editDistance a b = last (foldl row [0 .. length sa] (T.unpack b))
  where
    sa = T.unpack a
    row prev c = (base + 1) : cells 1 (base + 1)
      where
        base = case prev of
            (x : _) -> x
            [] -> 0
        cells i left
            | i > length sa = []
            | otherwise =
                let cost =
                        minimum
                            [ prev !! i + 1
                            , left + 1
                            , prev !! (i - 1)
                                + fromEnum (sa !! (i - 1) /= c)
                            ]
                 in cost : cells (i + 1) cost

cabalLine :: Text -> Text
cabalLine pkg = "-- cabal: build-depends: " <> pkg
