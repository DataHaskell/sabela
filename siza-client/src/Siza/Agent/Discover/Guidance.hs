module Siza.Agent.Discover.Guidance (
    actionNext,
    actionNextCap,
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
    MatchKind (..),
    NotebookEnv (..),
    Scope (..),
    SourceAnswer (..),
 )

{- | An install step, offered only when the top hit is what the caller asked
for. Read off a merely-similar top hit it advises installing a package for a
name the notebook may already hold, which is advice against interest.
-}
actionNext :: [DHit] -> Maybe Text
actionNext hits = case hits of
    (h : _) | derivable h -> fmap (fit h) (installStep h)
    _ -> Nothing
  where
    derivable h = dhKind h `elem` [MkExact, MkModule]
    fit h step
        | T.length full <= actionNextCap = full
        | otherwise = step
      where
        full = step <> " (top hit `" <> dhName h <> "`, " <> why h <> ")"
    why h
        | dhKind h == MkExact = "an exact name match"
        | otherwise = "the module asked for"
    installStep h = case (dhInstall h, dhCabal h) of
        (InstHidden, Just c) ->
            Just ("installed, not loaded — make a cell's first line: " <> c)
        (InstAbsentKnown, Just c) ->
            Just ("not installed, on Hackage — make a cell's first line: " <> c)
        _ -> Nothing

{- | The characters a next step may spend. An install line longer than this on
its own is emitted whole: truncating it would leave a line that does not build.
-}
actionNextCap :: Int
actionNextCap = 160

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
                <> ", so the compiler is the authority on what it exports: \
                   \probe with `try` (nothing commits, and a typed hole such \
                   \as `_ :: <goal type>` reports what fits). Do not guess a \
                   \package for a name this module may already provide."
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
    inventoryLine =
        "For 'what is available for a topic', call discover with \
        \mode=\"inventory\" and no query."

nearestNames :: NotebookEnv -> Text -> [Text]
nearestNames env q =
    take 3 . map fst . sortOn snd $
        [ (n, d)
        | n <- nub (neBuiltins env ++ neBindings env)
        , let d = editDistance (T.toLower q) (T.toLower n)
        , d <= 2
        ]

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
