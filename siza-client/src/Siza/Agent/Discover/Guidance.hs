module Siza.Agent.Discover.Guidance (
    absentScopePackage,
    actionNext,
    actionNextCap,
    missNext,
    nearestNames,
    editDistance,
    cabalLine,
    readSourceCall,
) where

import Control.Applicative ((<|>))
import Data.List (nub, sortOn)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.ReadSourceArgs (readSourceCallText)
import Siza.Agent.Discover.CabalFacts (PkgFacts (..))
import Siza.Agent.Discover.ModuleList (entryModule, shownModules)
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

tShow :: Int -> Text
tShow = T.pack . show

-- | The call that shows a module's released source, as the caller types it.
readSourceCall :: Text -> Text
readSourceCall m = readSourceCallText [("module", m)]

{- | The peek names a module, not the package, because read_source takes a
module; a package with several roots gets no minted module name.
-}
sourcePeek :: Scope -> HackageInfo -> Text -> Text
sourcePeek scope hk p =
    case scModule scope <|> entryModule (lookup p (hiFacts hk)) of
        Just m ->
            " "
                <> readSourceCall m
                <> " shows its released source without installing."
        Nothing -> ""

{- | The package a scope names that no local index covers, by package or by
module. The indexes hold installed packages, so their silence about an absent
one is the reach of the index, never the package lacking the name.
-}
absentScopePackage :: Scope -> [SourceAnswer] -> HackageInfo -> Maybe Text
absentScopePackage scope answers hk = byPackage <|> byModule
  where
    {- Only the Hoogle channel fills saPkgModules, so reading coverage from it
    alone calls a package absent whenever that one channel did not bucket it —
    while the hits beside the note say installed. The hits are the authority. -}
    covered =
        map fst (concatMap saPkgModules answers)
            ++ [ dhPackage h
               | a <- answers
               , h <- saHits a
               , dhInstall h `notElem` [InstAbsentKnown, InstAbsentUnknown]
               ]
    uncovered p = p `notElem` covered
    byPackage = do
        p <- scPackage scope
        if uncovered p && p `elem` map fst (hiFacts hk) then Just p else Nothing
    byModule = do
        m <- scModule scope
        listToMaybe [p | (p, f) <- hiFacts hk, uncovered p, exposes m f]
    exposes m f =
        any (\x -> x == m || (m <> ".") `T.isPrefixOf` x) (pfModules f)

{- | An install step, offered only when the top hit is what the caller asked
for. Read off a merely-similar top hit it advises installing a package for a
name the notebook may already hold, which is advice against interest.
-}
actionNext :: [DHit] -> Maybe Text
actionNext hits = case hits of
    (h : _) | derivable h -> fmap (withRest h . fit h) (installStep h)
    _ -> Nothing
  where
    derivable h = dhKind h `elem` [MkExact, MkModule]
    withRest h step = step <> restOfModules h
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

{- | The modules a bounded hit did not show, and the scope that lists them. A
count with no way to reach the rest reads as a wall.
-}
restOfModules :: DHit -> Text
restOfModules h = case dhFacts h of
    Just f
        | rest > 0 ->
            " ("
                <> tShow rest
                <> " more modules: discover {package=\""
                <> dhPackage h
                <> "\"})"
      where
        rest = length (pfModules f) - length (shownModules f)
    _ -> ""

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
    {- Naming the indexes as consulted implies they could have answered. For a
    package none of them covers they could not, and the caller who reads it as
    "no such name" varies the query instead of installing the package. -}
    scopeLine = case absentScopePackage scope answers hk of
        Just p ->
            "No installed index covers "
                <> p
                <> ", so none of them can state whether it has '"
                <> iName interp
                <> "'. "
                <> cabalLine p
                <> " installs it, after which the session answers for it."
                <> sourcePeek scope hk p
        Nothing ->
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
    {- Inventory lists candidate packages and states no signature, so it is
    offered for a topic only; named for a missed NAME it sends the caller to a
    mode that cannot answer them. Its query must be non-blank, or it is refused. -}
    inventoryLine
        | iShape interp /= "prose" = ""
        | Just _ <- absentScopePackage scope answers hk = ""
        | otherwise =
            "For 'what is available for a topic', call discover {mode=\
            \\"inventory\", "
                <> inventoryArg
                <> "}."
    inventoryArg = case (scPackage scope, scModule scope) of
        (Just p, _) -> "package=\"" <> p <> "\""
        (_, Just m) -> "module=\"" <> m <> "\""
        _ -> "query=\"" <> iName interp <> "\""

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
