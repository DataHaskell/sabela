module Siza.Agent.Discover.Rank (
    rankKeyRecent,
    RankKey,
    demotedCount,
    fuse,
    plainness,
    rankKey,
    rankKeyIn,
    samePackageVariants,
    stratum,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.ModuleResolve (isNoiseModule)
import Siza.Agent.Discover.Types (
    DHit (..),
    InstallState (..),
    Interpreted (..),
    MatchKind (..),
    NotebookEnv (..),
 )

stratum :: NotebookEnv -> Interpreted -> DHit -> Int
stratum env interp h
    | exact && dhInstall h `elem` [InstBuiltin, InstNotebook] = 1
    | exact && aliasScoped = 1
    | exact && dhInstall h == InstInstalled = 2
    | inScope && dhKind h == MkPrefix = 3
    | exact && dhInstall h == InstHidden = 4
    | exact && dhInstall h `elem` [InstAbsentKnown, InstAbsentUnknown] = 5
    | dhKind h == MkPrefix = 6
    | dhKind h == MkType = if iShape interp == "type" then 2 else 7
    | dhKind h `elem` [MkSubstring, MkModule] = 8
    | dhKind h == MkSynonym = 9
    | otherwise = 10
  where
    exact = dhKind h == MkExact
    aliasScoped =
        dhModule h `elem` (map snd (neAliases env) ++ neImports env)
    inScope =
        dhInstall h `elem` [InstBuiltin, InstNotebook, InstInstalled]
            || aliasScoped

type RankKey =
    (Int, Int, Int, Int, Int, (Int, Int), Int, Int, Text, Text)

rankKey :: NotebookEnv -> Interpreted -> DHit -> RankKey
rankKey = rankKeyIn []

rankKeyIn :: [Text] -> NotebookEnv -> Interpreted -> DHit -> RankKey
rankKeyIn = rankKeyRecent []

rankKeyRecent ::
    [Text] -> [Text] -> NotebookEnv -> Interpreted -> DHit -> RankKey
rankKeyRecent recentPkgs importedPkgs env interp h =
    ( stratum env interp h
    , importedBand
    , fromEnum (dhInstall h)
    , if isNoiseModule (dhModule h) then 1 else 0
    , if T.null (dhType h) then 1 else 0
    , plainness (dhType h)
    , T.length (dhModule h)
    , T.length (dhPackage h)
    , dhModule h
    , dhName h
    )
  where
    -- An unresolved package is not evidence of irrelevance: banding a
    -- package-less session hit with packages nothing implicates handed the
    -- tie to `plainness`, and the unconstrained namesake won it.
    importedBand
        | dhInstall h `elem` [InstBuiltin, InstNotebook] = 0
        | dhModule h `elem` (map snd (neAliases env) ++ neImports env) = 0
        | not (T.null (dhPackage h)), dhPackage h `elem` importedPkgs = 0
        | not (T.null (dhPackage h)), dhPackage h `elem` recentPkgs = 1
        | T.null (dhPackage h) = 2
        | otherwise = 3

plainness :: Text -> (Int, Int)
plainness ty = (constraintCount ty, typeLevelArgCount ty)

constraintCount :: Text -> Int
constraintCount ty = case T.splitOn "=>" ty of
    [_] -> 0
    segments -> sum (map atoms (init segments))
  where
    atoms seg =
        length
            [ a
            | a <- T.splitOn "," (T.filter (`notElem` ("()" :: String)) seg)
            , not (T.null (T.strip a))
            ]

typeLevelArgCount :: Text -> Int
typeLevelArgCount ty =
    length
        [w | w <- T.words ty, "@" `T.isPrefixOf` w || "'" `T.isPrefixOf` w]

demotedCount :: [DHit] -> Int
demotedCount = length . filter (isNoiseModule . dhModule)

samePackageVariants :: DHit -> DHit -> Bool
samePackageVariants a b =
    variantStub a && variantStub b && dhPackage a == dhPackage b
  where
    variantStub h =
        T.null (dhType h)
            && not (T.null (dhPackage h))
            && norm (dhName h) == norm (dhPackage h)
    norm = T.toLower . T.filter (/= '-')

fuse :: NotebookEnv -> Interpreted -> DHit -> DHit -> DHit
fuse env interp a b =
    best
        { dhType = pick dhType
        , dhPackage = pick dhPackage
        , dhVersion = pick dhVersion
        , dhCabal = dhCabal best `orElse` dhCabal other
        , dhUse = dhUse best `orElse` dhUse other
        }
  where
    (best, other) =
        if rankKey env interp a <= rankKey env interp b then (a, b) else (b, a)
    pick f = if T.null (f best) then f other else f best
    orElse (Just x) _ = Just x
    orElse Nothing y = y
