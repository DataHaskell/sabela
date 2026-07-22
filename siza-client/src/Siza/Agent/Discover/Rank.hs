{- | The discover ranking core (docs/discover/search-api.md section 7): the
total-order strata, the within-stratum key — including the internal-module
demotion, an API-visibility class judgement, never a library judgement — and
the duplicate-hit fusion. Split from "Siza.Agent.Discover.Merge" for the
module-size cap.
-}
module Siza.Agent.Discover.Rank (
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

{- | The total-order strata of section 7: environment exact first, then
installed / hidden / absent exact, then weaker kinds — an exact match can
never be outranked by substring, synonym or semantic evidence (R3.2).
-}
stratum :: NotebookEnv -> Interpreted -> DHit -> Int
stratum env interp h
    | exact && dhInstall h `elem` [InstBuiltin, InstNotebook] = 1
    | exact && aliasScoped = 1
    | exact && dhInstall h == InstInstalled = 2
    | exact && dhInstall h == InstHidden = 3
    | exact && dhInstall h `elem` [InstAbsentKnown, InstAbsentUnknown] = 4
    | dhKind h == MkPrefix = 6
    | dhKind h == MkType = if iShape interp == "type" then 2 else 7
    | dhKind h `elem` [MkSubstring, MkModule] = 8
    | dhKind h == MkSynonym = 9
    | otherwise = 10
  where
    exact = dhKind h == MkExact
    aliasScoped =
        dhModule h `elem` (map snd (neAliases env) ++ neImports env)

-- | The full deterministic ranking key (R3.7); 'Construct.producerKey' nests it.
type RankKey =
    (Int, Int, Int, Int, Int, (Int, Int), Int, Int, Text, Text)

{- | The within-stratum order (section 7 stage 2): install state, the
internal-module demotion (public API before @*.Internal@ noise), then a
call-ready bit — a hit carrying its signature outranks a signatureless
package stub on evidence, never on name order — then signature plainness and
the deterministic length/name tie-breaks (R3.7).
-}
rankKey :: NotebookEnv -> Interpreted -> DHit -> RankKey
rankKey = rankKeyIn []

{- | 'rankKey' with the union's imported-package evidence (R4.5): a hit from a
package the notebook imports (any of its modules) leads its stratum — decided
by import evidence and install state, never by package name.
-}
rankKeyIn :: [Text] -> NotebookEnv -> Interpreted -> DHit -> RankKey
rankKeyIn importedPkgs env interp h =
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
    importedBand
        | dhInstall h `elem` [InstBuiltin, InstNotebook] = 0
        | dhModule h `elem` (map snd (neAliases env) ++ neImports env) = 0
        | not (T.null (dhPackage h)), dhPackage h `elem` importedPkgs = 0
        | otherwise = 1

{- | Signature plainness (section 7, round 7): (constraint count, type-level
argument count), computed from the signature's own shape only — module and
package identity never enter the key, so a plain variant of a name always
outranks its constraint-heavy or type-applied twin.
-}
plainness :: Text -> (Int, Int)
plainness ty = (constraintCount ty, typeLevelArgCount ty)

-- | Constraint atoms left of the signature's @=>@ arrows.
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

-- | Type-level arguments: visible @\@T@ applications and promoted @'X@ tokens.
typeLevelArgCount :: Text -> Int
typeLevelArgCount ty =
    length
        [w | w <- T.words ty, "@" `T.isPrefixOf` w || "'" `T.isPrefixOf` w]

{- | How many internal-module hits sit in the tail a limit cut off — demoted
AND omitted, so the envelope can summarise them instead of dropping silently.
-}
demotedCount :: [DHit] -> Int
demotedCount = length . filter (isNoiseModule . dhModule)

{- | Same-package name-variant stubs (R6-T1): two SIGNATURELESS rows whose
names are both case\/hyphen variants of their one package are duplicates of
the package fact itself and fuse to one row — so variant rows can never crowd
a function-with-signature hit out of the shown window (R3.2\/R3.1). A hit
carrying a signature is never a variant stub, whatever its name.
-}
samePackageVariants :: DHit -> DHit -> Bool
samePackageVariants a b =
    variantStub a && variantStub b && dhPackage a == dhPackage b
  where
    variantStub h =
        T.null (dhType h)
            && not (T.null (dhPackage h))
            && norm (dhName h) == norm (dhPackage h)
    norm = T.toLower . T.filter (/= '-')

{- | Fuse two answers for the same (name, module): keep the better-ranked
copy, fill its empty provenance fields from the other (R3.3 conservation).
-}
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
