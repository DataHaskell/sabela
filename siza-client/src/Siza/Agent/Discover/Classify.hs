{- | Classify raw backend payloads into typed 'SourceAnswer's for the union
merge: the notebook environment layer, the session (@find_function@ matches
and browse cards), and the capability/hoogle channel. No payload shape ever
reaches the model unparsed (R3.6).
-}
module Siza.Agent.Discover.Classify (
    candidatePackages,
    envAnswer,
    sessionAnswer,
    capabilityAnswer,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Interpret (stripVersion)
import Siza.Agent.Discover.Types (
    DHit (..),
    InstallState (..),
    Interpreted (..),
    MatchKind (..),
    NotebookEnv (..),
    SourceAnswer (..),
    okAnswer,
    unavailableAnswer,
 )
import Siza.Agent.Discover.UnitName (scrubCardUnits)

-- | Package names whose upstream existence is worth checking for this query.
candidatePackages :: Interpreted -> [SourceAnswer] -> [Text]
candidatePackages interp answers =
    nub . filter (not . T.null) $
        [stripVersion (iName interp) | iShape interp `elem` ["name", "package"]]
            ++ [dhPackage h | a <- answers, h <- saHits a]
            ++ [p | a <- answers, (p, _) <- saPkgModules a]

{- | The notebook's own answer: builtins, cell-defined bindings, and modules
the notebook itself imports are hits before any backend is consulted (R1.5,
round 2 section 2) — a documented name or an imported module cannot be denied.
-}
envAnswer :: NotebookEnv -> Interpreted -> SourceAnswer
envAnswer env interp =
    okAnswer "notebook" (builtin ++ binding ++ dslModule ++ importedMod)
  where
    n = iName interp
    builtin =
        [ (envHit n "(session prelude)" "sabela")
            { dhVersion = "builtin"
            , dhInstall = InstBuiltin
            , dhUse = Just "in scope at session start — no import needed"
            }
        | n `elem` neBuiltins env
        ]
    binding =
        [ (envHit n "(notebook)" "(notebook)")
            { dhVersion = "live"
            , dhInstall = InstNotebook
            , dhUse = Just "defined by a notebook cell"
            }
        | n `elem` neBindings env
        ]
    dslModule =
        [ (envHit n n "sabela-notebook")
            { dhVersion = "builtin"
            , dhInstall = InstBuiltin
            , dhUse = Just ("import " <> n)
            }
        | n `elem` neBuiltinModules env
        ]
    importedMod =
        [ (envHit n n "")
            { dhVersion = "live"
            , dhInstall = InstNotebook
            , dhUse = Just (importedNote env n i)
            }
        | Just i <- [lookup n (neImportCells env)]
        ]

-- | An environment hit is an exact lookup: stratum 1, notebook provenance.
envHit :: Text -> Text -> Text -> DHit
envHit n m p = (baseHit n m p){dhKind = MkExact, dhOrigin = "notebook"}

-- | "imported by cell 0 as D; in scope" — the round 2 module answer.
importedNote :: NotebookEnv -> Text -> Int -> Text
importedNote env m i =
    "imported by cell " <> T.pack (show i) <> aliasPart <> "; in scope"
  where
    aliasPart = case [a | (a, m') <- neAliases env, m' == m] of
        (a : _) -> " as " <> a
        [] -> ""

{- | The session source: 'Nothing' means unreachable (kernel down or transport
error), never absence. Matches, browse cards and did-you-mean suggestions all
become hits or cards — no tier is ever discarded (R3.3).
-}
sessionAnswer :: Interpreted -> Maybe Value -> SourceAnswer
sessionAnswer _ Nothing =
    unavailableAnswer
        "session"
        "session unavailable (no live kernel or transport error)"
sessionAnswer interp (Just v@(Object o))
    | Just (Array ms) <- KM.lookup "matches" o =
        okAnswer "session" (map (matchHit interp) (toList ms))
    | Just (String st) <- KM.lookup "status" o = cardAnswer interp st v
    | otherwise = okAnswer "session" []
sessionAnswer _ (Just _) = okAnswer "session" []

{- | A browse-card payload by status: ok listing, hidden package, did-you-mean.
The card is unit-scrubbed at this seam (R3.10/P6): a version-qualified GHC
unit label never reaches the model through the card or its hits.
-}
cardAnswer :: Interpreted -> Text -> Value -> SourceAnswer
cardAnswer interp st v0 = case (st, scrubCardUnits v0) of
    ("ok", v@(Object _)) ->
        (okAnswer "session" (exportHits interp v)){saCard = Just v}
    ("hidden-package", v@(Object o)) ->
        (okAnswer "session" (hiddenHit o)){saCard = Just v}
    ("not-found", Object o) ->
        (okAnswer "session" (suggestHits o))
            { saNote = "module not found; did-you-mean listed"
            }
    (_, Object o) ->
        (okAnswer "session" []){saNote = textAt "message" o}
    _ -> okAnswer "session" []
  where
    hiddenHit o =
        [ (baseHit pkg (textAt "module" o) pkg)
            { dhInstall = InstHidden
            , dhOrigin = "session"
            , dhCabal = Just (textAt "cabal" o)
            , dhKind =
                if pkg == iName interp then MkExact else MkModule
            }
        | let pkg = textAt "package" o
        , not (T.null pkg)
        ]
    suggestHits o =
        [ (baseHit m m (textAt "package" o))
            { dhOrigin = "session"
            , dhKind = MkSynonym
            , dhCabal =
                let cabal = textAt "cabal" o
                 in if T.null cabal then Nothing else Just cabal
            }
        | Just (Array ss) <- [KM.lookup "suggestions" o]
        , String m <- toList ss
        ]

-- | The export lines of an ok listing card, as session-evidenced hits.
exportHits :: Interpreted -> Value -> [DHit]
exportHits interp (Object o) =
    [ (baseHit n modName "")
        { dhType = ty
        , dhInstall = InstInstalled
        , dhOrigin = "session"
        , dhKind = if n == iName interp then MkExact else MkModule
        }
    | Just (Array es) <- [KM.lookup "exports" o]
    , String line <- toList es
    , let (n, ty) = splitSig line
    , not (T.null n)
    ]
  where
    modName = textAt "module" o
exportHits _ _ = []

-- | One @find_function@ match, kind decided against the resolved name.
matchHit :: Interpreted -> Value -> DHit
matchHit interp m =
    (baseHit n (textAt' "module" m) "")
        { dhType = textAt' "type" m
        , dhInstall = InstInstalled
        , dhOrigin = "session"
        , dhKind = kind
        }
  where
    n = textAt' "name" m
    via = textAt' "via" m
    q = iName interp
    kind
        | via == "synonym" = MkSynonym
        | via == "type" = MkType
        | via == "module" = MkModule
        | n == q = MkExact
        | q `T.isPrefixOf` n = MkPrefix
        | q `T.isInfixOf` n = MkSubstring
        | otherwise = MkSemantic

{- | The capability/hoogle source: enriched per-package buckets (with their
API functions) or flat symbol hits. 'Nothing' = channel unreachable — the
lexical lookup runs on every arm (section 2); only SHIP enrichment is gated.
-}
capabilityAnswer :: Interpreted -> Maybe Value -> SourceAnswer
capabilityAnswer _ Nothing =
    unavailableAnswer "hoogle" "hoogle/capability channel unreachable"
capabilityAnswer interp (Just (Object o))
    | Just (Array hs) <- KM.lookup "hits" o =
        (okAnswer "hoogle" (concatMap (bucketHits interp) (toList hs)))
            { saPkgModules = concatMap bucketModules (toList hs)
            }
capabilityAnswer _ (Just _) = okAnswer "hoogle" []

-- | The hits of one bucket: the package itself plus its surfaced API.
bucketHits :: Interpreted -> Value -> [DHit]
bucketHits interp b@(Object o)
    | T.null pkg && not (T.null flatName) = [flatHit interp b]
    | T.null pkg = []
    | otherwise = pkgHit : apiHits
  where
    pkg = textAt "package" o
    flatName = textAt "name" o
    cabal = textAt "cabal" o
    version = textAt "version" o
    firstModule = case bucketModules b of
        ((_, m : _) : _) -> m
        _ -> ""
    pkgHit =
        (baseHit pkg firstModule pkg)
            { dhVersion = version
            , dhOrigin = "hoogle"
            , dhKind = kindFor interp pkg
            , dhCabal = if T.null cabal then Nothing else Just cabal
            }
    apiHits =
        [ (baseHit n (textAt' "module" a) pkg)
            { dhType = textAt' "type" a
            , dhVersion = version
            , dhOrigin = "hoogle"
            , dhKind = kindFor interp n
            , dhCabal = if T.null cabal then Nothing else Just cabal
            }
        | Just (Array as) <- [KM.lookup "api" o]
        , a@(Object _) <- toList as
        , let n = textAt' "name" a
        , not (T.null n)
        ]
bucketHits _ _ = []

-- | A flat @{name, type, module, package}@ symbol hit.
flatHit :: Interpreted -> Value -> DHit
flatHit interp h =
    (baseHit n (textAt' "module" h) (textAt' "package" h))
        { dhType = textAt' "type" h
        , dhOrigin = "hoogle"
        , dhKind = kindFor interp n
        }
  where
    n = textAt' "name" h

-- | The package -> modules map a bucket discloses (probe targets).
bucketModules :: Value -> [(Text, [Text])]
bucketModules (Object o) =
    [ (pkg, [m | String m <- toList ms])
    | let pkg = textAt "package" o
    , not (T.null pkg)
    , Just (Array ms) <- [KM.lookup "modules" o]
    ]
bucketModules _ = []

-- | Kind of a name against the resolved query and its prose terms.
kindFor :: Interpreted -> Text -> MatchKind
kindFor interp n
    | n == q = MkExact
    | T.toLower n `elem` iTerms interp = MkExact
    | not (T.null q) && q `T.isPrefixOf` n && iShape interp /= "prose" = MkPrefix
    | not (T.null q) && q `T.isInfixOf` n && iShape interp /= "prose" =
        MkSubstring
    | otherwise = MkSemantic
  where
    q = iName interp

-- | A default hit skeleton shared by the classifiers.
baseHit :: Text -> Text -> Text -> DHit
baseHit n m p =
    DHit n "" m p "" InstAbsentUnknown MkSemantic "" Nothing Nothing

splitSig :: Text -> (Text, Text)
splitSig line = case T.breakOn "::" line of
    (n, rest)
        | T.null rest -> (T.strip n, "")
        | otherwise -> (T.strip n, T.strip (T.drop 2 rest))

textAt :: K.Key -> KM.KeyMap Value -> Text
textAt k o = case KM.lookup k o of
    Just (String s) -> s
    _ -> ""

textAt' :: K.Key -> Value -> Text
textAt' k (Object o) = textAt k o
textAt' _ _ = ""
