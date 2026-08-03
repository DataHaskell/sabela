module Siza.Agent.Discover.Classify (
    notebookAnswer,
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

import Sabela.AI.PromptCore (drawingBuiltins)
import Siza.Agent.Discover.HitJson (baseHit, textAt, textAt')
import Siza.Agent.Discover.Interpret (stripVersion)
import Siza.Agent.Discover.SessionAnswer (sessionAnswer)
import Siza.Agent.Discover.Types (
    DHit (..),
    InstallState (..),
    Interpreted (..),
    MatchKind (..),
    NotebookEnv (..),
    SourceAnswer (..),
    mkHit,
    okAnswer,
    statedHitType,
    unavailableAnswer,
 )

candidatePackages :: Interpreted -> [SourceAnswer] -> [Text]
candidatePackages interp answers =
    nub . filter (not . T.null) $
        [stripVersion (iName interp) | iShape interp `elem` ["name", "package"]]
            ++ [dhPackage h | a <- answers, h <- saHits a]
            ++ [p | a <- answers, (p, _) <- saPkgModules a]

envAnswer :: NotebookEnv -> Interpreted -> SourceAnswer
envAnswer env interp =
    okAnswer
        "notebook"
        (builtin ++ dslName ++ binding ++ dslModule ++ importedMod)
  where
    n = iName interp
    builtin =
        [ (envHit n "(session prelude)" "sabela")
            { dhVersion = "builtin"
            , dhInstall = InstBuiltin
            , dhUse = Just "in scope at session start — no import needed"
            }
        | n `elem` neBuiltins env
        , n `notElem` drawingBuiltins
        ]
    dslName =
        [ (envHit n "Sabela.Notebook" "sabela-notebook")
            { dhVersion = "builtin"
            , dhInstall = InstBuiltin
            , dhUse = Just "import Sabela.Notebook"
            }
        | n `elem` drawingBuiltins
        , n `elem` neBuiltins env
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

envHit :: Text -> Text -> Text -> DHit
envHit n m p = (baseHit n m p){dhKind = MkExact, dhOrigin = "notebook"}

importedNote :: NotebookEnv -> Text -> Int -> Text
importedNote env m i =
    "imported by cell " <> T.pack (show i) <> aliasPart <> "; in scope"
  where
    aliasPart = case [a | (a, m') <- neAliases env, m' == m] of
        (a : _) -> " as " <> a
        [] -> ""

capabilityAnswer :: Interpreted -> Maybe Value -> SourceAnswer
capabilityAnswer _ Nothing =
    unavailableAnswer "hoogle" "hoogle/capability channel unreachable"
capabilityAnswer interp (Just (Object o))
    | Just (Array hs) <- KM.lookup "hits" o =
        (okAnswer "hoogle" (concatMap (bucketHits interp) hs))
            { saPkgModules = concatMap bucketModules hs
            }
capabilityAnswer _ (Just _) = okAnswer "hoogle" []

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
            { dhType = statedHitType (textAt' "type" a)
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

flatHit :: Interpreted -> Value -> DHit
flatHit interp h =
    (baseHit n (textAt' "module" h) (textAt' "package" h))
        { dhType = statedHitType (textAt' "type" h)
        , dhOrigin = "hoogle"
        , dhKind = kindFor interp n
        }
  where
    n = textAt' "name" h

bucketModules :: Value -> [(Text, [Text])]
bucketModules (Object o) =
    [ (pkg, [m | String m <- toList ms])
    | let pkg = textAt "package" o
    , not (T.null pkg)
    , Just (Array ms) <- [KM.lookup "modules" o]
    ]
bucketModules _ = []

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

notebookAnswer :: Interpreted -> Maybe Value -> SourceAnswer
notebookAnswer _ Nothing = okAnswer "notebook" []
notebookAnswer interp (Just (Object o))
    | Just (Array ms) <- KM.lookup "matches" o =
        okAnswer "notebook" (concatMap (cellHit interp) ms)
notebookAnswer _ (Just _) = okAnswer "notebook" []

cellHit :: Interpreted -> Value -> [DHit]
cellHit interp (Object m)
    | Just cid <- cellIdOf m =
        [ (mkHit (iName interp) "" "")
            { dhInstall = InstNotebook
            , dhOrigin = "notebook"
            , dhUse = Just ("defined in notebook cell " <> cid)
            , dhKind = MkExact
            }
        ]
cellHit _ _ = []

cellIdOf :: KM.KeyMap Value -> Maybe Text
cellIdOf m = case (KM.lookup "cellId" m, KM.lookup "cell_id" m) of
    (Just v, _) -> render v
    (_, Just v) -> render v
    _ -> Nothing
  where
    render (Number n) = Just (T.pack (show (round n :: Int)))
    render (String s) = Just s
    render _ = Nothing
