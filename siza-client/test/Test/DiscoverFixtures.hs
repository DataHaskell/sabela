{-# LANGUAGE OverloadedStrings #-}

{- | The controlled SYNTHETIC catalogue of testing-plan R10(a): two installed
packages (one with an internal module), a hidden package, an absent-but-known
package, a notebook alias import, and the prompt-documented builtins. No bench
libraries, so the invariants generalise past the eval corpus.
-}
module Test.DiscoverFixtures (
    SynPkg (..),
    synInstalled,
    synHidden,
    synAbsent,
    synHoogle,
    synHackageNames,
    catalogueExports,
    catalogueModules,
    cataloguePackages,
    discoverables,
    simCall,
    simSession,
    simCapability,
    runCat,
    runCatArgs,
    runCatArgsIn,
    installNamesFile,
    installNamesFileWith,
    argText,
    field,
    textField,
    stateOf,
    hitsOf,
    hitText,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isUpper)
import Data.Foldable (toList)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getTemporaryDirectory)
import System.Environment (setEnv)
import System.FilePath ((</>))

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.DiscoverTool (runDiscoverCall, runDiscoverTool)

-- | One synthetic package: name, version, hidden flag, modules with exports.
data SynPkg = SynPkg
    { spName :: Text
    , spVersion :: Text
    , spHidden :: Bool
    , spModules :: [(Text, [(Text, Text)])]
    }

synInstalled :: [SynPkg]
synInstalled =
    [ SynPkg
        "zephyr"
        "1.2.0"
        False
        [ ("Zephyr.Core", [("gust", "Int -> Wind"), ("lull", "Wind -> Wind")])
        , ("Zephyr.Internal.Raw", [("gustRaw", "Int -> Int")])
        ]
    , SynPkg
        "stratus"
        "0.9.1"
        False
        [
            ( "Stratus.Air"
            , [("lull", "Air -> Air"), ("stratify", "[Air] -> Layered")]
            )
        ]
    ]

synHidden :: SynPkg
synHidden =
    SynPkg
        "cumulus"
        "0.3.1"
        True
        [("Cumulus.Plot", [("bars", "[(Text, Double)] -> Plot -> Text")])]

synAbsent :: SynPkg
synAbsent =
    SynPkg "nimbus" "2.0.0" False [("Nimbus.Sky", [("drizzle", "Sky -> Rain")])]

-- | What the local hoogle DB knows: everything, installed or not.
synHoogle :: [SynPkg]
synHoogle = synInstalled ++ [synHidden, synAbsent]

-- | What the session can browse: everything in the universe except absentees.
sessionPkgsIn :: [SynPkg] -> [SynPkg]
sessionPkgsIn universe =
    [p | p <- universe, spName p /= spName synAbsent]

synHackageNames :: [Text]
synHackageNames = sort (map spName synHoogle)

catalogueExports :: [Text]
catalogueExports =
    [n | p <- synHoogle, (_, es) <- spModules p, (n, _) <- es]

catalogueModules :: [Text]
catalogueModules = [m | p <- synHoogle, (m, _) <- spModules p]

cataloguePackages :: [Text]
cataloguePackages = map spName synHoogle

{- | Every name the catalogue holds an answer for: exports, modules, packages,
the notebook binding and alias spelling, and two prompt-documented builtins.
-}
discoverables :: [Text]
discoverables =
    catalogueExports
        ++ catalogueModules
        ++ cataloguePackages
        ++ ["gustTotal", "Z.gust", "displayHtml", "slider"]

-- | The simulated tool dispatch a discover call fans out through.
simCall :: ToolName -> Value -> IO (Either Text ToolOutcome)
simCall = simCallIn synHoogle

-- | 'simCall' over an alternative package universe.
simCallIn :: [SynPkg] -> ToolName -> Value -> IO (Either Text ToolOutcome)
simCallIn _ ListCells _ = pure (Right (ToolOk notebookCells))
simCallIn universe FindFunction args =
    pure
        ( Right
            ( ToolOk
                ( simSession
                    (sessionPkgsIn universe)
                    (argText "query" args)
                    (argText "module" args)
                )
            )
        )
simCallIn universe SearchCapability args =
    pure (Right (ToolOk (simCapability universe (argText "query" args))))
simCallIn _ _ _ = pure (Left "unsupported tool in fixture")

notebookCells :: Value
notebookCells =
    object
        [ "cells"
            .= [ cellJ "import qualified Zephyr.Core as Z" []
               , cellJ "gustTotal = 42" ["gustTotal"]
               ]
        ]
  where
    cellJ src defs =
        object ["source" .= (src :: Text), "defines" .= (defs :: [Text])]

argText :: Text -> Value -> Text
argText k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
argText _ _ = ""

{- | @find_function@ over the SESSION-VISIBLE packages: browse cards for
module queries, name matches otherwise.
-}
simSession :: [SynPkg] -> Text -> Text -> Value
simSession sessionPkgs q mScope
    | moduleShaped q = browseAnswer q
    | otherwise =
        object ["query" .= q, "matches" .= (nameMatches ++ synonymNoise)]
  where
    moduleShaped t =
        not (T.any (== ' ') t) && maybe False (isUpper . fst) (T.uncons t)
    browseAnswer m = case [p | p <- sessionPkgs, m `elem` map fst (spModules p)] of
        (p : _)
            | spHidden p ->
                object
                    [ "module" .= m
                    , "status" .= ("hidden-package" :: Text)
                    , "package" .= spName p
                    , "cabal" .= ("-- cabal: build-depends: " <> spName p)
                    ]
            | otherwise ->
                object
                    [ "module" .= m
                    , "status" .= ("ok" :: Text)
                    , "exports"
                        .= [ n <> " :: " <> t
                           | (m', es) <- spModules p
                           , m' == m
                           , (n, t) <- es
                           ]
                    ]
        [] -> object ["query" .= m, "matches" .= ([] :: [Value])]
    nameMatches =
        [ matchJ m n t "name"
        | p <- sessionPkgs
        , not (spHidden p)
        , (m, es) <- spModules p
        , (n, t) <- es
        , q == n || q `T.isInfixOf` n
        , T.null mScope || mScope == m
        ]
    synonymNoise =
        [matchJ "Syn.Anim" "puffLoop" "Frame -> Frame" "synonym" | q == "gust"]
    matchJ m n t via =
        object ["module" .= m, "name" .= n, "type" .= t, "via" .= (via :: Text)]

-- | @search_capability@: enriched per-package buckets over the hoogle universe.
simCapability :: [SynPkg] -> Text -> Value
simCapability universe q = object ["query" .= q, "hits" .= map bucket matching]
  where
    tokens = T.words (T.toLower q)
    matching = [p | p <- universe, pkgMatch p || any (apiMatch . fst) (exports p)]
    pkgMatch p =
        spName p == q
            || spName p `elem` tokens
            || q `elem` map fst (spModules p)
            || (T.length q >= 3 && q `T.isInfixOf` spName p)
    apiMatch n = n == q || T.toLower n `elem` tokens
    exports p = [(n, t) | (_, es) <- spModules p, (n, t) <- es]
    bucket p =
        object
            [ "package" .= spName p
            , "version" .= spVersion p
            , "synopsis" .= ("synthetic package" :: Text)
            , "cabal" .= ("-- cabal: build-depends: " <> spName p)
            , "modules" .= map fst (spModules p)
            , "api" .= [apiJ p n t | (n, t) <- pickApi p]
            ]
    pickApi p = case [e | e <- exports p, apiMatch (fst e)] of
        [] -> take 2 (exports p)
        es -> es
    apiJ p n t =
        object
            [ "name" .= n
            , "module" .= moduleOf p n
            , "type" .= t
            ]
    moduleOf p n = case [m | (m, es) <- spModules p, n `elem` map fst es] of
        (m : _) -> m
        [] -> ""

-- | Run a discover query against the synthetic catalogue, capability lever on.
runCat :: Text -> IO Value
runCat q = do
    out <- runDiscoverTool True simCall q
    case out of
        ToolOk v -> pure v
        ToolErr v -> pure v

-- | Like 'runCat' but through the full argument-validating call path.
runCatArgs :: Text -> Value -> IO Value
runCatArgs = runCatArgsIn synHoogle

-- | 'runCatArgs' over an alternative package universe.
runCatArgsIn :: [SynPkg] -> Text -> Value -> IO Value
runCatArgsIn universe q args = do
    out <- runDiscoverCall True (simCallIn universe) q args
    case out of
        ToolOk v -> pure v
        ToolErr v -> pure v

-- | Write the sorted Hackage names file and point the resolver at it.
installNamesFile :: IO ()
installNamesFile = installNamesFileWith synHackageNames

-- | 'installNamesFile' with an explicit upstream name list.
installNamesFileWith :: [Text] -> IO ()
installNamesFileWith names = do
    dir <- getTemporaryDirectory
    let path = dir </> "siza-discover-test-names.txt"
    TIO.writeFile path (T.unlines (sort names))
    setEnv "SABELA_HACKAGE_NAMES" path

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (K.fromText k) o
field _ _ = Nothing

textField :: Text -> Value -> Text
textField k v = case field k v of
    Just (String s) -> s
    _ -> ""

stateOf :: Value -> Text
stateOf = textField "state"

hitsOf :: Value -> [Value]
hitsOf v = case field "hits" v of
    Just (Array a) -> toList a
    _ -> []

hitText :: Text -> Value -> Text
hitText = textField
