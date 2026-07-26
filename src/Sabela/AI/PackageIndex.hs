{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The installed-package module index, read from @ghc-pkg dump@.

A package installed in the store but not exposed in the session's environment
has no searchable surface at all: @:browse@ cannot reach it, and the local
Hoogle database indexes built haddocks, which the store does not have. So a
model asking what is in @dataframe@ was told only "hidden — declare it", and
went off to hand-roll the work instead (@live_test33_wine@).

@ghc-pkg dump@ answers the module half of that question for every installed
package, exposed or not, in about a second, with no network.
-}
module Sabela.AI.PackageIndex (
    PackageEntry (..),
    parsePackageDump,
    modulesOfPackage,
    packagesExposingModule,
    modulesMatching,
    packagesMatchingSynopsis,
    storePackageDb,
    installedPackages,
) where

import Control.Exception (SomeException, try)
import Data.Char (isAlphaNum, isSpace, isUpper)
import Data.List (isPrefixOf, nub, sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version (showVersion)
import System.Directory (doesDirectoryExist, getHomeDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Info (fullCompilerVersion)
import System.Process (readProcessWithExitCode)

-- | One installed package: what it is called, what it says it does, what it exposes.
data PackageEntry = PackageEntry
    { peName :: Text
    , peVersion :: Text
    , peSynopsis :: Text
    , peModules :: [Text]
    }
    deriving (Eq, Show)

{- | Parse @ghc-pkg dump@: records separated by a @---@ line, fields as
@key: value@ with indented continuations.

@exposed-modules@ is the one that matters and it is the awkward one — it wraps
across lines, and a re-exported module carries its origin, which is not part of
the name a caller writes:

> exposed-modules:
>     DataFrame,
>     DataFrame.Display from dtfrm-vz-1.0.3.0-60b765f1:DataFrame.Display
-}
parsePackageDump :: Text -> [PackageEntry]
parsePackageDump = concatMap entryOf . splitRecords . T.lines
  where
    entryOf rec = case (fieldOf "name" rec, fieldOf "version" rec) of
        (Just n, Just v)
            | not (T.null n) ->
                [ PackageEntry
                    n
                    v
                    (T.unwords (map T.strip (fieldLines "synopsis" rec)))
                    (moduleNames (fieldLines "exposed-modules" rec))
                ]
        _ -> []

-- | Records are separated by a bare @---@ line.
splitRecords :: [Text] -> [[Text]]
splitRecords = foldr step [[]]
  where
    step l acc@(cur : rest)
        | T.strip l == "---" = [] : acc
        | otherwise = (l : cur) : rest
    step l [] = [[l]]

-- | A single-line field's value.
fieldOf :: Text -> [Text] -> Maybe Text
fieldOf key rec = case fieldLines key rec of
    (v : _) -> Just (T.strip v)
    [] -> Nothing

{- | A field's value plus its indented continuation lines, the value's own
first line included (it may be empty when the list starts on the next line).
-}
fieldLines :: Text -> [Text] -> [Text]
fieldLines key rec = case break (isKey key) rec of
    (_, l : rest) -> T.drop (T.length key + 1) l : takeWhile indented rest
    _ -> []
  where
    isKey k l = (k <> ":") `T.isPrefixOf` l
    indented l = not (T.null l) && isSpace (T.head l)

{- | Module names from an @exposed-modules@ body. @ghc-pkg dump@ emits BOTH
separators: comma-separated with @from pkg:Module@ re-export origins
(dataframe), and plain space-separated (hspec). Splitting on commas alone and
rejecting spaced chunks dropped every space-form package's whole module list,
so @Test.Hspec@ missed EXACT match and near-spelling "corrected" it to a
module of a different package.
-}
moduleNames :: [Text] -> [Text]
moduleNames body =
    nub
        [ m
        | chunk <- T.splitOn "," (T.unwords (map T.strip body))
        , let noOrigin = fst (T.breakOn " from " (T.strip chunk))
        , m <- T.words noOrigin
        , moduleShaped m
        ]

-- | An Upper-headed dotted identifier — the only thing a module is called.
moduleShaped :: Text -> Bool
moduleShaped m = case T.uncons m of
    Just (c, _) ->
        isUpper c
            && T.all (\x -> isAlphaNum x || x `elem` ("._'" :: String)) m
    Nothing -> False

-- | The modules a named package exposes; empty when it is not installed.
modulesOfPackage :: [PackageEntry] -> Text -> [Text]
modulesOfPackage idx name =
    concat [peModules p | p <- idx, peName p == name]

-- | The installed packages exposing a module, by exact module name.
packagesExposingModule :: [PackageEntry] -> Text -> [PackageEntry]
packagesExposingModule idx m = [p | p <- idx, m `elem` peModules p]

{- | Installed packages whose SYNOPSIS answers a description, ranked by how
many of the query's words it carries.

A caller who knows what it wants to do but not what the library is called can
name neither the package nor its modules, and every name-keyed lookup misses.
The synopsis is the one place a package says what it is for, and @ghc-pkg
dump@ already carries it for every installed package, hidden ones included.
-}
packagesMatchingSynopsis :: [PackageEntry] -> Text -> [PackageEntry]
packagesMatchingSynopsis idx q
    | null terms = []
    | otherwise =
        map fst
            . sortOn (Down . snd)
            $ [ (p, hits)
              | p <- idx
              , let syn = T.toLower (peSynopsis p)
              , let hits = length [t | t <- terms, t `T.isInfixOf` syn]
              , hits > 0
              ]
  where
    terms =
        [ w
        | w <- T.words (T.toLower (T.strip q))
        , T.length w >= 3
        , w `notElem` synopsisStopWords
        ]

{- | Words too common in a synopsis to carry signal. Kept tiny and generic:
this is a stop list, never a domain vocabulary.
-}
synopsisStopWords :: [Text]
synopsisStopWords =
    ["the", "and", "for", "with", "haskell", "library", "bindings", "simple"]

{- | Modules whose name contains @q@, case-insensitively, paired with the
package exposing them — the keyword surface a hidden package otherwise lacks.
-}
modulesMatching :: [PackageEntry] -> Text -> [(Text, PackageEntry)]
modulesMatching idx q
    | T.null ql = []
    | otherwise =
        [ (m, p)
        | p <- idx
        , m <- peModules p
        , ql `T.isInfixOf` T.toLower m
        ]
  where
    ql = T.toLower (T.strip q)

{- | The cabal store's package database, where a notebook's installed-but-not
exposed packages live. @SABELA_STORE_PACKAGE_DB@ overrides; otherwise the
@~\/.cabal\/store\/ghc-\<version\>-*\/package.db@ for THIS compiler.

Keyed on 'fullCompilerVersion', never on "newest": a box with several GHCs has
a store per version, and picking the wrong one indexes packages the session
cannot load. ('listDirectory' does not sort, so there is no newest to take.)
-}
storePackageDb :: IO (Maybe FilePath)
storePackageDb = do
    override <- lookupEnv "SABELA_STORE_PACKAGE_DB"
    case override of
        Just p -> pure (Just p)
        Nothing -> do
            home <- getHomeDirectory
            let root = home </> ".cabal" </> "store"
                prefix = "ghc-" ++ showVersion fullCompilerVersion ++ "-"
            ok <- doesDirectoryExist root
            if not ok
                then pure Nothing
                else do
                    entries <- safeList root
                    dbs <-
                        filterExisting
                            [ root </> e </> "package.db"
                            | e <- entries
                            , prefix `isPrefixOf` e
                            ]
                    pure (listToMaybe dbs)
  where
    filterExisting = go
      where
        go [] = pure []
        go (p : ps) = do
            ok <- doesDirectoryExist p
            if ok then (p :) <$> go ps else go ps

safeList :: FilePath -> IO [FilePath]
safeList dir = do
    r <- try (listDirectory dir)
    pure $ case r of
        Left (_ :: SomeException) -> []
        Right es -> es

{- | Every package in @db@, parsed from one @ghc-pkg dump@. Empty when
@ghc-pkg@ is unavailable or the database cannot be read — a missing index is
never reported as an empty package set by the caller.
-}
installedPackages :: FilePath -> IO [PackageEntry]
installedPackages db = do
    r <-
        try
            ( readProcessWithExitCode
                "ghc-pkg"
                ["--package-db=" ++ db, "dump"]
                ""
            )
    pure $ case r of
        Left (_ :: SomeException) -> []
        Right (ExitSuccess, out, _) -> parsePackageDump (T.pack out)
        Right _ -> []
