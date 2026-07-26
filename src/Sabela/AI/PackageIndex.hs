{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

data PackageEntry = PackageEntry
    { peName :: Text
    , peVersion :: Text
    , peSynopsis :: Text
    , peModules :: [Text]
    }
    deriving (Eq, Show)

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

splitRecords :: [Text] -> [[Text]]
splitRecords = foldr step [[]]
  where
    step l acc@(cur : rest)
        | T.strip l == "---" = [] : acc
        | otherwise = (l : cur) : rest
    step l [] = [[l]]

fieldOf :: Text -> [Text] -> Maybe Text
fieldOf key rec = case fieldLines key rec of
    (v : _) -> Just (T.strip v)
    [] -> Nothing

fieldLines :: Text -> [Text] -> [Text]
fieldLines key rec = case break (isKey key) rec of
    (_, l : rest) -> T.drop (T.length key + 1) l : takeWhile indented rest
    _ -> []
  where
    isKey k l = (k <> ":") `T.isPrefixOf` l
    indented l = not (T.null l) && isSpace (T.head l)

moduleNames :: [Text] -> [Text]
moduleNames body =
    nub
        [ m
        | chunk <- T.splitOn "," (T.unwords (map T.strip body))
        , let noOrigin = fst (T.breakOn " from " (T.strip chunk))
        , m <- T.words noOrigin
        , moduleShaped m
        ]

moduleShaped :: Text -> Bool
moduleShaped m = case T.uncons m of
    Just (c, _) ->
        isUpper c
            && T.all (\x -> isAlphaNum x || x `elem` ("._'" :: String)) m
    Nothing -> False

modulesOfPackage :: [PackageEntry] -> Text -> [Text]
modulesOfPackage idx name =
    concat [peModules p | p <- idx, peName p == name]

packagesExposingModule :: [PackageEntry] -> Text -> [PackageEntry]
packagesExposingModule idx m = [p | p <- idx, m `elem` peModules p]

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

synopsisStopWords :: [Text]
synopsisStopWords =
    ["the", "and", "for", "with", "haskell", "library", "bindings", "simple"]

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
