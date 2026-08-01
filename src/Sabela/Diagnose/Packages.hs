{-# LANGUAGE OverloadedStrings #-}

module Sabela.Diagnose.Packages (
    packageNameIndex,
    resolvePackageToken,
    findModulePackage,
) where

import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

import Sabela.AI.PackageIndex (storePackageDb)
import Sabela.AI.Similarity (trigramSimilarity)

packageNameIndex :: [Text]
packageNameIndex =
    nubOrd
        [ "dataframe"
        , "granite"
        , "text"
        , "vector"
        , "containers"
        , "aeson"
        , "dataframe-core"
        , "bytestring"
        , "directory"
        , "filepath"
        , "process"
        , "time"
        , "random"
        , "unordered-containers"
        , "hashable"
        , "scientific"
        , "split"
        , "mtl"
        , "transformers"
        ]

nubOrd :: (Ord a) => [a] -> [a]
nubOrd = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
        | x `Set.member` seen = go seen xs
        | otherwise = x : go (Set.insert x seen) xs

resolvePackageToken :: Text -> Maybe Text
resolvePackageToken tok
    | tok `elem` packageNameIndex = Just tok
    | null scored = Nothing
    | bestScore >= fuzzyThreshold = Just best
    | otherwise = Nothing
  where
    scored = [(p, trigramSimilarity tok p) | p <- packageNameIndex]
    (best, bestScore) = maximumBy (comparing snd) scored

fuzzyThreshold :: Double
fuzzyThreshold = 0.2

{- | The package exposing a module, from the installed package dbs.

Must search the cabal store as well as the global db: everything a notebook
declares with @-- cabal: build-depends:@ lands in the store, so a global-only
lookup answers for base and nothing else.
-}
findModulePackage :: Text -> IO (Maybe Text)
findModulePackage m = do
    mDb <- storePackageDb
    (>>= firstPackage) <$> tryFind (dbArgs mDb)
  where
    dbArgs = maybe [] (\db -> ["--package-db=" ++ db])
    tryFind extra = do
        r <-
            readProcessWithExitCode
                "ghc-pkg"
                (extra ++ ["--simple-output", "find-module", T.unpack m])
                ""
        case r of
            (ExitSuccess, out, _) -> pure (Just (T.pack out))
            _ -> pure Nothing
    firstPackage out = case T.words out of
        (w : _) -> Just (stripVersion w)
        [] -> Nothing

stripVersion :: Text -> Text
stripVersion = T.intercalate "-" . takeWhile (not . isVersion) . T.splitOn "-"
  where
    isVersion p =
        not (T.null p) && T.all (\c -> c `elem` ("0123456789." :: String)) p
