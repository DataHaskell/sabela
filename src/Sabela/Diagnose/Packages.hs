{-# LANGUAGE OverloadedStrings #-}

module Sabela.Diagnose.Packages (
    packageForModule,
    table,
    packageNameIndex,
    resolvePackageToken,
    findModulePackage,
) where

import Data.List (find, maximumBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

import Sabela.AI.Similarity (trigramSimilarity)

table :: [(Text, Text)]
table =
    [ ("DataFrame.Display", "dataframe")
    , ("DataFrame", "dataframe")
    , ("Granite", "granite")
    , ("Data.Text", "text")
    , ("Data.Vector", "vector")
    , ("Data.Map", "containers")
    , ("Data.Set", "containers")
    , ("Data.Aeson", "aeson")
    ]

packageForModule :: Text -> Maybe Text
packageForModule m = snd <$> find (matches . fst) table
  where
    matches p = p == m || (p <> ".") `T.isPrefixOf` m

packageNameIndex :: [Text]
packageNameIndex =
    nubOrd $
        map snd table
            ++ [ "dataframe-core"
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

findModulePackage :: Text -> IO (Maybe Text)
findModulePackage m = (>>= firstPackage) <$> tryFind
  where
    tryFind = do
        r <-
            readProcessWithExitCode
                "ghc-pkg"
                ["--simple-output", "find-module", T.unpack m]
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
