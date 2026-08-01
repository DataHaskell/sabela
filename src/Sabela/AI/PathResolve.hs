{-# LANGUAGE OverloadedStrings #-}

{- | Resolving a path a cell named against the files that actually exist,
shared by the runtime repair path and the pre-write gate.
-}
module Sabela.AI.PathResolve (
    PathLookup (..),
    lookupPath,
    workDirFiles,
    isUrl,
) where

import Control.Exception (IOException, try)
import Data.Char (isAlphaNum)
import Data.List (isPrefixOf, sortOn)
import Data.Ord (Down (..))
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, takeFileName, (</>))

import Sabela.AI.Similarity (trigramSimilarity)

{- | 'Unique' is the only verdict safe to substitute into source: the same
file name under a different directory. A same-stem match with a different
extension is a suggestion, never a rewrite.
-}
data PathLookup
    = Unique FilePath
    | Nearby [FilePath]
    | NoneNearby
    deriving (Eq, Show)

fuzzyThreshold :: Double
fuzzyThreshold = 0.3

candidateCap :: Int
candidateCap = 3

lookupPath :: FilePath -> [FilePath] -> PathLookup
lookupPath wrong files = case basenameMatches of
    [one] -> Unique one
    (_ : _ : _) -> Nearby basenameMatches
    [] -> case take candidateCap (stemMatches <> fuzzyMatches) of
        [] -> NoneNearby
        cs -> Nearby cs
  where
    basenameMatches = [f | f <- files, takeFileName f == takeFileName wrong]
    stemMatches = [f | f <- files, takeBaseName f == takeBaseName wrong]
    fuzzyMatches =
        [ f
        | (f, s) <- sortOn (Down . snd) (map score files)
        , s >= fuzzyThreshold
        , f `notElem` stemMatches
        ]
    score f = (f, trigramSimilarity (T.pack wrong) (T.pack f))

isUrl :: FilePath -> Bool
isUrl p = case break (== ':') p of
    (scheme, ':' : '/' : '/' : _) ->
        not (null scheme)
            && all (\c -> isAlphaNum c || c `elem` ("+-." :: String)) scheme
    _ -> False

workDirFiles :: FilePath -> IO [FilePath]
workDirFiles workDir = take fileCap <$> walk "" (0 :: Int)
  where
    walk rel depth
        | depth > maxDepth = pure []
        | otherwise = do
            entries <- safeListDirectory (workDir </> rel)
            fmap concat (mapM (visit rel depth) (filter keep entries))
    visit rel depth name = do
        let relPath = if null rel then name else rel </> name
        isDir <- doesDirectoryExist (workDir </> relPath)
        if isDir then walk relPath (depth + 1) else pure [relPath]
    keep name = not ("." `isPrefixOf` name) && name `notElem` skipDirs
    skipDirs = ["dist-newstyle", "node_modules", ".git", ".stack-work", "_build"]
    maxDepth = 6
    fileCap = 2000

safeListDirectory :: FilePath -> IO [FilePath]
safeListDirectory dir = do
    r <- try (listDirectory dir) :: IO (Either IOException [FilePath])
    pure (either (const []) id r)
