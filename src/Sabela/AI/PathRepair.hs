{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.PathRepair (
    notFoundPath,
    pathNearMissFix,
    pathNotFoundGuidance,
) where

import Control.Exception (IOException, try)
import Data.Char (isAlphaNum)
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeFileName, (</>))

import Sabela.AI.Similarity (trigramSimilarity)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Diagnose (Guidance (..))

notFoundPath :: Text -> Maybe FilePath
notFoundPath err = listToMaybe (mapMaybe fromLine (T.lines err))
  where
    fromLine raw = case T.splitOn ": " (stripExceptionPrefix (T.strip raw)) of
        (p : loc : reason : _)
            | "does not exist" `T.isPrefixOf` reason
            , not (T.null p)
            , pathLike p
            , ioLocationLike loc ->
                Just (T.unpack p)
        _ -> Nothing
    stripExceptionPrefix t =
        fromMaybe t (T.stripPrefix "*** Exception: " t)

pathLike :: Text -> Bool
pathLike = T.all (`notElem` ("(){}\"[]," :: String))

ioLocationLike :: Text -> Bool
ioLocationLike loc = not (T.null loc) && not (T.any (== ' ') loc)

data PathLookup
    = Unique FilePath
    | Nearby [FilePath]
    | NoneNearby

fuzzyThreshold :: Double
fuzzyThreshold = 0.3

candidateCap :: Int
candidateCap = 3

lookupPath :: FilePath -> [FilePath] -> PathLookup
lookupPath wrong files = case basenameMatches of
    [one] -> Unique one
    (_ : _ : _) -> Nearby basenameMatches
    [] -> case fuzzyMatches of
        [] -> NoneNearby
        cs -> Nearby cs
  where
    basenameMatches = [f | f <- files, takeFileName f == takeFileName wrong]
    fuzzyMatches =
        take
            candidateCap
            [ f
            | (f, s) <- sortOn (Down . snd) (map score files)
            , s >= fuzzyThreshold
            ]
    score f = (f, trigramSimilarity (T.pack wrong) (T.pack f))

pathNearMissFix ::
    FilePath -> Either Text ExecutionResult -> Text -> IO (Maybe Text)
pathNearMissFix workDir res src = case notFoundPath =<< runtimeErrorOf res of
    Nothing -> pure Nothing
    Just wrong -> do
        files <- workDirFiles workDir
        pure $ case lookupPath wrong files of
            Unique right ->
                let src' = T.replace (T.pack wrong) ("./" <> T.pack right) src
                 in if src' == src then Nothing else Just src'
            _ -> Nothing

pathNotFoundGuidance ::
    FilePath -> Either Text ExecutionResult -> IO (Maybe Guidance)
pathNotFoundGuidance workDir res = case notFoundPath =<< runtimeErrorOf res of
    Nothing -> pure Nothing
    Just wrong
        | isUrl wrong -> pure (Just (Guidance "url-as-path" (urlMessage wrong)))
        | otherwise -> do
            files <- workDirFiles workDir
            pure $ case lookupPath wrong files of
                Unique _ -> Nothing
                Nearby cs -> Just (Guidance "file-not-found" (candidateMessage wrong cs))
                NoneNearby -> Just (Guidance "file-not-found" (noneMessage wrong))

isUrl :: FilePath -> Bool
isUrl p = case break (== ':') p of
    (scheme, ':' : '/' : '/' : _) ->
        not (null scheme)
            && all (\c -> isAlphaNum c || c `elem` ("+-." :: String)) scheme
    _ -> False

urlMessage :: FilePath -> Text
urlMessage wrong =
    "`"
        <> T.pack wrong
        <> "` is a URL, not a filesystem path. The file-opening functions \
           \(readFile and its relatives) resolve local paths only; nothing \
           \was fetched."

candidateMessage :: FilePath -> [FilePath] -> Text
candidateMessage wrong cs =
    "The path `"
        <> T.pack wrong
        <> "` does not exist. Nearest files under the work dir: "
        <> T.intercalate ", " (map (tick . T.pack) cs)
        <> ". Ask the user which one they meant, or use the exact path."
  where
    tick t = "`" <> t <> "`"

noneMessage :: FilePath -> Text
noneMessage wrong =
    "The path `"
        <> T.pack wrong
        <> "` does not exist and no similar file was found under the work \
           \dir. Ask the user for the correct path."

runtimeErrorOf :: Either Text ExecutionResult -> Maybe Text
runtimeErrorOf (Left e) = Just e
runtimeErrorOf (Right er) = erError er

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
