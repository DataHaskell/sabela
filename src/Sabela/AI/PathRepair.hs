{-# LANGUAGE OverloadedStrings #-}

{- | Auto-repair for a file-not-found runtime exception (C4-1): a path near a
real file under the work dir (missing @./@, a wrong directory) is a fact GHC's
own runtime error names, not a guess — the deterministic sibling of
"Sabela.AI.ImportRepair". A unique basename match retries with the corrected
path; an ambiguous or absent match surfaces the nearest candidates as
'Guidance' instead of a silent dead end.
-}
module Sabela.AI.PathRepair (
    notFoundPath,
    pathNearMissFix,
    pathNotFoundGuidance,
) where

import Control.Exception (IOException, try)
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

-- | The failing path from a runtime "does not exist" 'IOException' message.
notFoundPath :: Text -> Maybe FilePath
notFoundPath err = listToMaybe (mapMaybe fromLine (T.lines err))
  where
    fromLine raw = case T.splitOn ": " (stripExceptionPrefix (T.strip raw)) of
        (p : _ : rest@(_ : _))
            | "does not exist" `T.isInfixOf` last rest, not (T.null p) ->
                Just (T.unpack p)
        _ -> Nothing
    stripExceptionPrefix t =
        fromMaybe t (T.stripPrefix "*** Exception: " t)

-- | How a failing path resolves against the real files under the work dir.
data PathLookup
    = Unique FilePath
    | Nearby [FilePath]
    | NoneNearby

-- | The near-miss threshold below which a fuzzy match is not worth naming.
fuzzyThreshold :: Double
fuzzyThreshold = 0.3

-- | Most candidates surfaced in a "nearest files" question.
candidateCap :: Int
candidateCap = 3

{- | Classify a wrong path against the real files: a single basename match is
a confident retry target; more than one is ambiguous; none falls back to
whole-path fuzzy similarity, so a directory typo still surfaces candidates.
-}
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
        take candidateCap
            [ f
            | (f, s) <- sortOn (Down . snd) (map score files)
            , s >= fuzzyThreshold
            ]
    score f = (f, trigramSimilarity (T.pack wrong) (T.pack f))

{- | Rewrite the cell source's failing path to the unique real file under
the work dir it near-misses, prefixed @./@ so it resolves from the session's
cwd. 'Nothing' when there is no failure to fix, no unique match, or the
rewrite is a no-op.
-}
pathNearMissFix :: FilePath -> Either Text ExecutionResult -> Text -> IO (Maybe Text)
pathNearMissFix workDir res src = case notFoundPath =<< runtimeErrorOf res of
    Nothing -> pure Nothing
    Just wrong -> do
        files <- workDirFiles workDir
        pure $ case lookupPath wrong files of
            Unique right ->
                let src' = T.replace (T.pack wrong) ("./" <> T.pack right) src
                 in if src' == src then Nothing else Just src'
            _ -> Nothing

{- | Guidance for a failing path that could not be confidently auto-fixed:
names the nearest candidates under the work dir, or says plainly that none
were found, so the model can ask the user rather than fail silently.
-}
pathNotFoundGuidance ::
    FilePath -> Either Text ExecutionResult -> IO (Maybe Guidance)
pathNotFoundGuidance workDir res = case notFoundPath =<< runtimeErrorOf res of
    Nothing -> pure Nothing
    Just wrong -> do
        files <- workDirFiles workDir
        pure $ case lookupPath wrong files of
            Unique _ -> Nothing
            Nearby cs -> Just (Guidance "file-not-found" (candidateMessage wrong cs))
            NoneNearby -> Just (Guidance "file-not-found" (noneMessage wrong))

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

-- | The holistic runtime-exception text a failed run carries, if any.
runtimeErrorOf :: Either Text ExecutionResult -> Maybe Text
runtimeErrorOf (Left e) = Just e
runtimeErrorOf (Right er) = erError er

{- | Every file under the work dir, as work-dir-relative paths, skipping
dotfiles/dirs and the usual build-output trees. Bounded in depth and count
so a huge tree cannot make a single failed cell slow.
-}
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
