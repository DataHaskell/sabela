{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.Session.TryCache (
    CacheKey,
    cacheKeyText,
    CacheOutcome (..),
    CacheEntry (..),
    tryCacheMaxEntries,
    tryCacheRoot,
    acquireCacheEntry,
    commitCacheEntry,
    discardCacheEntry,
    shelveCacheEntry,
    resolvedGhcVersion,
) where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Hashable (hash)
import Data.List (sort, sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime)
import Numeric (showHex)
import ScriptHs.Parser (CabalMeta (..), SourceRepoPin (..))
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getModificationTime,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
 )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

newtype CacheKey = CacheKey Text
    deriving (Eq, Show)

cacheKeyText :: CabalMeta -> Text -> CacheKey
cacheKeyText meta ghcVersion =
    CacheKey . T.intercalate "\n" $
        [ field "deps" (metaDeps meta)
        , field "exts" (metaExts meta)
        , field "opts" (metaGhcOptions meta)
        , field "libdirs" (metaExtraLibDirs meta)
        , field "incdirs" (metaExtraIncludeDirs meta)
        , field "pkgs" (metaPackages meta)
        , field "repos" (map repoText (metaSourceRepos meta))
        , "ghc:" <> ghcVersion
        ]
  where
    field label xs = label <> ":" <> T.intercalate "," (sort xs)
    repoText r =
        T.intercalate "@" (srpLocation r : srpRef r : maybe [] pure (srpSubdir r))

data CacheOutcome = CacheHit | CacheMiss
    deriving (Eq, Show)

data CacheEntry = CacheEntry
    { ceOutcome :: CacheOutcome
    , ceBucketDir :: FilePath
    , ceProjectDir :: FilePath
    , ceStoreDir :: FilePath
    }
    deriving (Eq, Show)

tryCacheMaxEntries :: Int
tryCacheMaxEntries = 6

tryCacheRoot :: FilePath -> FilePath
tryCacheRoot tmpDir = tmpDir </> "try-cache"

completeMarker, keyFile :: FilePath -> FilePath
completeMarker dir = dir </> ".complete"
keyFile dir = dir </> "key.txt"

bucketDirFor :: FilePath -> CacheKey -> FilePath
bucketDirFor root (CacheKey k) =
    root </> ("env-" <> showHex (fromIntegral (hash k) :: Word) "")

acquireCacheEntry :: FilePath -> CacheKey -> IO CacheEntry
acquireCacheEntry root key@(CacheKey keyText) = do
    createDirectoryIfMissing True root
    let dir = bucketDirFor root key
        entry outcome = CacheEntry outcome dir (dir </> "project") (dir </> "cabal-store")
    hit <- isValidHit dir keyText
    if hit
        then touchComplete dir >> pure (entry CacheHit)
        else do
            resetBucket dir
            createDirectoryIfMissing True dir
            TIO.writeFile (keyFile dir) keyText
            pure (entry CacheMiss)

isValidHit :: FilePath -> Text -> IO Bool
isValidHit dir keyText = do
    complete <- doesFileExist (completeMarker dir)
    if not complete
        then pure False
        else do
            stored <- try (TIO.readFile (keyFile dir)) :: IO (Either SomeException Text)
            pure (either (const False) (== keyText) stored)

resetBucket :: FilePath -> IO ()
resetBucket dir = do
    exists <- doesDirectoryExist dir
    when exists $ do
        removeQuietly (completeMarker dir)
        removeTreeQuietly (dir </> "project")

removeQuietly :: FilePath -> IO ()
removeQuietly f = do
    exists <- doesFileExist f
    when exists $ do
        r <- try (removeFile f) :: IO (Either SomeException ())
        either (const (pure ())) pure r

removeTreeQuietly :: FilePath -> IO ()
removeTreeQuietly d = do
    exists <- doesDirectoryExist d
    when exists $ do
        r <- try (removeDirectoryRecursive d) :: IO (Either SomeException ())
        either (const (pure ())) pure r

touchComplete :: FilePath -> IO ()
touchComplete dir = TIO.writeFile (completeMarker dir) ""

commitCacheEntry :: FilePath -> FilePath -> Int -> IO ()
commitCacheEntry root dir maxEntries = do
    touchComplete dir
    evictOldest root maxEntries

discardCacheEntry :: FilePath -> IO ()
discardCacheEntry = removeTreeQuietly

shelveCacheEntry :: FilePath -> IO ()
shelveCacheEntry dir = do
    exists <- doesDirectoryExist dir
    when exists (removeQuietly (completeMarker dir))

evictOldest :: FilePath -> Int -> IO ()
evictOldest root maxEntries = do
    entries <- listCompletedBuckets root
    let oldestFirst = sortOn snd entries
        excess = length oldestFirst - maxEntries
    when (excess > 0) $
        mapM_ (discardCacheEntry . fst) (take excess oldestFirst)

listCompletedBuckets :: FilePath -> IO [(FilePath, UTCTime)]
listCompletedBuckets root = do
    exists <- doesDirectoryExist root
    if not exists
        then pure []
        else do
            names <- listDirectory root
            catMaybes <$> mapM stat names
  where
    stat name = do
        let dir = root </> name
        complete <- doesFileExist (completeMarker dir)
        if complete
            then Just . (,) dir <$> getModificationTime (completeMarker dir)
            else pure Nothing

resolvedGhcVersion :: IO Text
resolvedGhcVersion = do
    ghc <- fromMaybe "ghc" <$> lookupEnv "GHC"
    res <-
        try (readProcessWithExitCode ghc ["--numeric-version"] "") ::
            IO (Either SomeException (ExitCode, String, String))
    pure $ case res of
        Right (ExitSuccess, out, _) -> T.strip (T.pack out)
        _ -> "unknown"
