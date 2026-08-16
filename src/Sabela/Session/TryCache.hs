{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Per-dependency-set trial environments, keyed canonically and mutated
only under a bucket lease. Compiled deps live in the shared cabal store, so
a bucket holds only the resolved plan and the local shim build.
-}
module Sabela.Session.TryCache (
    CacheKey,
    cacheKeyRaw,
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
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import ScriptHs.Parser (CabalMeta)
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
 )
import System.FilePath (takeDirectory, takeFileName, (</>))

import Sabela.Session.EnvKey (canonicalKeyText)
import Sabela.Session.GhcProbe (resolvedGhcVersion)
import Sabela.Session.TryCache.Lease (
    Lease,
    leaseBucketDir,
    leaseKeyText,
    withCandidateLease,
 )
import Sabela.Session.Workspace (buildIsDirty)

newtype CacheKey = CacheKey Text
    deriving (Eq, Show)

-- | The canonical identity: resolved local packages + metadata + compiler.
cacheKeyText :: [FilePath] -> CabalMeta -> Text -> CacheKey
cacheKeyText localPkgs meta ghcVersion =
    CacheKey (canonicalKeyText localPkgs meta ghcVersion)

cacheKeyRaw :: CacheKey -> Text
cacheKeyRaw (CacheKey k) = k

data CacheOutcome = CacheHit | CacheMiss
    deriving (Eq, Show)

data CacheEntry = CacheEntry
    { ceOutcome :: CacheOutcome
    , ceBucketDir :: FilePath
    , ceProjectDir :: FilePath
    }
    deriving (Eq, Show)

tryCacheMaxEntries :: Int
tryCacheMaxEntries = 24

tryCacheRoot :: FilePath -> FilePath
tryCacheRoot tmpDir = tmpDir </> "try-cache"

completeMarker, keyFile :: FilePath -> FilePath
completeMarker dir = dir </> ".complete"
keyFile dir = dir </> "key.txt"

{- | The held lease's bucket, validated UNDER the lease: a hit only for a
committed, key-matching, clean bucket; anything else resets to a miss. A
waiter re-validates here, so a bucket that changed mid-wait answers honestly.
-}
acquireCacheEntry :: Lease -> IO CacheEntry
acquireCacheEntry lease = do
    let dir = leaseBucketDir lease
        entry outcome = CacheEntry outcome dir (dir </> "project")
    hit <- isValidHit dir (leaseKeyText lease)
    if hit
        then touchComplete dir >> pure (entry CacheHit)
        else do
            resetBucket dir
            createDirectoryIfMissing True dir
            TIO.writeFile (keyFile dir) (leaseKeyText lease)
            pure (entry CacheMiss)

{- | Committed, same key, and not dirty: a crash between spawn and a clean
quit leaves the dirty marker, and such artifacts are never served as a hit.
-}
isValidHit :: FilePath -> Text -> IO Bool
isValidHit dir keyText = do
    complete <- doesFileExist (completeMarker dir)
    if not complete
        then pure False
        else do
            stored <- try (TIO.readFile (keyFile dir)) :: IO (Either SomeException Text)
            case stored of
                Right k | k == keyText -> not <$> buildIsDirty (dir </> "project")
                _ -> pure False

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

{- | The marker doubles as the LRU stamp: nanosecond wall-clock content,
because filesystem mtimes tie under rapid commits and coarse filesystems.
-}
touchComplete :: FilePath -> IO ()
touchComplete dir = do
    ns <- round . (* 1e9) <$> getPOSIXTime :: IO Integer
    TIO.writeFile (completeMarker dir) (T.pack (show ns))

commitCacheEntry :: Lease -> Int -> IO ()
commitCacheEntry lease maxEntries = do
    touchComplete (leaseBucketDir lease)
    evictOldest (takeDirectory (leaseBucketDir lease)) maxEntries

discardCacheEntry :: Lease -> IO ()
discardCacheEntry = removeTreeQuietly . leaseBucketDir

shelveCacheEntry :: Lease -> IO ()
shelveCacheEntry lease = do
    let dir = leaseBucketDir lease
    exists <- doesDirectoryExist dir
    when exists (removeQuietly (completeMarker dir))

{- | Best-effort cap: each candidate is deleted only while HOLDING its lease
(non-blocking), so eviction can never race a concurrent acquire; whatever is
leased is skipped and the cap can transiently run over.
-}
evictOldest :: FilePath -> Int -> IO ()
evictOldest root maxEntries = do
    entries <- listCompletedBuckets root
    let oldestFirst = map fst (sortOn snd entries)
        excess = length entries - maxEntries
    when (excess > 0) (go excess oldestFirst)
  where
    go 0 _ = pure ()
    go _ [] = pure ()
    go n (dir : rest) = do
        deleted <-
            withCandidateLease root (takeFileName dir) (removeTreeQuietly dir)
        go (if deleted then n - 1 else n) rest

listCompletedBuckets :: FilePath -> IO [(FilePath, Integer)]
listCompletedBuckets root = do
    exists <- doesDirectoryExist root
    if not exists
        then pure []
        else do
            names <- listDirectory root
            catMaybes <$> mapM stat (filter isBucketName names)
  where
    isBucketName name = take 4 name == "env-"
    stat name = do
        let dir = root </> name
        complete <- doesFileExist (completeMarker dir)
        if complete
            then Just . (,) dir <$> stampOf dir
            else pure Nothing
    stampOf dir = do
        raw <- try (TIO.readFile (completeMarker dir)) :: IO (Either SomeException Text)
        pure $ case raw of
            Right t | [(n, _)] <- reads (T.unpack (T.strip t)) -> n
            _ -> 0
