{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Size-bounded, on-disk cache of built disposable package environments for
'Sabela.Session.Materialize.runDisposableTry'. A trial's Cabal metadata
(deps/extensions/options/repos + resolved GHC version) fully determines the
built @dist-newstyle@/store — the candidate code itself never enters the
Cabal project, only the interactive session — so two trials with the same
metadata can share one build. A cache hit reuses that build; the scratch
GHCi process spawned on top of it is still fresh every call.
-}
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
 )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

-- | A canonical, order-independent encoding of a disposable build environment.
newtype CacheKey = CacheKey Text
    deriving (Eq, Show)

{- | The dependency-relevant slice of 'CabalMeta' plus the resolved GHC
version. Local package /content/ and notebook cell source are deliberately
excluded: the candidate and notebook cells reach the scratch session over
the interactive protocol, never through the Cabal project, so they cannot
affect what gets built.
-}
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

{- | Bound the cache to a handful of built environments. One exploratory
session tries a small number of distinct dependency ideas, not dozens, and
each built environment (store + dist-newstyle) can run to hundreds of
megabytes; six caps worst-case disk use while comfortably covering a
session's working set.
-}
tryCacheMaxEntries :: Int
tryCacheMaxEntries = 6

-- | The cache root, under the caller's temp area, never under the work dir.
tryCacheRoot :: FilePath -> FilePath
tryCacheRoot tmpDir = tmpDir </> "try-cache"

completeMarker, keyFile :: FilePath -> FilePath
completeMarker dir = dir </> ".complete"
keyFile dir = dir </> "key.txt"

bucketDirFor :: FilePath -> CacheKey -> FilePath
bucketDirFor root (CacheKey k) =
    root </> ("env-" <> showHex (fromIntegral (hash k) :: Word) "")

{- | Look up a build for 'key' under 'root'. A hit only counts when the
bucket is marked complete (a prior build actually finished) and its stored
key matches exactly — collisions in the directory-naming hash degrade to a
miss rather than silently serving the wrong environment. A miss clears any
stale/partial contents so the caller starts a genuinely fresh build.
-}
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
    when exists (removeDirectoryRecursive dir)

touchComplete :: FilePath -> IO ()
touchComplete dir = TIO.writeFile (completeMarker dir) ""

{- | Mark a build as successfully finished (the LRU touch for this entry) and
evict buckets beyond 'maxEntries', oldest access first. Call only after the
scratch session actually spawned on top of the build — never on a build
that timed out or threw, so a broken build can never masquerade as a hit.
-}
commitCacheEntry :: FilePath -> FilePath -> Int -> IO ()
commitCacheEntry root dir maxEntries = do
    touchComplete dir
    evictOldest root maxEntries

-- | Tear a bucket down entirely: used when its build breaches budget or
-- throws, so the next attempt for that key starts from a clean slate.
discardCacheEntry :: FilePath -> IO ()
discardCacheEntry dir = do
    exists <- doesDirectoryExist dir
    when exists $ do
        r <- try (removeDirectoryRecursive dir) :: IO (Either SomeException ())
        either (const (pure ())) pure r

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

-- | @ghc --numeric-version@, honouring the same @GHC@ override as the
-- session spawner; "unknown" on any failure keeps caching safe (a bad
-- version string just changes the cache key, it never crashes the trial).
resolvedGhcVersion :: IO Text
resolvedGhcVersion = do
    ghc <- fromMaybe "ghc" <$> lookupEnv "GHC"
    res <-
        try (readProcessWithExitCode ghc ["--numeric-version"] "") ::
            IO (Either SomeException (ExitCode, String, String))
    pure $ case res of
        Right (ExitSuccess, out, _) -> T.strip (T.pack out)
        _ -> "unknown"
