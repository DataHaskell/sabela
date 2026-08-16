{-# LANGUAGE OverloadedStrings #-}

{- | The try-cache contract under leases: canonical keys name buckets, every
mutation needs the lease token, a dirty bucket never answers as a hit, and
eviction spares whatever is leased.
-}
module Test.TryCacheSpec (spec) where

import Control.Monad (forM, forM_)
import qualified Data.Text as T
import ScriptHs.Parser (CabalMeta (..))
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
 )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Sabela.Session.TryCache
import Sabela.Session.TryCache.Lease (Lease, withBucketLease)
import Sabela.Session.Workspace (markBuildDirty)

emptyMeta :: CabalMeta
emptyMeta =
    CabalMeta
        { metaDeps = []
        , metaExts = []
        , metaGhcOptions = []
        , metaExtraLibDirs = []
        , metaExtraIncludeDirs = []
        , metaPackages = []
        , metaSourceRepos = []
        , metaUnknownKeys = []
        }

metaWithDeps :: [String] -> CabalMeta
metaWithDeps deps = emptyMeta{metaDeps = map T.pack deps}

keyFor :: [String] -> String -> CacheKey
keyFor deps ghc = cacheKeyText [] (metaWithDeps deps) (T.pack ghc)

withLease :: FilePath -> CacheKey -> (Lease -> IO a) -> IO a
withLease root key act =
    withBucketLease root (cacheKeyRaw key) (2 * 1000 * 1000) $
        maybe (fail "lease unexpectedly busy") act

spec :: Spec
spec = describe "Sabela.Session.TryCache" $ do
    it "misses the first time and hits once the build is committed" $
        withSystemTempDirectory "try-cache-spec" $ \root -> do
            let key = keyFor ["aeson"] "9.12.2"
            first <- withLease root key $ \l -> do
                e <- acquireCacheEntry l
                ceOutcome e `shouldBe` CacheMiss
                doesDirectoryExist (ceBucketDir e) `shouldReturn` True
                commitCacheEntry l tryCacheMaxEntries
                pure e
            withLease root key $ \l -> do
                second <- acquireCacheEntry l
                ceOutcome second `shouldBe` CacheHit
                ceBucketDir second `shouldBe` ceBucketDir first
                ceProjectDir second `shouldBe` ceProjectDir first

    it "spelling variants of one dependency set share a bucket" $
        withSystemTempDirectory "try-cache-spec" $ \root -> do
            let spelledA = keyFor ["containers ==0.6.7", "text"] "9.12.2"
                spelledB = keyFor ["text", "containers==0.6.7"] "9.12.2"
            a <- withLease root spelledA $ \l -> do
                e <- acquireCacheEntry l
                commitCacheEntry l tryCacheMaxEntries
                pure e
            withLease root spelledB $ \l -> do
                b <- acquireCacheEntry l
                ceOutcome b `shouldBe` CacheHit
                ceBucketDir b `shouldBe` ceBucketDir a

    it "misses for a dependency set that differs from a committed one" $
        withSystemTempDirectory "try-cache-spec" $ \root -> do
            let keyA = keyFor ["aeson"] "9.12.2"
                keyB = keyFor ["containers"] "9.12.2"
            a <- withLease root keyA $ \l -> do
                e <- acquireCacheEntry l
                commitCacheEntry l tryCacheMaxEntries
                pure e
            withLease root keyB $ \l -> do
                b <- acquireCacheEntry l
                ceOutcome b `shouldBe` CacheMiss
                ceBucketDir b `shouldNotBe` ceBucketDir a

    it "misses for the same dependencies under a different GHC version" $
        withSystemTempDirectory "try-cache-spec" $ \root -> do
            let keyOld = keyFor ["aeson"] "9.6.4"
                keyNew = keyFor ["aeson"] "9.12.2"
            withLease root keyOld $ \l -> do
                _ <- acquireCacheEntry l
                commitCacheEntry l tryCacheMaxEntries
            withLease root keyNew $ \l -> do
                b <- acquireCacheEntry l
                ceOutcome b `shouldBe` CacheMiss

    it "a shelved (budget-breached) bucket answers the next acquire as a miss" $
        withSystemTempDirectory "try-cache-spec" $ \root -> do
            let key = keyFor ["dataframe"] "9.12.2"
            withLease root key $ \l -> do
                e <- acquireCacheEntry l
                createDirectoryIfMissing True (ceProjectDir e)
                writeFile (ceProjectDir e </> "partial.txt") "partial"
                shelveCacheEntry l
            withLease root key $ \l -> do
                second <- acquireCacheEntry l
                ceOutcome second `shouldBe` CacheMiss
                doesFileExist (ceProjectDir second </> "partial.txt")
                    `shouldReturn` False

    it "a committed bucket whose project is DIRTY answers as a miss, reset" $
        withSystemTempDirectory "try-cache-spec" $ \root -> do
            let key = keyFor ["vector"] "9.12.2"
            withLease root key $ \l -> do
                e <- acquireCacheEntry l
                createDirectoryIfMissing True (ceProjectDir e)
                writeFile (ceProjectDir e </> "stale.o") "truncated artifact"
                commitCacheEntry l tryCacheMaxEntries
                markBuildDirty (ceProjectDir e)
            withLease root key $ \l -> do
                second <- acquireCacheEntry l
                ceOutcome second `shouldBe` CacheMiss
                doesFileExist (ceProjectDir second </> "stale.o")
                    `shouldReturn` False

    it "a discarded bucket is gone and the next acquire rebuilds it" $
        withSystemTempDirectory "try-cache-spec" $ \root -> do
            let key = keyFor ["hasktorch"] "9.12.2"
            bucket <- withLease root key $ \l -> do
                e <- acquireCacheEntry l
                discardCacheEntry l
                pure (ceBucketDir e)
            doesDirectoryExist bucket `shouldReturn` False
            withLease root key $ \l -> do
                retry <- acquireCacheEntry l
                ceOutcome retry `shouldBe` CacheMiss
                doesDirectoryExist (ceBucketDir retry) `shouldReturn` True

    it "rebuilds correctly after an entry is evicted, never serving it stale" $
        withSystemTempDirectory "try-cache-spec" $ \root -> do
            let keys =
                    [ keyFor ["dep-" ++ show n] "9.12.2"
                    | n <- [1 .. tryCacheMaxEntries + 1 :: Int]
                    ]
            case keys of
                [] -> expectationFailure "tryCacheMaxEntries + 1 keys is never empty"
                oldestKey : _ -> do
                    bucketDirs <- forM keys $ \k -> withLease root k $ \l -> do
                        e <- acquireCacheEntry l
                        commitCacheEntry l tryCacheMaxEntries
                        pure (ceBucketDir e)
                    case bucketDirs of
                        [] -> expectationFailure "unreachable: same length as keys"
                        oldestDir : _ -> do
                            doesFileExist (oldestDir </> ".complete")
                                `shouldReturn` False
                            withLease root oldestKey $ \l -> do
                                rebuilt <- acquireCacheEntry l
                                ceOutcome rebuilt `shouldBe` CacheMiss
                                commitCacheEntry l tryCacheMaxEntries
                            withLease root oldestKey $ \l -> do
                                reHit <- acquireCacheEntry l
                                ceOutcome reHit `shouldBe` CacheHit

    it "keeps the cache at or under the configured cap after eviction" $
        withSystemTempDirectory "try-cache-spec" $ \root -> do
            let keys =
                    [ keyFor ["dep-" ++ show n] "9.12.2"
                    | n <- [1 .. tryCacheMaxEntries + 3 :: Int]
                    ]
            forM_ keys $ \k -> withLease root k $ \l -> do
                _ <- acquireCacheEntry l
                commitCacheEntry l tryCacheMaxEntries
            hits <- forM keys $ \k -> withLease root k (fmap ceOutcome . acquireCacheEntry)
            length (filter (== CacheHit) hits) `shouldBe` tryCacheMaxEntries

    it "eviction spares a bucket whose lease is held" $
        withSystemTempDirectory "try-cache-spec" $ \root -> do
            let heldKey = keyFor ["held"] "9.12.2"
                keys =
                    [ keyFor ["dep-" ++ show n] "9.12.2"
                    | n <- [1 .. tryCacheMaxEntries + 2 :: Int]
                    ]
            heldBucket <- withLease root heldKey $ \heldLease -> do
                e <- acquireCacheEntry heldLease
                commitCacheEntry heldLease tryCacheMaxEntries
                forM_ keys $ \k -> withLease root k $ \l -> do
                    _ <- acquireCacheEntry l
                    commitCacheEntry l tryCacheMaxEntries
                pure (ceBucketDir e)
            doesDirectoryExist heldBucket `shouldReturn` True
            withLease root heldKey $ \l -> do
                back <- acquireCacheEntry l
                ceOutcome back `shouldBe` CacheHit

    it "never places a bucket outside the supplied cache root" $
        withSystemTempDirectory "try-cache-spec" $ \root -> do
            let key = keyFor ["aeson"] "9.12.2"
            withLease root key $ \l -> do
                entry <- acquireCacheEntry l
                ceBucketDir entry `shouldSatisfy` (root `isPrefixOfPath`)
                ceProjectDir entry `shouldSatisfy` (root `isPrefixOfPath`)
  where
    isPrefixOfPath prefix path = take (length prefix) path == prefix
