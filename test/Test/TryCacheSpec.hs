{-# LANGUAGE OverloadedStrings #-}

{- | Unit-level coverage of 'Sabela.Session.TryCache': the disposable-route
build cache never touches cabal itself, so these run purely against the
filesystem and stay fast. 'Test.MaterializeSpec' covers the live-build
integration (warm-hit timing, budget breach) end to end.
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

spec :: Spec
spec = describe "Sabela.Session.TryCache" $ do
    {- live_test21-23: `dataframe` never installed. A budget-breached build
    was torn down store and all, and the next attempt's acquire wiped the
    bucket again — so every attempt rebuilt from empty and re-breached the
    same ceiling. A heavy dependency was not slow, it was unreachable. -}
    describe "heavy-dep-never-converges" $ do
        it "a budget breach keeps the store so the next attempt resumes" $
            withSystemTempDirectory "try-cache-spec" $ \root -> do
                let key = cacheKeyText (metaWithDeps ["dataframe"]) "9.12.2"
                first <- acquireCacheEntry root key
                createDirectoryIfMissing True (ceStoreDir first)
                writeFile (ceStoreDir first </> "built.pkg") "partial build"

                shelveCacheEntry (ceBucketDir first)

                -- Never served as a usable environment ...
                second <- acquireCacheEntry root key
                ceOutcome second `shouldBe` CacheMiss
                -- ... but the partial build survives for it to resume from.
                doesFileExist (ceStoreDir second </> "built.pkg")
                    `shouldReturn` True

        it "acquiring a miss preserves the store, clearing only the project" $
            withSystemTempDirectory "try-cache-spec" $ \root -> do
                let key = cacheKeyText (metaWithDeps ["dataframe"]) "9.12.2"
                first <- acquireCacheEntry root key
                createDirectoryIfMissing True (ceStoreDir first)
                createDirectoryIfMissing True (ceProjectDir first)
                writeFile (ceStoreDir first </> "built.pkg") "partial build"
                writeFile (ceProjectDir first </> "stale.cabal") "stale"

                second <- acquireCacheEntry root key
                doesFileExist (ceStoreDir second </> "built.pkg")
                    `shouldReturn` True
                doesFileExist (ceProjectDir second </> "stale.cabal")
                    `shouldReturn` False

        it "a build that THREW is still torn down store and all" $
            withSystemTempDirectory "try-cache-spec" $ \root -> do
                let key = cacheKeyText (metaWithDeps ["dataframe"]) "9.12.2"
                first <- acquireCacheEntry root key
                createDirectoryIfMissing True (ceStoreDir first)
                writeFile (ceStoreDir first </> "built.pkg") "possibly corrupt"

                discardCacheEntry (ceBucketDir first)

                doesDirectoryExist (ceBucketDir first) `shouldReturn` False

    it "misses the first time and hits once the build is committed" $
        withSystemTempDirectory "try-cache-spec" $ \root -> do
            let key = cacheKeyText (metaWithDeps ["aeson"]) "9.12.2"
            first <- acquireCacheEntry root key
            ceOutcome first `shouldBe` CacheMiss
            doesDirectoryExist (ceBucketDir first) `shouldReturn` True

            commitCacheEntry root (ceBucketDir first) tryCacheMaxEntries

            second <- acquireCacheEntry root key
            ceOutcome second `shouldBe` CacheHit
            ceBucketDir second `shouldBe` ceBucketDir first
            ceProjectDir second `shouldBe` ceProjectDir first
            ceStoreDir second `shouldBe` ceStoreDir first

    it "misses for a dependency set that differs from a committed one" $
        withSystemTempDirectory "try-cache-spec" $ \root -> do
            let keyA = cacheKeyText (metaWithDeps ["aeson"]) "9.12.2"
                keyB = cacheKeyText (metaWithDeps ["containers"]) "9.12.2"
            a <- acquireCacheEntry root keyA
            commitCacheEntry root (ceBucketDir a) tryCacheMaxEntries

            b <- acquireCacheEntry root keyB
            ceOutcome b `shouldBe` CacheMiss
            ceBucketDir b `shouldNotBe` ceBucketDir a

    it "misses for the same dependencies under a different GHC version" $
        withSystemTempDirectory "try-cache-spec" $ \root -> do
            let keyOld = cacheKeyText (metaWithDeps ["aeson"]) "9.6.4"
                keyNew = cacheKeyText (metaWithDeps ["aeson"]) "9.12.2"
            a <- acquireCacheEntry root keyOld
            commitCacheEntry root (ceBucketDir a) tryCacheMaxEntries

            b <- acquireCacheEntry root keyNew
            ceOutcome b `shouldBe` CacheMiss

    it "never reuses an uncommitted (build-in-progress or failed) bucket" $
        withSystemTempDirectory "try-cache-spec" $ \root -> do
            let key = cacheKeyText (metaWithDeps ["text"]) "9.12.2"
            first <- acquireCacheEntry root key
            -- No commitCacheEntry: the "build" never finished.
            second <- acquireCacheEntry root key
            ceOutcome second `shouldBe` CacheMiss
            ceBucketDir second `shouldBe` ceBucketDir first

    it "rebuilds correctly after an entry is evicted, never serving it stale" $
        withSystemTempDirectory "try-cache-spec" $ \root -> do
            let keys =
                    [ cacheKeyText (metaWithDeps ["dep-" ++ show n]) "9.12.2"
                    | n <- [1 .. tryCacheMaxEntries + 1 :: Int]
                    ]
            case keys of
                [] -> expectationFailure "tryCacheMaxEntries + 1 keys is never empty"
                oldestKey : _ -> do
                    bucketDirs <- forM keys $ \k -> do
                        e <- acquireCacheEntry root k
                        commitCacheEntry root (ceBucketDir e) tryCacheMaxEntries
                        pure (ceBucketDir e)
                    case bucketDirs of
                        [] -> expectationFailure "unreachable: same length as keys"
                        oldestDir : _ -> do
                            -- The oldest (first-committed) key was pushed out by the cap.
                            doesFileExist (oldestDir </> ".complete") `shouldReturn` False

                            rebuilt <- acquireCacheEntry root oldestKey
                            ceOutcome rebuilt `shouldBe` CacheMiss
                            doesDirectoryExist (ceBucketDir rebuilt) `shouldReturn` True

                            commitCacheEntry root (ceBucketDir rebuilt) tryCacheMaxEntries
                            reHit <- acquireCacheEntry root oldestKey
                            ceOutcome reHit `shouldBe` CacheHit

    it "keeps the cache at or under the configured cap after eviction" $
        withSystemTempDirectory "try-cache-spec" $ \root -> do
            let keys =
                    [ cacheKeyText (metaWithDeps ["dep-" ++ show n]) "9.12.2"
                    | n <- [1 .. tryCacheMaxEntries + 3 :: Int]
                    ]
            forM_ keys $ \k -> do
                e <- acquireCacheEntry root k
                commitCacheEntry root (ceBucketDir e) tryCacheMaxEntries

            hits <- forM keys (fmap ceOutcome . acquireCacheEntry root)
            length (filter (== CacheHit) hits) `shouldBe` tryCacheMaxEntries

    it "tears a discarded bucket down cleanly, leaving no orphan directory" $
        withSystemTempDirectory "try-cache-spec" $ \root -> do
            let key = cacheKeyText (metaWithDeps ["hasktorch"]) "9.12.2"
            entry <- acquireCacheEntry root key
            doesDirectoryExist (ceBucketDir entry) `shouldReturn` True

            discardCacheEntry (ceBucketDir entry)
            doesDirectoryExist (ceBucketDir entry) `shouldReturn` False

            retry <- acquireCacheEntry root key
            ceOutcome retry `shouldBe` CacheMiss

    it "never places a bucket outside the supplied cache root" $
        withSystemTempDirectory "try-cache-spec" $ \root -> do
            let key = cacheKeyText (metaWithDeps ["aeson"]) "9.12.2"
            entry <- acquireCacheEntry root key
            ceBucketDir entry `shouldSatisfy` (root `isPrefixOfPath`)
            ceProjectDir entry `shouldSatisfy` (root `isPrefixOfPath`)
            ceStoreDir entry `shouldSatisfy` (root `isPrefixOfPath`)
  where
    isPrefixOfPath prefix path = take (length prefix) path == prefix
