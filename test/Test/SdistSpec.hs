{-# LANGUAGE OverloadedStrings #-}

module Test.SdistSpec (spec) where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import System.Directory (
    createDirectoryIfMissing,
    getTemporaryDirectory,
    removeDirectoryRecursive,
 )
import System.FilePath ((</>))
import Test.Hspec

import Sabela.AI.Sdist (
    SdistProvenance (..),
    acquireSdist,
    cabalCacheDir,
    cachedVersions,
    drainToCap,
    maxSdistBytes,
    sdistUrl,
 )
import Sabela.AI.VersionKey (versionKey)
import Test.WorldFixtures (sdistArchive, withEnvVars)

-- | A one-file gzipped tarball, enough for the acquisition ladder to hand on.
tinySdist :: Text -> Text -> BL.ByteString
tinySdist pkg ver =
    sdistArchive
        [(T.unpack (pkg <> "-" <> ver) <> "/Probe.hs", "module Probe where\n")]

{- | A temp world holding both cache shapes: the cabal package cache's
@pkg/ver/pkg-ver.tar.gz@ and our flat sdist mirror.
-}
withCaches :: (FilePath -> FilePath -> IO a) -> IO a
withCaches act = bracket acquire removeDirectoryRecursive body
  where
    acquire = do
        tmp <- getTemporaryDirectory
        let root = tmp </> "sabela-sdist-spec"
            cabalDir = root </> "cabal-packages"
            mirrorDir = root </> "sdists"
        createDirectoryIfMissing True (cabalDir </> "aeson" </> "2.2.3.0")
        createDirectoryIfMissing True (cabalDir </> "aeson" </> "2.1.0.0")
        createDirectoryIfMissing True mirrorDir
        BL.writeFile
            (cabalDir </> "aeson" </> "2.2.3.0" </> "aeson-2.2.3.0.tar.gz")
            (tinySdist "aeson" "2.2.3.0")
        BL.writeFile
            (cabalDir </> "aeson" </> "preferred-versions")
            "aeson <2.3\n"
        BL.writeFile
            (mirrorDir </> "vapour-1.2.3.tar.gz")
            (tinySdist "vapour" "1.2.3")
        pure root
    body root =
        withEnvVars
            [ ("SABELA_CABAL_PACKAGES_DIR", root </> "cabal-packages")
            , ("SABELA_SDIST_CACHE_DIR", root </> "sdists")
            ]
            (act (root </> "cabal-packages") (root </> "sdists"))

spec :: Spec
spec = describe "sdist acquisition" $ do
    it "builds the one fixed-host Hackage URL" $
        sdistUrl "hodatime" "0.2.2.1"
            `shouldBe` "https://hackage.haskell.org/package/hodatime-0.2.2.1\
                       \/hodatime-0.2.2.1.tar.gz"

    it "never names a git host" $ do
        src <- readFile ("src" </> "Sabela" </> "AI" </> "Sdist.hs")
        src `shouldSatisfy` (not . T.isInfixOf "github.com" . T.pack)

    it "reads the cabal package cache first, without a manager" $
        withCaches $ \_ _ -> do
            r <- acquireSdist Nothing "aeson" "2.2.3.0"
            fmap fst r `shouldBe` Right CabalCache

    it "reads its own mirror when cabal has no copy" $
        withCaches $ \_ _ -> do
            r <- acquireSdist Nothing "vapour" "1.2.3"
            fmap fst r `shouldBe` Right CacheLocal

    it "without a manager, a cache miss names both caches and stops" $
        withCaches $ \cabalDir mirrorDir -> do
            r <- acquireSdist Nothing "hodatime" "0.2.2.1"
            case r of
                Left e -> do
                    e `shouldSatisfy` T.isInfixOf (T.pack cabalDir)
                    e `shouldSatisfy` T.isInfixOf (T.pack mirrorDir)
                Right _ -> expectationFailure "expected a miss"

    it "rejects a traversal-shaped package name before any path is built" $ do
        r <- acquireSdist Nothing "../evil" "1.0"
        case r of
            Left e -> do
                e `shouldSatisfy` T.isInfixOf "package name"
                e `shouldSatisfy` (not . T.isInfixOf "cache")
            Right _ -> expectationFailure "expected rejection"

    it "rejects a traversal-shaped version before any path is built" $ do
        r <- acquireSdist Nothing "aeson" "1.0/../.."
        case r of
            Left e -> do
                e `shouldSatisfy` T.isInfixOf "version"
                e `shouldSatisfy` (not . T.isInfixOf "cache")
            Right _ -> expectationFailure "expected rejection"

    it "SABELA_NO_NETWORK forbids the fetch even with a manager" $
        withCaches $ \_ _ -> do
            mgr <- newManager defaultManagerSettings
            r <-
                withEnvVars [("SABELA_NO_NETWORK", "1")] $
                    acquireSdist (Just mgr) "hodatime" "0.2.2.1"
            case r of
                Left e -> e `shouldSatisfy` T.isInfixOf "no network fetch"
                Right _ -> expectationFailure "expected the kill switch"

    it "lists the versions the cabal cache holds, newest first" $
        withCaches $ \_ _ ->
            cachedVersions "aeson" `shouldReturn` ["2.2.3.0", "2.1.0.0"]

    it "skips the preferred-versions file beside the version dirs" $
        withCaches $ \_ _ -> do
            vers <- cachedVersions "aeson"
            vers `shouldSatisfy` notElem "preferred-versions"

    it "lists nothing for a package the cache has never held" $
        withCaches $ \_ _ ->
            cachedVersions "nosuchpkg" `shouldReturn` []

    describe "cabalCacheDir" $ do
        it "honours CABAL_DIR when the override is absent" $ do
            dir <-
                withEnvVars
                    [ ("SABELA_CABAL_PACKAGES_DIR", "")
                    , ("CABAL_DIR", "/tmp/cabal-home")
                    ]
                    cabalCacheDir
            dir
                `shouldBe` ( "/tmp/cabal-home"
                                </> "packages"
                                </> "hackage.haskell.org"
                           )
        it "lets the explicit override beat CABAL_DIR" $ do
            dir <-
                withEnvVars
                    [ ("SABELA_CABAL_PACKAGES_DIR", "/tmp/override")
                    , ("CABAL_DIR", "/tmp/cabal-home")
                    ]
                    cabalCacheDir
            dir `shouldBe` "/tmp/override"

    describe "versionKey" $ do
        it "orders 0.10 above 0.9 numerically" $
            versionKey "0.10" `shouldSatisfy` (> versionKey "0.9")
        it "reads a malformed segment as 0" $
            versionKey "1.x.2" `shouldBe` [1, 0, 2]

    describe "the fetch cap" $ do
        it "rejects an over-cap body outright rather than truncating" $ do
            feeder <- chunkFeeder (replicate 3 (BS.replicate oversize 0))
            r <- drainToCap maxSdistBytes feeder
            case r of
                Left e -> e `shouldSatisfy` T.isInfixOf "cap"
                Right _ -> expectationFailure "expected the cap to reject"
        it "passes a body under the cap through whole" $ do
            feeder <- chunkFeeder [BS.replicate 1024 7]
            r <- drainToCap maxSdistBytes feeder
            fmap BL.length r `shouldBe` Right 1024
  where
    oversize = maxSdistBytes `div` 2 + 1

-- | Yields each chunk once, then empty — the shape http-client's brRead has.
chunkFeeder :: [BS.ByteString] -> IO (IO BS.ByteString)
chunkFeeder chunks = do
    ref <- newIORef chunks
    pure $ do
        cs <- readIORef ref
        case cs of
            [] -> pure BS.empty
            (c : rest) -> writeIORef ref rest >> pure c
