{-# LANGUAGE OverloadedStrings #-}

{- | Network-dependent read_source checks: one real Hackage fetch into a
temporary mirror, and its idempotence. Gated by 'liveSpecs'.
-}
module Test.ReadSourceLiveSpec (spec) where

import Control.Exception (bracket)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    getTemporaryDirectory,
    removeDirectoryRecursive,
 )
import System.FilePath ((</>))
import Test.Hspec

import Sabela.AI.Sdist (SdistProvenance (..), acquireSdist)
import Test.Live (liveSpecs)
import Test.WorldFixtures (withEnvVars)

{- | An empty temp world for both caches, so the fetch cannot be satisfied
by (or write into) this machine's real ones.
-}
withEmptyCaches :: IO a -> IO a
withEmptyCaches act = bracket acquire removeDirectoryRecursive inWorld
  where
    acquire = do
        tmp <- getTemporaryDirectory
        let root = tmp </> "sabela-sdist-live-spec"
        createDirectoryIfMissing True (root </> "cabal")
        createDirectoryIfMissing True (root </> "sdists")
        pure root
    inWorld root =
        withEnvVars
            [ ("SABELA_CABAL_PACKAGES_DIR", root </> "cabal")
            , ("SABELA_SDIST_CACHE_DIR", root </> "sdists")
            ]
            act

spec :: Spec
spec = liveSpecs $
    describe "read_source against real Hackage" $
        it "fetches a small sdist once, then reads its own mirror" $
            withEmptyCaches $ do
                mgr <- newManager tlsManagerSettings
                first <- acquireSdist (Just mgr) "data-ordlist" "0.4.7.0"
                fmap fst first `shouldBe` Right Fetched
                tmp <- getTemporaryDirectory
                let cached =
                        tmp
                            </> "sabela-sdist-live-spec"
                            </> "sdists"
                            </> "data-ordlist-0.4.7.0.tar.gz"
                doesFileExist cached `shouldReturn` True
                second <- acquireSdist (Just mgr) "data-ordlist" "0.4.7.0"
                fmap fst second `shouldBe` Right CacheLocal
