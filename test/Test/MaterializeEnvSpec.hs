{-# LANGUAGE OverloadedStrings #-}

{- | Shared-store materialization: bucket-cold trials ride the warm global
cabal store, and concurrent different-key trials share it safely. These
live tests build against the developer's real store by design.
-}
module Test.MaterializeEnvSpec (spec) where

import Control.Concurrent (forkFinally, readMVar)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Data.Set as Set
import GHC.Clock (getMonotonicTimeNSec)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Sabela.Server (newApp)
import Sabela.Session.Materialize
import Sabela.Session.Project (buildTimeSupportDir)
import Test.Materialize.Helpers (
    newPackageCandidate,
    nsToSeconds,
    packagesCandidate,
    requireCompleted,
    requireLiveIntegration,
 )

spec :: Spec
spec = describe "shared-store materialization" $ do
    it "a bucket-cold trial against a warm store completes quickly" $
        withSystemTempDirectory "sabela-materialize-warm-store" $ \workDir -> do
            requireLiveIntegration
            app <- newApp workDir Set.empty Nothing Nothing [buildTimeSupportDir]
            start <- getMonotonicTimeNSec
            result <-
                requireCompleted (runDisposableTry app (newPackageCandidate "split"))
            end <- getMonotonicTimeNSec
            disposableVerdict result `shouldBe` DisposableOk
            nsToSeconds (end - start) `shouldSatisfy` (< 30)

    it "two different-key trials sharing a dependency both succeed concurrently" $
        withSystemTempDirectory "sabela-materialize-concurrent" $ \workDir -> do
            requireLiveIntegration
            app <- newApp workDir Set.empty Nothing Nothing [buildTimeSupportDir]
            go <- newEmptyMVar
            aVar <- newEmptyMVar
            bVar <- newEmptyMVar
            let child var pkgs =
                    forkFinally
                        ( readMVar go
                            >> requireCompleted
                                (runDisposableTry app (packagesCandidate pkgs))
                        )
                        (putMVar var)
            _ <- child aVar ["split"]
            _ <- child bVar ["split", "containers"]
            putMVar go ()
            a <- takeMVar aVar
            b <- takeMVar bVar
            let verdictOf =
                    either
                        (const Nothing)
                        (Just . disposableVerdict)
            verdictOf a `shouldBe` Just DisposableOk
            verdictOf b `shouldBe` Just DisposableOk
