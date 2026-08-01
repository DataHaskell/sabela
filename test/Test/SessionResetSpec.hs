{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.SessionResetSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (modifyMVar_, newEmptyMVar, putMVar, takeMVar)
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as Set
import Sabela.Deps (EnvSig, collectMetadata, envSig)
import Sabela.Model (Notebook (..))
import Sabela.State.SessionManager (
    SessionManager (..),
    currentKernelEpoch,
    forceResetAllSessions,
    getHaskellSession,
    haskellEnvOf,
    installHaskellSession,
    newSessionManager,
    recordHaskellEnv,
    setHaskellSession,
    takeHaskellSession,
 )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.WriteAckFixture (inertBackend)

-- | Any valid signature; these tests are about the epoch, not the environment.
anySig :: IO EnvSig
anySig = pure (envSig Set.empty [] (collectMetadata (Notebook "t" [])))

withInstalledSession :: IO SessionManager
withInstalledSession = do
    sm <- newSessionManager
    backend <- inertBackend
    setHaskellSession sm (Just backend)
    installed <- getHaskellSession sm
    isJust installed `shouldBe` True
    pure sm

spec :: Spec
spec = do
    describe "the kernel epoch (what a client compares to spot a restart)" $ do
        it "is 0 before any kernel has run" $ do
            sm <- newSessionManager
            currentKernelEpoch sm >>= (`shouldBe` 0)

        it "advances when a kernel is installed" $ do
            sm <- newSessionManager
            before <- currentKernelEpoch sm
            backend <- inertBackend
            installHaskellSession sm backend =<< anySig
            after <- currentKernelEpoch sm
            (after > before) `shouldBe` True

        it
            "advances AGAIN on the next restart: it counted the kernel's own\
            \ generation before, which is 1 for every freshly spawned process,\
            \ so two restarts were indistinguishable"
            $ do
                sm <- newSessionManager
                sig <- anySig
                first <- inertBackend
                installHaskellSession sm first sig
                one <- currentKernelEpoch sm
                second <- inertBackend
                installHaskellSession sm second sig
                two <- currentKernelEpoch sm
                (two > one) `shouldBe` True

        it "does not advance when a kernel merely goes away" $ do
            sm <- newSessionManager
            backend <- inertBackend
            installHaskellSession sm backend =<< anySig
            installed <- currentKernelEpoch sm
            _ <- takeHaskellSession sm
            currentKernelEpoch sm >>= (`shouldBe` installed)
    describe "smHaskellEnv (what the running kernel was built from)" $ do
        let sig = envSig Set.empty [] (collectMetadata (Notebook "t" []))
        it "is empty before any kernel exists" $ do
            sm <- newSessionManager
            haskellEnvOf sm >>= (`shouldBe` True) . isNothing

        it "records the signature against the kernel that was installed" $ do
            sm <- newSessionManager
            backend <- inertBackend
            setHaskellSession sm (Just backend)
            recordHaskellEnv sm backend sig
            recorded <- haskellEnvOf sm
            fmap snd recorded `shouldBe` Just sig

        it "is cleared when the session is taken, so a dead kernel cannot look current" $ do
            sm <- newSessionManager
            backend <- inertBackend
            setHaskellSession sm (Just backend)
            recordHaskellEnv sm backend sig
            _ <- takeHaskellSession sm
            haskellEnvOf sm >>= (`shouldBe` True) . isNothing

        it "is cleared by a force reset" $ do
            sm <- newSessionManager
            backend <- inertBackend
            setHaskellSession sm (Just backend)
            recordHaskellEnv sm backend sig
            forceResetAllSessions sm
            haskellEnvOf sm >>= (`shouldBe` True) . isNothing

    describe "forceResetAllSessions (a kill must actually kill)" $ do
        it "empties an idle slot" $ do
            sm <- withInstalledSession
            forceResetAllSessions sm
            getHaskellSession sm >>= (`shouldBe` True) . isNothing

        it "waits out a briefly contended slot instead of silently skipping" $ do
            sm <- withInstalledSession
            holding <- newEmptyMVar
            _ <- forkIO $
                modifyMVar_ (smHaskell sm) $ \v -> do
                    putMVar holding ()
                    threadDelay 150_000
                    pure v
            takeMVar holding
            forceResetAllSessions sm
            getHaskellSession sm >>= (`shouldBe` True) . isNothing
