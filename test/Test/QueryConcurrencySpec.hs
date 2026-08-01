{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.QueryConcurrencySpec (spec) where

import Control.Concurrent (
    forkIO,
    newEmptyMVar,
    putMVar,
    takeMVar,
    threadDelay,
    tryReadMVar,
 )
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent.STM (newTVarIO)
import Data.IORef (newIORef)
import Data.Maybe (isJust, isNothing)
import Data.Unique (newUnique)
import Sabela.Session (
    Session (..),
    SessionConfig (..),
    isBusy,
    withQueryLocks,
    withRunLock,
 )
import Sabela.Session.Proc (ProcSession (..))
import Sabela.Session.Reader (newOutQueue)
import System.Timeout (timeout)
import Test.Hspec (
    Spec,
    describe,
    it,
    shouldBe,
 )

defaultCfg :: SessionConfig
defaultCfg =
    SessionConfig
        { scProjectDir = "."
        , scWorkDir = "."
        , scCabalStoreDir = Nothing
        , scExecutionTimeoutUs = 120_000_000
        , scResyncTimeoutUs = 5_000_000
        , scJsonDiagnostics = False
        }

dummySession :: IO Session
dummySession = do
    q <- newOutQueue
    lock <- newMVar ()
    queryLock <- newMVar ()
    errRef <- newIORef []
    ctrRef <- newIORef 0
    cbRef <- newIORef (\_ -> pure ())
    killLock <- newMVar ()
    uid <- newUnique
    lastIntRef <- newIORef Nothing
    gen <- newIORef 1
    owner <- newTVarIO Nothing
    let ps =
            ProcSession
                { psId = uid
                , psProc = error "dummySession: psProc"
                , psPgid = Nothing
                , psKillLock = killLock
                , psStdin = error "dummySession: psStdin"
                , psStdout = error "dummySession: psStdout"
                , psStderr = error "dummySession: psStderr"
                , psQueue = q
                }
    pure
        Session
            { sessProcSess = ps
            , sessLock = lock
            , sessQueryLock = queryLock
            , sessLockOwner = owner
            , sessErrBuf = errRef
            , sessBaselineBindings = errRef
            , sessCounter = ctrRef
            , sessConfig = defaultCfg
            , sessErrCallback = cbRef
            , sessNonce = 12_345
            , sessLastInterruptTime = lastIntRef
            , sessionGen = gen
            }

spec :: Spec
spec = describe "query side-channel: sessQueryLock (stress case 20)" $ do
    it "a query holds the run lock too: one drain at a time on one queue" $ do
        sess <- dummySession
        withQueryLocks sess $ do
            runHeld <- isNothing <$> tryReadMVar (sessLock sess)
            runHeld `shouldBe` True
            queryHeld <- isNothing <$> tryReadMVar (sessQueryLock sess)
            queryHeld `shouldBe` True

    it "a query waits for a running cell rather than draining underneath it" $ do
        sess <- dummySession
        done <- newEmptyMVar
        withRunLock sess $ do
            _ <- forkIO (withQueryLocks sess (pure ()) >> putMVar done ())
            threadDelay 50_000
            started <- tryReadMVar done
            isJust started `shouldBe` False
        finished <- timeout 1_000_000 (takeMVar done)
        finished `shouldBe` Just ()

    it "two concurrent queries serialize" $ do
        sess <- dummySession
        results <- newEmptyMVar
        let query name =
                withQueryLocks sess $ do
                    threadDelay 100_000
                    putMVar results name
        _ <- forkIO (query ("query1" :: String))
        threadDelay 10_000
        _ <- forkIO (query "query2")
        r1 <- takeMVar results
        r2 <- takeMVar results
        [r1, r2] `shouldBe` ["query1", "query2"]

    describe "busy means a cell is running, not merely that the lock is held" $ do
        it "is True while a cell runs" $ do
            sess <- dummySession
            withRunLock sess (isBusy sess) `shouldReturn'` True

        it "is False while an editor query holds the lock" $ do
            sess <- dummySession
            withQueryLocks sess (isBusy sess) `shouldReturn'` False

        it "is False once the run releases" $ do
            sess <- dummySession
            withRunLock sess (pure ())
            isBusy sess `shouldReturn'` False
  where
    shouldReturn' act expected = act >>= (`shouldBe` expected)
