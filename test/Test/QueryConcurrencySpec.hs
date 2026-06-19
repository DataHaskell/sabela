{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Pins the query side-channel (P1 stress case 20): GHCi introspection
serializes on 'sessQueryLock', a lock distinct from the cell run-lock
'sessLock'. Queries no longer corrupt each other's stdin writes, and the
run-lock stays free so a query never blocks behind a draining cell at the
lock level (the busy gate is admission control, not the run-lock). Pure
locking logic over a dummy 'Session'; no live kernel.
-}
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
import Data.IORef (newIORef)
import Data.Maybe (isJust, isNothing)
import Data.Unique (newUnique)
import Sabela.Session (
    Session (..),
    SessionConfig (..),
 )
import Sabela.Session.Proc (ProcSession (..))
import Sabela.Session.Reader (newOutQueue)
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
        , scExecutionTimeoutUs = 120_000_000
        , scResyncTimeoutUs = 5_000_000
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
    busy <- newIORef False
    lastIntRef <- newIORef Nothing
    gen <- newIORef 1
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
            , sessErrBuf = errRef
            , sessCounter = ctrRef
            , sessConfig = defaultCfg
            , sessErrCallback = cbRef
            , sessBusy = busy
            , sessNonce = 12_345
            , sessLastInterruptTime = lastIntRef
            , sessionGen = gen
            }

spec :: Spec
spec = describe "query side-channel: sessQueryLock (stress case 20)" $ do
    it "the query lock is distinct from the cell run-lock" $ do
        sess <- dummySession
        -- Hold the query lock; the run-lock must stay free, so a query
        -- never serializes behind a cell at the lock level.
        withMVar (sessQueryLock sess) $ \_ -> do
            runFree <- isJust <$> tryReadMVar (sessLock sess)
            runFree `shouldBe` True
            queryHeld <- isNothing <$> tryReadMVar (sessQueryLock sess)
            queryHeld `shouldBe` True

    it "holding the run-lock leaves the query lock free" $ do
        sess <- dummySession
        withMVar (sessLock sess) $ \_ -> do
            queryFree <- isJust <$> tryReadMVar (sessQueryLock sess)
            queryFree `shouldBe` True

    it "two concurrent queries serialize on sessQueryLock" $ do
        sess <- dummySession
        results <- newEmptyMVar
        let query name =
                withMVar (sessQueryLock sess) $ \_ -> do
                    threadDelay 100_000
                    putMVar results name
        _ <- forkIO (query ("query1" :: String))
        threadDelay 10_000
        _ <- forkIO (query "query2")
        r1 <- takeMVar results
        r2 <- takeMVar results
        [r1, r2] `shouldBe` ["query1", "query2"]
