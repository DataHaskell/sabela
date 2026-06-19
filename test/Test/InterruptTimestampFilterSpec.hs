{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Pins the interrupt-timestamp filter (P1 stress case 25): a request
stamped before the kernel's last interrupt is stale; one stamped after it
is not. Pure logic over a dummy 'Session'; no live kernel.
-}
module Test.InterruptTimestampFilterSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newMVar)
import Data.IORef (newIORef)
import Data.Time (getCurrentTime)
import Data.Unique (newUnique)
import Sabela.Session (
    Session (..),
    SessionConfig (..),
    isRequestStale,
    markInterrupt,
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
spec = describe "interrupt-timestamp filter (stress case 25)" $ do
    it "request before any interrupt is not stale" $ do
        sess <- dummySession
        now <- getCurrentTime
        stale <- isRequestStale sess now
        stale `shouldBe` False

    it "after interrupt, request stamped before it is stale" $ do
        sess <- dummySession
        beforeTs <- getCurrentTime
        threadDelay 100_000
        markInterrupt sess
        staleCheck <- isRequestStale sess beforeTs
        staleCheck `shouldBe` True

    it "request stamped after interrupt is not stale" $ do
        sess <- dummySession
        markInterrupt sess
        threadDelay 100_000
        afterTs <- getCurrentTime
        stale <- isRequestStale sess afterTs
        stale `shouldBe` False

    it "multiple interrupts track the latest timestamp" $ do
        sess <- dummySession
        markInterrupt sess
        threadDelay 100_000
        t1 <- getCurrentTime
        threadDelay 100_000
        markInterrupt sess
        threadDelay 100_000
        t2 <- getCurrentTime
        stale1 <- isRequestStale sess t1
        stale1 `shouldBe` True
        stale2 <- isRequestStale sess t2
        stale2 `shouldBe` False
