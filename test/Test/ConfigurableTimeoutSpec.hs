{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.ConfigurableTimeoutSpec (spec) where

import Control.Concurrent.MVar (newMVar)
import Data.IORef (newIORef)
import Data.Unique (newUnique)
import Sabela.Session (
    Session (..),
    SessionConfig (..),
    executionTimeoutUs,
    resyncTimeoutUs,
 )
import Sabela.Session.Proc (ProcSession (..))
import Sabela.Session.Reader (newOutQueue)
import Sabela.Session.Timeout (
    defaultTimeoutConfig,
    tcExecutionUs,
    tcResyncUs,
    timedOutMessage,
 )
import Test.Hspec (
    Spec,
    describe,
    it,
    shouldBe,
 )

dummySessionWithConfig :: SessionConfig -> IO Session
dummySessionWithConfig cfg = do
    q <- newOutQueue
    lock <- newMVar ()
    errRef <- newIORef []
    ctrRef <- newIORef 0
    cbRef <- newIORef (\_ -> pure ())
    klock <- newMVar ()
    qlock <- newMVar ()
    uid <- newUnique
    busy <- newIORef False
    lastInt <- newIORef Nothing
    gen <- newIORef 1
    let ps =
            ProcSession
                { psId = uid
                , psProc = error "dummySessionWithConfig: psProc"
                , psPgid = Nothing
                , psKillLock = klock
                , psStdin = error "dummySessionWithConfig: psStdin"
                , psStdout = error "dummySessionWithConfig: psStdout"
                , psStderr = error "dummySessionWithConfig: psStderr"
                , psQueue = q
                }
    pure
        Session
            { sessProcSess = ps
            , sessLock = lock
            , sessQueryLock = qlock
            , sessErrBuf = errRef
            , sessBaselineBindings = errRef
            , sessCounter = ctrRef
            , sessConfig = cfg
            , sessErrCallback = cbRef
            , sessBusy = busy
            , sessNonce = 4242
            , sessLastInterruptTime = lastInt
            , sessionGen = gen
            }

mkCfg :: Int -> Int -> SessionConfig
mkCfg execUs resyncUs =
    SessionConfig
        { scProjectDir = "."
        , scWorkDir = "."
        , scExecutionTimeoutUs = execUs
        , scResyncTimeoutUs = resyncUs
        }

spec :: Spec
spec = do
    describe "configurable execution timeout" $ do
        it "uses default 120s timeout when not configured" $ do
            sess <- dummySessionWithConfig (mkCfg 120_000_000 5_000_000)
            executionTimeoutUs sess `shouldBe` 120_000_000

        it "uses configured timeout from SessionConfig" $ do
            sess <- dummySessionWithConfig (mkCfg 60_000_000 3_000_000)
            executionTimeoutUs sess `shouldBe` 60_000_000

        it "respects custom resync timeout" $ do
            sess <- dummySessionWithConfig (mkCfg 120_000_000 10_000_000)
            resyncTimeoutUs sess `shouldBe` 10_000_000

        it "timeout notice reflects the actual configured budget" $ do
            timedOutMessage 60_000_000
                `shouldBe` "\n*** Execution timed out after 60 seconds; \
                           \computation interrupted ***"

    describe "defaultTimeoutConfig" $ do
        it "is the historical 120s / 5s pair" $ do
            tcExecutionUs defaultTimeoutConfig `shouldBe` 120_000_000
            tcResyncUs defaultTimeoutConfig `shouldBe` 5_000_000
