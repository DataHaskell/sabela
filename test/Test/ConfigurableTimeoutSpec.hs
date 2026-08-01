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
    buildTimedOutMessage,
    defaultTimeoutConfig,
    tcBuildUs,
    tcExecutionUs,
    tcResyncUs,
    tcTryBuildUs,
    timedOutMessage,
    tryBuildTimedOutMessage,
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
            , sessNonce = 4242
            , sessLastInterruptTime = lastInt
            , sessionGen = gen
            }

mkCfg :: Int -> Int -> SessionConfig
mkCfg execUs resyncUs =
    SessionConfig
        { scProjectDir = "."
        , scWorkDir = "."
        , scCabalStoreDir = Nothing
        , scExecutionTimeoutUs = execUs
        , scResyncTimeoutUs = resyncUs
        , scJsonDiagnostics = False
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
        it "is the 30-minute execution budget with a 5s resync window" $ do
            tcExecutionUs defaultTimeoutConfig `shouldBe` 1800_000_000
            tcResyncUs defaultTimeoutConfig `shouldBe` 5_000_000
        it "bounds the off-lock build phase at 30 minutes too" $
            tcBuildUs defaultTimeoutConfig `shouldBe` 1800_000_000
        it "bounds the disposable try build phase at 120s, tighter than tcBuildUs" $
            tcTryBuildUs defaultTimeoutConfig `shouldBe` 120_000_000

    describe "buildTimedOutMessage" $ do
        it "reports the configured build budget and how to raise it" $
            buildTimedOutMessage 300_000_000
                `shouldBe` "\n*** Build (dependency install / cold start) timed \
                           \out after 300 seconds; the kernel was reset. Check \
                           \the dependencies compile, or raise \
                           \SABELA_BUILD_TIMEOUT_SECONDS ***"

    describe "tryBuildTimedOutMessage" $ do
        it "names the heavy dependencies and the commit-deliberately alternative" $
            tryBuildTimedOutMessage ["hasktorch"] 120_000_000
                `shouldBe` "\n*** try build timed out after 120 seconds building \
                           \hasktorch; this looks like a heavy dependency for a \
                           \disposable trial. Commit it deliberately with a \
                           \`-- cabal:` line in a real cell instead of retrying \
                           \try ***"
        it "falls back to generic wording when no dependency names are known" $
            tryBuildTimedOutMessage [] 60_000_000
                `shouldBe` "\n*** try build timed out after 60 seconds building \
                           \the requested dependencies; this looks like a heavy \
                           \dependency for a disposable trial. Commit it \
                           \deliberately with a `-- cabal:` line in a real cell \
                           \instead of retrying try ***"
