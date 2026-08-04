{-# LANGUAGE OverloadedStrings #-}

{- | A Session with no process behind it: the handles are errors, so a test
that reaches for one fails by name instead of talking to a real GHCi.
-}
module Test.SessionSpec.Fixture (
    dummySession,
    defaultCfg,
) where

import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.STM (newTVarIO)
import Data.IORef (IORef, newIORef)
import Data.Text (Text)
import Data.Unique (newUnique)

import Sabela.Session (Session (..), SessionConfig (..))
import Sabela.Session.Proc (ProcSession (..))
import Sabela.Session.Reader (OutQueue)
import Sabela.Session.Timeout (
    defaultTimeoutConfig,
    tcExecutionUs,
    tcResyncUs,
 )

dummySession ::
    OutQueue ->
    IORef [Text] ->
    IORef Int ->
    SessionConfig ->
    IO Session
dummySession q errRef ctrRef cfg = do
    lock <- newMVar ()
    qlock <- newMVar ()
    lockOwner <- newTVarIO Nothing
    cbRef <- newIORef (\_ -> pure ())
    klock <- newMVar ()
    uid <- newUnique
    lastInt <- newIORef Nothing
    gen <- newIORef 1
    let ps =
            ProcSession
                { psId = uid
                , psProc = error "dummySession: psProc used unexpectedly"
                , psPgid = Nothing
                , psKillLock = klock
                , psStdin = error "dummySession: psStdin used unexpectedly"
                , psStdout = error "dummySession: psStdout used unexpectedly"
                , psStderr = error "dummySession: psStderr used unexpectedly"
                , psQueue = q
                }
    pure
        Session
            { sessProcSess = ps
            , sessLock = lock
            , sessQueryLock = qlock
            , sessLockOwner = lockOwner
            , sessErrBuf = errRef
            , sessBaselineBindings = errRef
            , sessCounter = ctrRef
            , sessConfig = cfg
            , sessErrCallback = cbRef
            , sessNonce = 4242
            , sessLastInterruptTime = lastInt
            , sessionGen = gen
            }

defaultCfg :: SessionConfig
defaultCfg =
    SessionConfig
        { scProjectDir = "."
        , scWorkDir = "."
        , scCabalStoreDir = Nothing
        , scExecutionTimeoutUs = tcExecutionUs defaultTimeoutConfig
        , scResyncTimeoutUs = tcResyncUs defaultTimeoutConfig
        , scJsonDiagnostics = False
        }
