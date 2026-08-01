{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Bringing a spawned kernel up, and reporting when it does not come up.
Everything here runs after the process exists and before it is current.
-}
module Sabela.Handlers.Lifecycle.Startup (
    startSessionWith,
    injectPrelude,
    loadSabelaPrelude,
    handleKernelCrash,
    reportKernelFailure,
    reportKernelFailureAt,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, fromException, throwIO, try)
import Control.Monad (forM_, unless, void)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Deps (EnvSig, blameCells)
import Sabela.Handlers.Shared
import Sabela.Model (
    KernelPhase (..),
    NotebookEvent (..),
    SessionStatus (..),
 )
import Sabela.Output (displayPrelude)
import Sabela.Session (
    Session,
    clearErrCallback,
    mkSessionConfig,
    readErrorBuffer,
    runBlock,
 )
import Sabela.Session.Process (
    closeSession,
    ghciBackend,
    newSessionStreaming,
 )
import Sabela.Session.Query (captureBindingsBaseline)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), broadcastNotebookState, readNotebook)
import Sabela.State.Environment (Environment (..))
import Sabela.State.SessionManager (
    getHaskellSession,
    installHaskellSession,
    modifyHaskellSession,
 )
import System.Timeout (Timeout)

startSessionWith :: App -> FilePath -> EnvSig -> IO Bool
startSessionWith app projDir sig = do
    debugLog app "[handler] Injecting display prelude"
    cfg <- mkSessionConfig projDir (envWorkDir (appEnv app))
    let onLine t = unless (T.null t) $ broadcast app (EvInstallLog t)
        locals = envLocalPackages (appEnv app)
    unless (null locals) $
        broadcast
            app
            (EvInstallLog (T.pack ("Local package overlays: " <> unwords locals)))
    sessResult <-
        try (newSessionStreaming cfg onLine) :: IO (Either SomeException Session)
    case sessResult of
        Left e
            | Just t <- (fromException e :: Maybe Timeout) -> throwIO t
            | otherwise -> reportSessionFailure app KpBuildFailed "Session startup failed" e
        Right sess -> do
            clearErrCallback sess
            injectPrelude app sess sig

{- | Report a failure where the user can act on it: the detail on its own
channel, then the state. Deliberately writes no 'cellError' — that would mark
the blamed cells settled, and the planner would stop re-running them.
-}
reportKernelFailureAt ::
    App -> KernelPhase -> SessionStatus -> Text -> [Int] -> IO ()
reportKernelFailureAt app phase status message cellIds = do
    broadcast app (EvKernelError phase message cellIds)
    broadcast app (EvSessionStatus status)

-- | As 'reportKernelFailureAt', blaming whichever cells declared the packages
-- the message names.
reportKernelFailure :: App -> KernelPhase -> SessionStatus -> Text -> IO ()
reportKernelFailure app phase status message = do
    nb <- readNotebook (appNotebook app)
    reportKernelFailureAt app phase status message (blameCells nb message)

reportSessionFailure :: App -> KernelPhase -> Text -> SomeException -> IO Bool
reportSessionFailure app phase msg e = do
    let detail = msg <> ": " <> T.pack (show e)
    debugLog app ("[handler] " <> detail)
    reportKernelFailure app phase SReset detail
    pure False

broadcastInstallLog :: App -> Session -> IO ()
broadcastInstallLog app sess = do
    startupLog <- readErrorBuffer sess
    mapM_
        (broadcast app . EvInstallLog)
        (filter (not . T.null) (T.lines startupLog))

{- | The one place a working kernel becomes current, so it is the one place the
environment it was built from is recorded.
-}
injectPrelude :: App -> Session -> EnvSig -> IO Bool
injectPrelude app sess sig = do
    result <-
        try (runBlock sess displayPrelude) :: IO (Either SomeException (Text, Text))
    case result of
        Left e -> do
            _ <- reportSessionFailure app KpPreludeFailed "Prelude injection failed" e
            threadDelay 100000
            broadcastInstallLog app sess
            void (try (closeSession sess) :: IO (Either SomeException ()))
            pure False
        Right _ -> do
            captureBindingsBaseline sess
            installHaskellSession (appSessions app) (ghciBackend sess) sig
            -- The epoch only moves here, so this is the one place a client can
            -- learn its view was built against a kernel that no longer exists.
            broadcastNotebookState app
            broadcast app (EvSessionStatus SReady)
            pure True

loadSabelaPrelude :: App -> IO ()
loadSabelaPrelude app = do
    mSess <- getHaskellSession (appSessions app)
    forM_ mSess $ \backend -> do
        result <- try (ST.sbRunBlock backend displayPrelude)
        case result of
            Left (e :: SomeException) ->
                handleKernelCrash
                    app
                    backend
                    ("Kernel crashed during prelude: " <> T.pack (show e))
            Right _ -> pure ()

handleKernelCrash :: App -> ST.SessionBackend -> Text -> IO ()
handleKernelCrash app crashed msg = do
    debugLog app $ "[handler] Kernel crash detected: " <> msg
    modifyHaskellSession (appSessions app) $ \case
        Just s | ST.sbSessionId s == ST.sbSessionId crashed -> Nothing
        other -> other
    void (try (ST.sbClose crashed) :: IO (Either SomeException ()))
    reportKernelFailure app KpCrashed SCrashed msg
