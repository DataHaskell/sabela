{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.Handlers.Lifecycle (
    killAllSessions,
    shutdownAllSessions,
    killSession,
    killSessionAsync,
    ensureSessionAlive,
    sessionMetaMatches,
    installAndRestart,
    hardResetKernel,
    handleKernelCrash,
    loadSabelaPrelude,
    setupReplProject,
    resolveLocalPackages,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, fromException, throwIO, try)
import Control.Monad (forM_, unless, void, when)
import Data.List (nub)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Deps (
    ProjectSig,
    collectMetadata,
    depsMatch,
    mergedMeta,
    projectSig,
 )
import Sabela.Handlers.Shared
import Sabela.Model (NotebookEvent (..), SessionStatus (..))
import Sabela.Output (displayPrelude)
import Sabela.Session (
    Session,
    clearErrCallback,
    mkSessionConfig,
    readErrorBuffer,
    runBlock,
 )
import Sabela.Session.Proc (killLeftoverSessions)
import Sabela.Session.Process (
    closeSession,
    ghciBackend,
    newSessionStreaming,
 )
import Sabela.Session.Query (captureBindingsBaseline)
import Sabela.Session.Timeout (
    buildTimedOutMessage,
    readTimeoutConfig,
    tcBuildUs,
 )
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), clearCompiledModules, withBuilding)
import Sabela.State.DependencyTracker (
    getHaskellDeps,
    getHaskellExts,
    getHaskellProjectSig,
    setHaskellDeps,
    setHaskellExts,
    setHaskellProjectSig,
 )
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (
    forceResetAllSessions,
    getHaskellSession,
    modifyHaskellSession,
    setHaskellSession,
    takeHaskellSession,
    withHaskellLifecycle,
 )
import ScriptHs.Parser (CabalMeta (..))
import System.FilePath (isAbsolute, (</>))
import System.Timeout (Timeout, timeout)

import Sabela.Session.Project (ReplSupport (..), setupReplProject)

killAllSessions :: App -> IO ()
killAllSessions app =
    forceResetAllSessions (appSessions app)

hardResetKernel :: App -> Int -> IO ()
hardResetKernel app gen =
    withHaskellLifecycle (appSessions app) $
        whenCurrentGen app gen $ do
            debugLog app "[handler] hardResetKernel: force-kill + respawn"
            mSess <- getHaskellSession (appSessions app)
            killAllSessions app
            whenCurrentGen app gen $ do
                let projDir = envTmpDir (appEnv app) </> "repl-project"
                ok <- case mSess of
                    Just _ -> startSessionWith app projDir
                    Nothing ->
                        installAndRestartUnlocked app gen . collectMetadata
                            =<< readNotebook (appNotebook app)
                when ok $ broadcast app EvExecutionDone

shutdownAllSessions :: App -> IO ()
shutdownAllSessions app = do
    forceResetAllSessions (appSessions app)
    killLeftoverSessions

killSession :: App -> IO ()
killSession app = do
    mSess <- takeHaskellSession (appSessions app)
    forM_ mSess $ \s ->
        void (try (ST.sbClose s) :: IO (Either SomeException ()))

killSessionAsync :: App -> IO ()
killSessionAsync app = do
    mSess <- takeHaskellSession (appSessions app)
    forM_ mSess $ \s ->
        void $ forkIO (void (try (ST.sbClose s) :: IO (Either SomeException ())))

ensureSessionAlive :: App -> Int -> CabalMeta -> IO Bool
ensureSessionAlive app gen metas = do
    ok <- sessionMetaMatches app metas
    if ok then pure True else installAndRestart app gen metas

sessionMetaMatches :: App -> CabalMeta -> IO Bool
sessionMetaMatches app metas = do
    installed <- getHaskellDeps (appDeps app)
    instExts <- getHaskellExts (appDeps app)
    instSig <- getHaskellProjectSig (appDeps app)
    mSess <- getHaskellSession (appSessions app)
    let neededSig = neededProjectSig app metas
        metaMatch =
            depsMatch
                metas
                installed
                instExts
                (envGlobalDeps (appEnv app))
                neededSig
                instSig
    case mSess of
        Just _ -> pure metaMatch
        Nothing -> pure False

neededProjectSig :: App -> CabalMeta -> ProjectSig
neededProjectSig app metas =
    let merged = mergedMeta (envGlobalDeps (appEnv app)) metas
        localPkgs =
            resolveLocalPackages
                (envWorkDir (appEnv app))
                (envLocalPackages (appEnv app))
                merged
     in projectSig localPkgs merged

installAndRestart :: App -> Int -> CabalMeta -> IO Bool
installAndRestart app gen metas =
    withHaskellLifecycle (appSessions app) $
        installAndRestartUnlocked app gen metas

installAndRestartUnlocked :: App -> Int -> CabalMeta -> IO Bool
installAndRestartUnlocked app gen metas = do
    current <- isCurrentGen app gen
    if not current
        then pure False
        else installDepsAndStartSession app gen metas

installDepsAndStartSession :: App -> Int -> CabalMeta -> IO Bool
installDepsAndStartSession app gen metas = withBuilding app $ do
    budgetUs <- tcBuildUs <$> readTimeoutConfig
    result <- timeout budgetUs (runInstallAndStart app gen metas)
    case result of
        Just ok -> pure ok
        Nothing -> do
            broadcast app (EvInstallLog (buildTimedOutMessage budgetUs))
            broadcast app (EvSessionStatus SReset)
            pure False

runInstallAndStart :: App -> Int -> CabalMeta -> IO Bool
runInstallAndStart app gen metas = do
    broadcastDepsStatus app metas
    setHaskellExts (appDeps app) (S.fromList (metaExts metas))
    let projDir = envTmpDir (appEnv app) </> "repl-project"
        merged = mergedMeta (envGlobalDeps (appEnv app)) metas
        localPkgs =
            resolveLocalPackages
                (envWorkDir (appEnv app))
                (envLocalPackages (appEnv app))
                merged
    setHaskellProjectSig (appDeps app) (projectSig localPkgs merged)
    setupReplProject WithNotebookSupport localPkgs projDir merged
    current <- isCurrentGen app gen
    if not current
        then pure False
        else do
            broadcast app (EvSessionStatus SStarting)
            killSession app
            currentAfterDetach <- isCurrentGen app gen
            if currentAfterDetach then startSessionWith app projDir else pure False

broadcastDepsStatus :: App -> CabalMeta -> IO ()
broadcastDepsStatus app metas = do
    installedDeps <- getHaskellDeps (appDeps app)
    let globalDeps = envGlobalDeps (appEnv app)
        notebookDeps = S.difference (S.fromList (metaDeps metas)) globalDeps
    unless (notebookDeps `S.isSubsetOf` installedDeps) $ do
        let newDeps = S.difference notebookDeps installedDeps
        broadcast app $
            EvSessionStatus $
                if S.null newDeps then SDepsUpToDate else SUpdateDeps (S.toList newDeps)
        setHaskellDeps (appDeps app) notebookDeps

resolveLocalPackages :: FilePath -> [FilePath] -> CabalMeta -> [FilePath]
resolveLocalPackages workDir envLocals meta =
    nub (envLocals ++ map resolve (metaPackages meta))
  where
    resolve t =
        let p = T.unpack t
         in if isAbsolute p then p else workDir </> p

startSessionWith :: App -> FilePath -> IO Bool
startSessionWith app projDir = do
    clearCompiledModules app
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
            | otherwise -> reportSessionFailure app "Session startup failed" e
        Right sess -> do
            clearErrCallback sess
            injectPrelude app sess

reportSessionFailure :: App -> Text -> SomeException -> IO Bool
reportSessionFailure app msg e = do
    debugLog app $ "[handler] " <> msg <> ": " <> T.pack (show e)
    broadcast app (EvSessionStatus SReset)
    pure False

broadcastInstallLog :: App -> Session -> IO ()
broadcastInstallLog app sess = do
    startupLog <- readErrorBuffer sess
    mapM_
        (broadcast app . EvInstallLog)
        (filter (not . T.null) (T.lines startupLog))

injectPrelude :: App -> Session -> IO Bool
injectPrelude app sess = do
    result <-
        try (runBlock sess displayPrelude) :: IO (Either SomeException (Text, Text))
    case result of
        Left e -> do
            _ <- reportSessionFailure app "Prelude injection failed" e
            threadDelay 100000
            broadcastInstallLog app sess
            void (try (closeSession sess) :: IO (Either SomeException ()))
            pure False
        Right _ -> do
            captureBindingsBaseline sess
            setHaskellSession (appSessions app) (Just (ghciBackend sess))
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
    clearCompiledModules app
    modifyHaskellSession (appSessions app) $ \mSess ->
        pure $ case mSess of
            Just s | ST.sbSessionId s == ST.sbSessionId crashed -> Nothing
            other -> other
    void (try (ST.sbClose crashed) :: IO (Either SomeException ()))
    broadcast app (EvSessionStatus SCrashed)
