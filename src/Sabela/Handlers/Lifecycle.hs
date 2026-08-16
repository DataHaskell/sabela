{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.Handlers.Lifecycle (
    killAllSessions,
    killAllSessionsUnlocked,
    shutdownAllSessions,
    killSession,
    killSessionAsync,
    ensureSessionAlive,
    envStale,
    neededEnvSig,
    sessionMetaMatches,
    installAndRestart,
    installAndRestartUnlocked,
    handleKernelCrash,
    loadSabelaPrelude,
    setupReplProject,
    resolveLocalPackages,
) where

import Sabela.Handlers.Lifecycle.Startup (
    handleKernelCrash,
    loadSabelaPrelude,
    reportKernelFailure,
    startSessionWith,
 )

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void)
import Data.List (nub)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Deps (
    EnvSig (..),
    envSig,
    mergedMeta,
 )
import Sabela.Handlers.Shared
import Sabela.Model (KernelPhase (..), NotebookEvent (..), SessionStatus (..))
import Sabela.Session.EnvKey (resolveLocalPackages)
import Sabela.Session.Proc (killLeftoverSessions)
import Sabela.Session.Timeout (
    buildTimedOutMessage,
    readTimeoutConfig,
    tcBuildUs,
 )
import Sabela.Session.Workspace (buildIsDirty, wipeBuildArtifacts)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), withBuilding)
import Sabela.State.Environment (Environment (..))
import Sabela.State.SessionManager (
    forceResetAllSessions,
    getHaskellSession,
    haskellEnvOf,
    takeHaskellSession,
    withHaskellLifecycle,
 )
import ScriptHs.Parser (CabalMeta (..))
import System.FilePath (isAbsolute, (</>))
import System.Timeout (timeout)

import Sabela.Session.Project (ReplSupport (..), setupReplProject)

killAllSessions :: App -> IO ()
killAllSessions app =
    withHaskellLifecycle (appSessions app) $ killAllSessionsUnlocked app

{- | For callers already inside 'withHaskellLifecycle'. The mutex is not
reentrant, so taking it again would wedge the notebook permanently.
-}
killAllSessionsUnlocked :: App -> IO ()
killAllSessionsUnlocked app =
    forceResetAllSessions (appSessions app)

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

{- | Does the running kernel differ from what this notebook now needs? Sole
reader of the signature recorded at spawn, so every caller gets one answer. No
kernel, or one recorded against another process, is stale — hence recoverable.
-}
envStale :: App -> CabalMeta -> IO Bool
envStale app metas = do
    mSess <- getHaskellSession (appSessions app)
    recorded <- haskellEnvOf (appSessions app)
    pure $ case (mSess, recorded) of
        (Just backend, Just (uid, sig)) ->
            ST.sbSessionId backend /= uid || sig /= neededEnvSig app metas
        _ -> True

-- | The complement of 'envStale', kept for callers that read better positively.
sessionMetaMatches :: App -> CabalMeta -> IO Bool
sessionMetaMatches app metas = not <$> envStale app metas

-- | The environment this notebook needs, as recorded against a live kernel.
neededEnvSig :: App -> CabalMeta -> EnvSig
neededEnvSig app metas =
    envSig
        (envGlobalDeps (appEnv app))
        (localPackagesFor app (mergedMeta (envGlobalDeps (appEnv app)) metas))
        metas

localPackagesFor :: App -> CabalMeta -> [FilePath]
localPackagesFor app =
    resolveLocalPackages
        (envWorkDir (appEnv app))
        (envLocalPackages (appEnv app))

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
            reportKernelFailure app KpBuildTimeout SReset (buildTimedOutMessage budgetUs)
            pure False

runInstallAndStart :: App -> Int -> CabalMeta -> IO Bool
runInstallAndStart app gen metas = do
    broadcastDepsStatus app metas
    let projDir = envTmpDir (appEnv app) </> "repl-project"
        merged = mergedMeta (envGlobalDeps (appEnv app)) metas
        localPkgs = localPackagesFor app merged
    dirty <- buildIsDirty projDir
    setupReplProject WithNotebookSupport localPkgs projDir merged
    current <- isCurrentGen app gen
    if not current
        then pure False
        else do
            broadcast app (EvSessionStatus SStarting)
            killSession app
            recoverWorkspace app projDir dirty
            currentAfterDetach <- isCurrentGen app gen
            if currentAfterDetach
                then startSessionWith app projDir (neededEnvSig app metas)
                else pure False

{- | Discard artefacts a previous kernel was killed midway through writing.
Strictly after 'killSession' reaped it: wiping while it lives fails on Windows,
and on POSIX lets the doomed process write on into an unlinked directory.
-}
recoverWorkspace :: App -> FilePath -> Bool -> IO ()
recoverWorkspace _ _ False = pure ()
recoverWorkspace app projDir True = do
    debugLog app "[handler] previous kernel was killed; rebuilding from clean"
    broadcast app (EvInstallLog recoveringNotice)
    wipeBuildArtifacts projDir

recoveringNotice :: Text
recoveringNotice =
    "A previous build was interrupted, so its partial output is being \
    \discarded. This build will take longer than usual."

{- | Report which packages this build is about to add, diffed against what the
running kernel actually has rather than against a tracker written before the
build succeeded. Reporting only; the provenance write happens at 'injectPrelude'.
-}
broadcastDepsStatus :: App -> CabalMeta -> IO ()
broadcastDepsStatus app metas = do
    recorded <- haskellEnvOf (appSessions app)
    let globalDeps = envGlobalDeps (appEnv app)
        notebookDeps = S.difference (S.fromList (metaDeps metas)) globalDeps
        haveDeps = maybe S.empty (esDeps . snd) recorded
        newDeps = S.difference notebookDeps haveDeps
    unless (notebookDeps `S.isSubsetOf` haveDeps) $
        broadcast app $
            EvSessionStatus $
                if S.null newDeps then SDepsUpToDate else SUpdateDeps (S.toList newDeps)
