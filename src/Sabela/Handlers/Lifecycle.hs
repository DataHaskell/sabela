{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Session-lifecycle handlers: spawn, restart, kill, install dependencies,
inject the display prelude, and recover from kernel crashes. Used by both
the entry-point handlers and the planning module to keep a working GHCi
backend behind the reactive engine.
-}
module Sabela.Handlers.Lifecycle (
    -- * Top-level lifecycle
    killAllSessions,
    reloadHaskellSession,
    killSession,
    ensureSessionAlive,
    installAndRestart,
    handleKernelCrash,
    loadSabelaPrelude,

    -- * Install + REPL setup (also exposed for tests)
    setupReplProject,

    -- * Pieces (exposed for reuse from Plan)
    depsMatch,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Deps (mergedMeta)
import Sabela.Handlers.Shared
import Sabela.Model (NotebookEvent (..), SessionStatus (..))
import Sabela.Output (displayPrelude)
import Sabela.Session (
    Session,
    SessionConfig (..),
    clearErrCallback,
    readErrorBuffer,
    runBlock,
 )
import Sabela.Session.Process (
    closeSession,
    ghciBackend,
    newSessionStreaming,
 )
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.DependencyTracker (
    getHaskellDeps,
    getHaskellExts,
    setHaskellDeps,
    setHaskellExts,
 )
import Sabela.State.Environment (Environment (..))
import Sabela.State.SessionManager (
    forceResetAllSessions,
    getHaskellSession,
    modifyHaskellSession,
    setHaskellSession,
 )
import ScriptHs.Parser (CabalMeta (..))
import ScriptHs.Run (renderCabalFile, renderCabalProject)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

killAllSessions :: App -> IO ()
killAllSessions app =
    forceResetAllSessions (appSessions app)

reloadHaskellSession :: App -> IO ()
reloadHaskellSession app = do
    debugLog app "[handler] reloadHaskellSession: :reload"
    mSess <- getHaskellSession (appSessions app)
    forM_ mSess $ \backend -> do
        result <- try (ST.sbRunBlock backend ":reload")
        case result of
            Left (e :: SomeException) ->
                handleKernelCrash
                    app
                    ("Kernel crashed during :reload: " <> T.pack (show e))
            Right _ -> pure ()

killSession :: App -> IO ()
killSession app =
    modifyHaskellSession (appSessions app) $ \mSess -> do
        forM_ mSess $ \s ->
            void (try (ST.sbClose s) :: IO (Either SomeException ()))
        pure Nothing

ensureSessionAlive :: App -> Int -> CabalMeta -> IO Bool
ensureSessionAlive app gen metas = do
    installed <- getHaskellDeps (appDeps app)
    instExts <- getHaskellExts (appDeps app)
    mSess <- getHaskellSession (appSessions app)
    let metaMatch = depsMatch metas installed instExts (envGlobalDeps (appEnv app))
    case mSess of
        Just _ | metaMatch -> pure True
        _ -> installAndRestart app gen metas

depsMatch :: CabalMeta -> Set Text -> Set Text -> Set Text -> Bool
depsMatch metas installed instExts globalDeps =
    S.fromList (metaDeps metas) `S.isSubsetOf` (installed `S.union` globalDeps)
        && S.fromList (metaExts metas) == instExts

installAndRestart :: App -> Int -> CabalMeta -> IO Bool
installAndRestart app gen metas = do
    current <- isCurrentGen app gen
    if not current
        then pure False
        else installDepsAndStartSession app gen metas

installDepsAndStartSession :: App -> Int -> CabalMeta -> IO Bool
installDepsAndStartSession app _gen metas = do
    broadcastDepsStatus app metas
    setHaskellExts (appDeps app) (S.fromList (metaExts metas))
    let projDir = envTmpDir (appEnv app) </> "repl-project"
    setupReplProject
        (envLocalPackages (appEnv app))
        projDir
        (mergedMeta (envGlobalDeps (appEnv app)) metas)
    broadcast app (EvSessionStatus SStarting)
    killSession app
    startSessionWith app projDir

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

setupReplProject :: [FilePath] -> FilePath -> CabalMeta -> IO ()
setupReplProject localPkgs dir meta = do
    createDirectoryIfMissing True dir
    -- Regenerate every run (the repl-project temp dir is per-server) so changes
    -- to local packages / git pins take effect.
    writeFile
        (dir </> "cabal.project")
        (T.unpack (renderCabalProject localPkgs (metaSourceRepos meta)))
    ensureFile (dir </> "Main.hs") "main :: IO ()\nmain = pure ()\n"
    writeFile (dir </> "sabela-repl.cabal") (renderCabalFile "sabela-repl" meta)

ensureFile :: FilePath -> String -> IO ()
ensureFile path content = do
    exists <- doesFileExist path
    unless exists $ writeFile path content

startSessionWith :: App -> FilePath -> IO Bool
startSessionWith app projDir = do
    debugLog app "[handler] Injecting display prelude"
    let cfg = SessionConfig{scProjectDir = projDir, scWorkDir = envWorkDir (appEnv app)}
        onLine t = unless (T.null t) $ broadcast app (EvInstallLog t)
        locals = envLocalPackages (appEnv app)
    unless (null locals) $
        broadcast
            app
            (EvInstallLog (T.pack ("Local package overlays: " <> unwords locals)))
    sessResult <-
        try (newSessionStreaming cfg onLine) :: IO (Either SomeException Session)
    case sessResult of
        Left e -> reportSessionFailure app "Session startup failed" e
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
                handleKernelCrash app ("Kernel crashed during prelude: " <> T.pack (show e))
            Right _ -> pure ()

handleKernelCrash :: App -> Text -> IO ()
handleKernelCrash app msg = do
    debugLog app $ "[handler] Kernel crash detected: " <> msg
    setHaskellSession (appSessions app) Nothing
    broadcast app (EvSessionStatus SCrashed)
