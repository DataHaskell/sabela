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
    shutdownAllSessions,
    reloadHaskellSession,
    killSession,
    ensureSessionAlive,
    sessionMetaMatches,
    installAndRestart,
    handleKernelCrash,
    loadSabelaPrelude,

    -- * Install + REPL setup (also exposed for tests)
    setupReplProject,
    resolveLocalPackages,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void)
import Data.List (nub)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Deps (ProjectSig, depsMatch, mergedMeta, projectSig)
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
import Sabela.Session.Proc (killLeftoverSessions)
import Sabela.Session.Process (
    closeSession,
    ghciBackend,
    newSessionStreaming,
 )
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), clearCompiledModules)
import Sabela.State.DependencyTracker (
    getHaskellDeps,
    getHaskellExts,
    getHaskellProjectSig,
    setHaskellDeps,
    setHaskellExts,
    setHaskellProjectSig,
 )
import Sabela.State.Environment (Environment (..))
import Sabela.State.SessionManager (
    forceResetAllSessions,
    getHaskellSession,
    modifyHaskellSession,
    setHaskellSession,
    takeHaskellSession,
 )
import ScriptHs.Parser (CabalMeta (..))
import ScriptHs.Run (renderCabalFile, renderCabalProject)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (isAbsolute, (</>))

-- | Runtime reset of the managed backends (notebook reset/restart).
killAllSessions :: App -> IO ()
killAllSessions app =
    forceResetAllSessions (appSessions app)

{- | Server-exit teardown: polite closes plus the registry sweep that
reclaims sessions no manager slot references (scratchpad, half-spawns).
Shutdown only — at runtime the sweep would kill live scratchpad sessions.
-}
shutdownAllSessions :: App -> IO ()
shutdownAllSessions app = do
    forceResetAllSessions (appSessions app)
    killLeftoverSessions

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
                    backend
                    ("Kernel crashed during :reload: " <> T.pack (show e))
            Right _ -> pure ()

{- | Swap the slot out first, then close outside the manager MVar so
session queries never stall behind a multi-second teardown.
-}
killSession :: App -> IO ()
killSession app = do
    mSess <- takeHaskellSession (appSessions app)
    forM_ mSess $ \s ->
        void (try (ST.sbClose s) :: IO (Either SomeException ()))

ensureSessionAlive :: App -> Int -> CabalMeta -> IO Bool
ensureSessionAlive app gen metas = do
    ok <- sessionMetaMatches app metas
    if ok then pure True else installAndRestart app gen metas

{- | Is there a live session whose installed state covers the notebook's
metadata — dep names, extensions, and the project signature (local
packages, git pins, ghc-options)?
-}
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

{- | The project signature the notebook's metadata asks for, resolved the
same way 'installDepsAndStartSession' resolves it before installing.
-}
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
        merged = mergedMeta (envGlobalDeps (appEnv app)) metas
        localPkgs =
            resolveLocalPackages
                (envWorkDir (appEnv app))
                (envLocalPackages (appEnv app))
                merged
    setHaskellProjectSig (appDeps app) (projectSig localPkgs merged)
    setupReplProject localPkgs projDir merged
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

{- | Combine the operator's @SABELA_LOCAL_PACKAGES@ overlays with the notebook's
own @-- cabal: packages:@ directive. Operator overlays come first (and win on
dedupe); notebook-relative dirs resolve against the working directory, absolute
dirs pass through.
-}
resolveLocalPackages :: FilePath -> [FilePath] -> CabalMeta -> [FilePath]
resolveLocalPackages workDir envLocals meta =
    nub (envLocals ++ map resolve (metaPackages meta))
  where
    resolve t =
        let p = T.unpack t
         in if isAbsolute p then p else workDir </> p

{- | Write the throwaway repl project (regenerated each run) into @dir@. The @[]@
to 'renderCabalFile' adds no extra local-package names to @build-depends@; local
packages resolve through the @packages:@ stanza in the generated @cabal.project@.
-}
setupReplProject :: [FilePath] -> FilePath -> CabalMeta -> IO ()
setupReplProject localPkgs dir meta = do
    createDirectoryIfMissing True dir
    writeFile
        (dir </> "cabal.project")
        ( T.unpack
            ( renderCabalProject
                localPkgs
                (metaSourceRepos meta)
                (metaExtraLibDirs meta)
                (metaExtraIncludeDirs meta)
            )
        )
    ensureFile (dir </> "Main.hs") "main :: IO ()\nmain = pure ()\n"
    writeFile (dir </> "sabela-repl.cabal") (renderCabalFile "sabela-repl" [] meta)

ensureFile :: FilePath -> String -> IO ()
ensureFile path content = do
    exists <- doesFileExist path
    unless exists $ writeFile path content

startSessionWith :: App -> FilePath -> IO Bool
startSessionWith app projDir = do
    clearCompiledModules app
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
                handleKernelCrash
                    app
                    backend
                    ("Kernel crashed during prelude: " <> T.pack (show e))
            Right _ -> pure ()

{- | Clear the manager slot only if it still holds the crashed backend,
then unconditionally tear the crashed backend down (idempotent), so a
late crash event can never drop or leak a fresh replacement session.
-}
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
