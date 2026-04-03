module Sabela.State (
    App (..),
    newApp,

    -- * Re-exports for convenience
    module Sabela.State.Environment,
    module Sabela.State.EventBus,
    module Sabela.State.NotebookStore,
    module Sabela.State.SessionManager,
    module Sabela.State.DependencyTracker,
    module Sabela.State.WidgetStore,
    module Sabela.State.BridgeStore,
) where

import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Text (Text)
import Sabela.State.BridgeStore
import Sabela.State.DependencyTracker
import Sabela.State.Environment
import Sabela.State.EventBus
import Sabela.State.NotebookStore
import Sabela.State.SessionManager
import Sabela.State.WidgetStore
import System.Directory (
    canonicalizePath,
    createDirectoryIfMissing,
    getHomeDirectory,
 )
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

data App = App
    { appEnv :: Environment
    , appNotebook :: NotebookStore
    , appEvents :: EventBus
    , appSessions :: SessionManager
    , appDeps :: DependencyTracker
    , appWidgets :: WidgetStore
    , appBridge :: BridgeStore
    }

newApp :: FilePath -> Set Text -> IO App
newApp workDir globalDeps = do
    absWork <- canonicalizePath workDir
    tmpBase <- getCanonicalTemporaryDirectory
    tmpDir <- createTempDirectory tmpBase "sabela-server"
    debug <- isJust <$> lookupEnv "SABELA_DEBUG"
    replBin <- lookupEnv "SABELA_LEAN_REPL"
    leanBase <- lookupEnv "SABELA_LEAN_BASE"
    leanCache <- resolveLeanCache
    let env =
            Environment
                { envWorkDir = absWork
                , envTmpDir = tmpDir
                , envGlobalDeps = globalDeps
                , envDebugLog = debug
                , envLeanReplBin = replBin
                , envLeanBase = leanBase
                , envLeanCache = leanCache
                }
    App env
        <$> newNotebookStore
        <*> newEventBus
        <*> newSessionManager
        <*> newDependencyTracker
        <*> newWidgetStore
        <*> newBridgeStore

{- | Resolve the persistent Lean cache directory.
Priority: SABELA_LEAN_CACHE env var > ~/.sabela/lean-cache/
-}
resolveLeanCache :: IO FilePath
resolveLeanCache = do
    mEnv <- lookupEnv "SABELA_LEAN_CACHE"
    cacheDir <- case mEnv of
        Just p -> pure p
        Nothing -> do
            home <- getHomeDirectory
            pure (home </> ".sabela" </> "lean-cache")
    createDirectoryIfMissing True cacheDir
    pure cacheDir
