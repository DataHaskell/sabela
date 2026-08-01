module Sabela.State.App (
    App (..),
    loadedModules,
    setLoadedModules,
    forgetLoadedModules,
    setBuilding,
    withBuilding,
    getAIStore,
    setAIStore,
    broadcastNotebook,
    broadcastNotebookState,
    notebookState,
    resolveCliHandleStore,
) where

import Control.Concurrent.MVar (
    MVar,
    modifyMVar,
    modifyMVar_,
    readMVar,
 )
import Control.Exception (bracket_)
import Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Unique (Unique)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client (Manager)

import Sabela.AI.Handles (HandleStore, newHandleStore)
import Sabela.AI.Store (AIStore)
import Sabela.Model (
    Cell (..),
    Notebook (..),
    NotebookEvent (..),
    cellDirty,
 )
import Sabela.State.BridgeStore
import Sabela.State.DependencyTracker
import Sabela.State.Environment
import Sabela.State.EventBus
import Sabela.State.NotebookStore
import Sabela.State.SessionManager
import Sabela.State.WidgetStore

data App = App
    { appEnv :: Environment
    , appNotebook :: NotebookStore
    , appEvents :: EventBus
    , appSessions :: SessionManager
    , appDeps :: DependencyTracker
    , appWidgets :: WidgetStore
    , appBridge :: BridgeStore
    , appCompiledModules :: IORef (Maybe (Unique, M.Map Text Text))
    , appAI :: MVar (Maybe AIStore)
    , appHttpMgr :: Maybe Manager
    , appAiToken :: Maybe Text
    , appCliSessions :: MVar (M.Map Text HandleStore)
    , appBuilding :: IORef Bool
    , appBuildingSince :: IORef (Maybe Word64)
    , appAINumCtx :: IORef Int
    , appAIToolLimit :: IORef Int
    }

{- | Which modules a given kernel has loaded. Keyed by the kernel, so a replaced
kernel has none without anyone remembering to say so: a fresh process cannot
have loaded anything, and the record simply stops matching.
-}
loadedModules :: App -> Unique -> IO (M.Map Text Text)
loadedModules app kernel = do
    recorded <- readIORef (appCompiledModules app)
    pure $ case recorded of
        Just (owner, mods) | owner == kernel -> mods
        _ -> M.empty

setLoadedModules :: App -> Unique -> M.Map Text Text -> IO ()
setLoadedModules app kernel mods =
    writeIORef (appCompiledModules app) (Just (kernel, mods))

{- | Forget what a kernel had loaded, for when a @:load@ failed and left the
namespace in a state we can no longer describe.
-}
forgetLoadedModules :: App -> IO ()
forgetLoadedModules app = writeIORef (appCompiledModules app) Nothing

setBuilding :: App -> Bool -> IO ()
setBuilding app True = do
    t <- getMonotonicTimeNSec
    writeIORef (appBuildingSince app) (Just t)
    writeIORef (appBuilding app) True
setBuilding app False = do
    writeIORef (appBuilding app) False
    writeIORef (appBuildingSince app) Nothing

withBuilding :: App -> IO a -> IO a
withBuilding app = bracket_ (setBuilding app True) (setBuilding app False)

getAIStore :: App -> IO (Maybe AIStore)
getAIStore = readMVar . appAI

setAIStore :: App -> Maybe AIStore -> IO ()
setAIStore app val = modifyMVar_ (appAI app) (const (pure val))

broadcastNotebook :: App -> IO ()
broadcastNotebook app = do
    nb <- readNotebook (appNotebook app)
    broadcast (appEvents app) (EvNotebookChanged nb)

{- | Which cells are out of date, and against which kernel. Level-triggered and
narrow on purpose: a client that missed events gets the whole answer without a
full-notebook broadcast, which would re-render over whatever the user is typing.
-}
notebookState :: App -> IO NotebookEvent
notebookState app = do
    nb <- readNotebook (appNotebook app)
    epoch <- currentKernelEpoch (appSessions app)
    pure (EvNotebookState epoch [cellId c | c <- nbCells nb, cellDirty c])

broadcastNotebookState :: App -> IO ()
broadcastNotebookState app = notebookState app >>= broadcast (appEvents app)

resolveCliHandleStore :: App -> Text -> IO HandleStore
resolveCliHandleStore app sid = modifyMVar (appCliSessions app) $ \m ->
    case M.lookup sid m of
        Just hs -> pure (m, hs)
        Nothing -> do
            hs <- newHandleStore
            pure (M.insert sid hs m, hs)
