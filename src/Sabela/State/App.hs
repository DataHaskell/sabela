module Sabela.State.App (
    App (..),
    clearCompiledModules,
    setBuilding,
    withBuilding,
    getAIStore,
    setAIStore,
    broadcastNotebook,
    resolveCliHandleStore,
) where

import Control.Concurrent.MVar (
    MVar,
    modifyMVar,
    modifyMVar_,
    readMVar,
 )
import Control.Exception (bracket_)
import Data.IORef (IORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client (Manager)

import Sabela.AI.Handles (HandleStore, newHandleStore)
import Sabela.AI.Store (AIStore)
import Sabela.Model (NotebookEvent (..))
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
    , appCompiledModules :: IORef (M.Map Text Text)
    , appAI :: MVar (Maybe AIStore)
    , appHttpMgr :: Maybe Manager
    , appAiToken :: Maybe Text
    , appCliSessions :: MVar (M.Map Text HandleStore)
    , appBuilding :: IORef Bool
    , appBuildingSince :: IORef (Maybe Word64)
    , appAINumCtx :: IORef Int
    , appAIToolLimit :: IORef Int
    }

clearCompiledModules :: App -> IO ()
clearCompiledModules app = writeIORef (appCompiledModules app) M.empty

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

resolveCliHandleStore :: App -> Text -> IO HandleStore
resolveCliHandleStore app sid = modifyMVar (appCliSessions app) $ \m ->
    case M.lookup sid m of
        Just hs -> pure (m, hs)
        Nothing -> do
            hs <- newHandleStore
            pure (M.insert sid hs m, hs)
