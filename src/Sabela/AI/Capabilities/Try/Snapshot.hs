module Sabela.AI.Capabilities.Try.Snapshot (
    AppSnapshot (..),
    snapshotApp,
) where

import Control.Concurrent.MVar (readMVar)
import Data.IORef (readIORef)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import Data.Unique (Unique)

import Sabela.Deps (EnvSig)
import Sabela.Model (Notebook)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.BridgeStore (getBridgeValues)
import Sabela.State.DependencyTracker (
    getPythonDeps,
 )
import Sabela.State.EventBus (EventBus (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession, haskellEnvOf)
import Sabela.State.WidgetStore (WidgetStore (..))

data AppSnapshot = AppSnapshot
    { snapshotNotebook :: Notebook
    , snapshotEventGeneration :: Int
    , snapshotHaskellEnv :: Maybe (Unique, EnvSig)
    , snapshotPythonDeps :: S.Set Text
    , snapshotCompiledModules :: Maybe (Unique, M.Map Text Text)
    , snapshotBridgeValues :: M.Map Text Text
    , snapshotWidgetValues :: M.Map Int (M.Map Text Text)
    , snapshotHaskellSession :: Maybe (Unique, Int)
    }
    deriving (Eq)

snapshotApp :: App -> IO AppSnapshot
snapshotApp app = do
    notebook <- readNotebook (appNotebook app)
    eventGeneration <- readIORef (ebGeneration (appEvents app))
    haskellEnv <- haskellEnvOf (appSessions app)
    pythonDeps <- getPythonDeps (appDeps app)
    compiled <- readIORef (appCompiledModules app)
    bridge <- getBridgeValues (appBridge app)
    widgets <- readMVar (wsValues (appWidgets app))
    session <- getHaskellSession (appSessions app)
    sessionFingerprint <- case session of
        Nothing -> pure Nothing
        Just backend -> do
            generation <- ST.sbSessionGen backend
            pure (Just (ST.sbSessionId backend, generation))
    pure
        AppSnapshot
            { snapshotNotebook = notebook
            , snapshotEventGeneration = eventGeneration
            , snapshotHaskellEnv = haskellEnv
            , snapshotPythonDeps = pythonDeps
            , snapshotCompiledModules = compiled
            , snapshotBridgeValues = bridge
            , snapshotWidgetValues = widgets
            , snapshotHaskellSession = sessionFingerprint
            }
