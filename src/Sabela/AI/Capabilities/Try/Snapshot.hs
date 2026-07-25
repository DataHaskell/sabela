{- | The pre\/post state fingerprint 'Sabela.AI.Capabilities.Try' compares
around every trial to prove it left no trace — split out for the
module-size cap.
-}
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

import Sabela.Deps (ProjectSig)
import Sabela.Model (Notebook)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.BridgeStore (getBridgeValues)
import Sabela.State.DependencyTracker (
    getHaskellDeps,
    getHaskellExts,
    getHaskellProjectSig,
    getPythonDeps,
 )
import Sabela.State.EventBus (EventBus (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession)
import Sabela.State.WidgetStore (WidgetStore (..))

data AppSnapshot = AppSnapshot
    { snapshotNotebook :: Notebook
    , snapshotEventGeneration :: Int
    , snapshotHaskellDeps :: S.Set Text
    , snapshotHaskellExts :: S.Set Text
    , snapshotProjectSig :: ProjectSig
    , snapshotPythonDeps :: S.Set Text
    , snapshotCompiledModules :: M.Map Text Text
    , snapshotBridgeValues :: M.Map Text Text
    , snapshotWidgetValues :: M.Map Int (M.Map Text Text)
    , snapshotHaskellSession :: Maybe (Unique, Int)
    }
    deriving (Eq)

snapshotApp :: App -> IO AppSnapshot
snapshotApp app = do
    notebook <- readNotebook (appNotebook app)
    eventGeneration <- readIORef (ebGeneration (appEvents app))
    haskellDeps <- getHaskellDeps (appDeps app)
    haskellExts <- getHaskellExts (appDeps app)
    projectSig <- getHaskellProjectSig (appDeps app)
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
            , snapshotHaskellDeps = haskellDeps
            , snapshotHaskellExts = haskellExts
            , snapshotProjectSig = projectSig
            , snapshotPythonDeps = pythonDeps
            , snapshotCompiledModules = compiled
            , snapshotBridgeValues = bridge
            , snapshotWidgetValues = widgets
            , snapshotHaskellSession = sessionFingerprint
            }
