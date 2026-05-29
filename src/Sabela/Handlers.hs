{-# LANGUAGE OverloadedStrings #-}

{- | Top-level reactive-notebook handlers: edit / run-cell / run-all /
reset / restart, plus the 'ReactiveNotebook' record the server wires
into the SSE bus. The actual machinery lives in three sibling modules:

* 'Sabela.Handlers.Lifecycle' — GHCi session spawn, restart, kill, install.
* 'Sabela.Handlers.Exec' — single-cell execution.
* 'Sabela.Handlers.Plan' — dependency-aware planning + dispatch.
-}
module Sabela.Handlers (
    -- * Reactive notebook interface
    ReactiveNotebook (..),
    setupReactive,

    -- * Initialization
    initGlobalEnv,
    initPreinstalledPackages,

    -- * Haskell session management (also used by tests)
    installAndRestart,
    setupReplProject,
    updateCellSource,
    killAllSessions,
    reloadHaskellSession,

    -- * Re-exports from submodules
    module Sabela.Handlers.Shared,
) where

import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Sabela.AI.Store as AI
import qualified Sabela.AI.Types as AI
import qualified Sabela.Anthropic.Types as AI (cancel)
import Sabela.Deps (collectMetadataFromContent)
import Sabela.Handlers.Lifecycle (
    installAndRestart,
    killAllSessions,
    reloadHaskellSession,
    setupReplProject,
 )
import Sabela.Handlers.Plan (
    dispatchByLang,
    executeAffected,
    executeFullRestart,
    executeSingleCell,
 )
import Sabela.Handlers.Shared
import Sabela.Model (
    Cell (..),
    Notebook (..),
    NotebookEvent (..),
    SessionStatus (..),
    cellLangOf,
 )
import Sabela.State (App (..), getAIStore)
import Sabela.State.NotebookStore (modifyNotebook, readNotebook)
import ScriptHs.Parser (CabalMeta (..))
import System.Directory (doesFileExist)

initGlobalEnv :: FilePath -> IO (Set Text)
initGlobalEnv path = do
    exists <- doesFileExist path
    if not exists
        then pure S.empty
        else do
            content <- TIO.readFile path
            pure (S.fromList (metaDeps (collectMetadataFromContent content)))

initPreinstalledPackages :: FilePath -> [String] -> IO (Set Text)
initPreinstalledPackages _ [] = pure S.empty
initPreinstalledPackages _ pkgs = pure (S.fromList (map T.pack pkgs))

data ReactiveNotebook = ReactiveNotebook
    { rnCellEdit :: Int -> Text -> IO ()
    , rnRunCell :: Int -> IO ()
    , rnRunAll :: IO ()
    , rnReset :: IO ()
    , rnRestartKernel :: IO ()
    , rnWidgetCell :: Int -> IO ()
    }

setupReactive :: App -> IO ReactiveNotebook
setupReactive app =
    pure $
        ReactiveNotebook
            { rnCellEdit = handleCellEdit app
            , rnRunCell = handleRunCell app
            , rnRunAll = handleRunAll app
            , rnReset = handleReset app
            , rnRestartKernel = handleRestartKernel app
            , rnWidgetCell = handleWidgetCell app
            }

handleCellEdit :: App -> Int -> Text -> IO ()
handleCellEdit app cid src = do
    debugLog app $ "[handler] handleCellEdit: cell " <> T.pack (show cid)
    modifyNotebook (appNotebook app) $ updateCellSource cid src
    nb <- readNotebook (appNotebook app)
    gen <- bumpGeneration app
    dispatchByLang app gen cid (cellLangOf cid nb) (executeAffected app gen cid)

updateCellSource :: Int -> Text -> Notebook -> Notebook
updateCellSource cid src nb =
    nb{nbCells = map upd (nbCells nb)}
  where
    upd c
        | cellId c == cid = c{cellSource = src, cellDirty = True}
        | otherwise = c

handleWidgetCell :: App -> Int -> IO ()
handleWidgetCell app cid = do
    debugLog app $ "[handler] handleWidgetCell: cell " <> T.pack (show cid)
    gen <- bumpGeneration app
    void $ forkIO $ executeAffected app gen cid

handleRunCell :: App -> Int -> IO ()
handleRunCell app cid = do
    debugLog app $ "[handler] handleRunCell: cell " <> T.pack (show cid)
    nb <- readNotebook (appNotebook app)
    gen <- bumpGeneration app
    dispatchByLang app gen cid (cellLangOf cid nb) $
        void $
            forkIO $
                executeSingleCell app gen cid

handleRunAll :: App -> IO ()
handleRunAll app = do
    debugLog app "[handler] handleRunAll: fullRestart"
    gen <- bumpGeneration app
    void $ forkIO $ executeFullRestart app gen

handleReset :: App -> IO ()
handleReset app = do
    debugLog app "[handler] handleReset"
    void $ bumpGeneration app
    void $ forkIO $ killAllSessions app
    cleanupAI app True
    modifyNotebook (appNotebook app) clearAllOutputs
    broadcast app (EvSessionStatus SReset)

handleRestartKernel :: App -> IO ()
handleRestartKernel app = do
    debugLog app "[handler] handleRestartKernel"
    gen <- bumpGeneration app
    cleanupAI app False
    broadcast app (EvSessionStatus SReset)
    void $ forkIO $ executeFullRestart app gen

{- | Cleanup AI state on reset/restart.
fullReset clears conversation and reverts edits; partial only kills scratchpad.
-}
cleanupAI :: App -> Bool -> IO ()
cleanupAI app fullReset = do
    mStore <- getAIStore app
    case mStore of
        Nothing -> pure ()
        Just store -> do
            mTurn <- AI.getCurrentTurn store
            case mTurn of
                Just turn -> AI.cancel (AI.turnCancel turn)
                Nothing -> pure ()
            AI.clearScratchpad store
            when fullReset $ do
                AI.clearConversation store
                AI.revertAllPendingEdits store

clearAllOutputs :: Notebook -> Notebook
clearAllOutputs nb = nb{nbCells = map clr (nbCells nb)}
  where
    clr c = c{cellOutputs = [], cellError = Nothing, cellDirty = False}
