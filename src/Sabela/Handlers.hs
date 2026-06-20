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
    resolveLocalPackages,
    updateCellSource,
    killAllSessions,
    shutdownAllSessions,
    killSessionAsync,

    -- * Re-exports from submodules
    module Sabela.Handlers.Shared,
) where

import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import Data.Foldable (find)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.IORef (readIORef)
import qualified Sabela.AI.Store as AI
import qualified Sabela.AI.Types as AI
import qualified Sabela.Anthropic.Types as AI (cancel)
import Sabela.Deps (collectMetadataFromContent)
import Sabela.Handlers.Lifecycle (
    installAndRestart,
    killAllSessions,
    killSessionAsync,
    resolveLocalPackages,
    setupReplProject,
    shutdownAllSessions,
 )
import Sabela.Handlers.Plan (
    dispatchByLang,
    executeAffected,
    executeFullRestart,
    executeRunAll,
    executeSingleCell,
    isSessionUpToDate,
 )
import Sabela.Handlers.Shared
import Sabela.Model (
    Cell (..),
    CellType (..),
    Notebook (..),
    NotebookEvent (..),
    SessionStatus (..),
    cellLangOf,
 )
import Sabela.Reactivity (
    cellStale,
    haskellCodeCells,
    markDependentsDirty,
    runAllNeedsRun,
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

{- | Apply an edit and run reactively — but only when the source really
changed; an identical write (e.g. the run button's flush) is a no-op so
clean cells are never re-executed.
-}
handleCellEdit :: App -> Int -> Text -> IO ()
handleCellEdit app cid src = do
    debugLog app $ "[handler] handleCellEdit: cell " <> T.pack (show cid)
    before <- readNotebook (appNotebook app)
    let changed = case find (\c -> cellId c == cid) (nbCells before) of
            Just c -> cellSource c /= src
            Nothing -> False
    modifyNotebook (appNotebook app) $ updateCellSource cid src
    when changed $ do
        nb <- readNotebook (appNotebook app)
        gen <- bumpGeneration app
        dispatchByLang app gen cid (cellLangOf cid nb) (executeAffected app gen cid)

{- | Update a cell's source. On a real change the cell AND its transitive
dependents go dirty (so staleness survives solo runs); an identical
write changes nothing.
-}
updateCellSource :: Int -> Text -> Notebook -> Notebook
updateCellSource cid src nb
    | not changed = nb
    | otherwise = markDependentsDirty cid nb{nbCells = map upd (nbCells nb)}
  where
    changed = case find (\c -> cellId c == cid) (nbCells nb) of
        Just c -> cellSource c /= src
        Nothing -> False
    upd c
        | cellId c == cid = c{cellSource = src, cellDirty = True}
        | otherwise = c

handleWidgetCell :: App -> Int -> IO ()
handleWidgetCell app cid = do
    debugLog app $ "[handler] handleWidgetCell: cell " <> T.pack (show cid)
    gen <- bumpGeneration app
    void $ forkIO $ executeAffected app gen cid

{- | Run one cell — unless it is clean (unchanged since its last
successful run), in which case the click is a no-op.
-}
handleRunCell :: App -> Int -> IO ()
handleRunCell app cid = do
    debugLog app $ "[handler] handleRunCell: cell " <> T.pack (show cid)
    nb <- readNotebook (appNotebook app)
    let runnable = case find (\c -> cellId c == cid) (nbCells nb) of
            Just c -> cellType c /= CodeCell || cellStale c
            Nothing -> False
    if not runnable
        then debugLog app "[handler] handleRunCell: cell unchanged; skipping"
        else do
            gen <- bumpGeneration app
            dispatchByLang app gen cid (cellLangOf cid nb) $
                void $
                    forkIO $
                        executeSingleCell app gen cid

handleRunAll :: App -> IO ()
handleRunAll app = do
    debugLog app "[handler] handleRunAll"
    nb <- readNotebook (appNotebook app)
    building <- readIORef (appBuilding app)
    ready <- isSessionUpToDate app nb
    if not (runAllNeedsRun building ready (haskellCodeCells nb) nb)
        then
            debugLog
                app
                "[handler] handleRunAll: nothing to run (clean, or a build is in flight); skipping"
        else do
            gen <- bumpGeneration app
            void $ forkIO $ executeRunAll app gen

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
