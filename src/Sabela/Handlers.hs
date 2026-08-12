{-# LANGUAGE OverloadedStrings #-}

module Sabela.Handlers (
    ReactiveNotebook (..),
    setupReactive,
    cellRunnable,
    initGlobalEnv,
    initPreinstalledPackages,
    installAndRestart,
    clearAllOutputs,
    ReplSupport (..),
    buildTimeSupportDir,
    setupReplProject,
    resolveLocalPackages,
    updateCellSource,
    killAllSessions,
    shutdownAllSessions,
    killSessionAsync,
    module Sabela.Handlers.Shared,
) where

import Control.Concurrent (forkIO)
import Control.Monad (void, when)
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
    executeRestartOnly,
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
import Sabela.Parse.Change (significantCodeChange)
import Sabela.Reactivity (
    RestartMode (..),
    applyRestart,
    cellStale,
    clearCellResult,
    haskellCodeCells,
    markDependentsDirty,
    runAllNeedsRun,
 )
import Sabela.Session.Project (ReplSupport (..), buildTimeSupportDir)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), broadcastNotebookState, getAIStore)
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
    , rnRunCellForced :: Int -> IO ()
    , rnRunAll :: IO ()
    , rnRestart :: RestartMode -> IO ()
    , rnWidgetCell :: Int -> IO ()
    }

setupReactive :: App -> IO ReactiveNotebook
setupReactive app =
    pure $
        ReactiveNotebook
            { rnCellEdit = handleCellEdit app
            , rnRunCell = handleRunCell app
            , rnRunCellForced = handleRunCellForced app
            , rnRunAll = handleRunAll app
            , rnRestart = handleRestart app
            , rnWidgetCell = handleWidgetCell app
            }

handleCellEdit :: App -> Int -> Text -> IO ()
handleCellEdit app cid src = do
    debugLog app $ "[handler] handleCellEdit: cell " <> T.pack (show cid)
    before <- readNotebook (appNotebook app)
    let significant = case find (\c -> cellId c == cid) (nbCells before) of
            Just c -> editRequiresRun c src
            Nothing -> False
    modifyNotebook (appNotebook app) $ updateCellSource cid src
    broadcastNotebookState app
    when significant $ do
        nb <- readNotebook (appNotebook app)
        gen <- bumpGeneration app
        dispatchByLang app gen cid (cellLangOf cid nb) (executeAffected app gen cid)

{- | Does this edit change what the kernel would run? Prose never does; a
Haskell cell only when its tokens or directives change ('significantCodeChange');
any other cell on any textual change.
-}
editRequiresRun :: Cell -> Text -> Bool
editRequiresRun c src = case (cellType c, cellLang c) of
    (ProseCell, _) -> False
    (CodeCell, ST.Haskell) -> significantCodeChange (cellSource c) src
    _ -> cellSource c /= src

updateCellSource :: Int -> Text -> Notebook -> Notebook
updateCellSource cid src nb
    | not changed = nb
    | not significant = nb{nbCells = map keepSource (nbCells nb)}
    | otherwise = markDependentsDirty cid nb{nbCells = map invalidate (nbCells nb)}
  where
    (changed, significant) = case find (\c -> cellId c == cid) (nbCells nb) of
        Just c -> (cellSource c /= src, editRequiresRun c src)
        Nothing -> (False, False)
    keepSource c
        | cellId c == cid = c{cellSource = src}
        | otherwise = c
    invalidate c
        | cellId c == cid = c{cellSource = src, cellDirty = True}
        | otherwise = c

handleWidgetCell :: App -> Int -> IO ()
handleWidgetCell app cid = do
    debugLog app $ "[handler] handleWidgetCell: cell " <> T.pack (show cid)
    gen <- bumpGeneration app
    void $ forkIO $ executeAffected app gen cid

handleRunCell :: App -> Int -> IO ()
handleRunCell = handleRunCellWith False

handleRunCellForced :: App -> Int -> IO ()
handleRunCellForced = handleRunCellWith True

handleRunCellWith :: Bool -> App -> Int -> IO ()
handleRunCellWith force app cid = do
    debugLog app $ "[handler] handleRunCell: cell " <> T.pack (show cid)
    nb <- readNotebook (appNotebook app)
    if not (cellRunnable force (find (\c -> cellId c == cid) (nbCells nb)))
        then do
            debugLog app "[handler] handleRunCell: cell unchanged; skipping"
            broadcast app EvExecutionDone
        else do
            gen <- bumpGeneration app
            dispatchByLang app gen cid (cellLangOf cid nb) $
                executeSingleCell app gen cid

cellRunnable :: Bool -> Maybe Cell -> Bool
cellRunnable _ Nothing = False
cellRunnable force (Just c) = force || cellType c /= CodeCell || cellStale c

handleRunAll :: App -> IO ()
handleRunAll app = do
    debugLog app "[handler] handleRunAll"
    nb <- readNotebook (appNotebook app)
    building <- readIORef (appBuilding app)
    ready <- isSessionUpToDate app nb
    if not (runAllNeedsRun building ready (haskellCodeCells nb) nb)
        then do
            debugLog
                app
                "[handler] handleRunAll: nothing to run (clean, or a build is in flight); skipping"
            broadcast app EvExecutionDone
        else do
            gen <- bumpGeneration app
            void $ forkIO $ executeRunAll app gen

{- | All three restarts respawn the kernel. Only 'RestartRunAll' executes
afterwards: restarting because a cell hangs must not immediately re-run it.
-}
handleRestart :: App -> RestartMode -> IO ()
handleRestart app mode = do
    debugLog app $ "[handler] handleRestart: " <> T.pack (show mode)
    gen <- bumpGeneration app
    cleanupAI app (mode == RestartClear)
    modifyNotebook (appNotebook app) (applyRestart mode)
    broadcastNotebookState app
    broadcast app (EvSessionStatus SReset)
    void . forkIO $ case mode of
        RestartRunAll -> executeFullRestart app gen
        _ -> executeRestartOnly app gen

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

{- | Reset kills every session, so afterwards no kernel holds anything and every
code cell must come back invalidated rather than clean.
-}
clearAllOutputs :: Notebook -> Notebook
clearAllOutputs nb = nb{nbCells = map clearCellResult (nbCells nb)}
