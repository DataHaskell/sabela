{-# LANGUAGE OverloadedStrings #-}

{- | Reactive-execution planning + plan-driven dispatch: turn a cell edit
into the ordered list of downstream cells to re-run, broadcast plan-level
errors (cycles, redefinitions), and step through the resulting cells via
'Sabela.Handlers.Exec.runAndBroadcast'. Also owns the dispatch into the
Python sub-engine for non-Haskell cells.
-}
module Sabela.Handlers.Plan (
    -- * Dispatchers
    dispatchByLang,
    executeAffected,
    executeSingleCell,
    executeFullRestart,

    -- * Sub-pieces (exposed for the entry-points module)
    rerunBridgeCells,
) where

import Control.Concurrent (forkIO)
import Control.Monad (forM_, unless, void, when)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

import Sabela.Deps (collectMetadata)
import Sabela.Handlers.Exec (runAndBroadcast)
import Sabela.Handlers.Lifecycle (
    depsMatch,
    ensureSessionAlive,
    installAndRestart,
    killAllSessions,
    loadSabelaPrelude,
 )
import Sabela.Handlers.Python (executePythonCell, executePythonCells)
import Sabela.Handlers.Shared
import Sabela.Model (
    Cell (..),
    CellType (..),
    Notebook (..),
    NotebookEvent (..),
 )
import Sabela.Reactivity (
    ExecutionPlan (..),
    computeExecutionPlan,
    computeFullExecutionPlan,
    cycleErrorMsg,
    haskellCodeCells,
    redefinitionErrorMsg,
 )
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.BridgeStore (getBridgeValues)
import Sabela.State.DependencyTracker (getHaskellDeps, getHaskellExts)
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession)
import qualified Sabela.Topo as Topo

dispatchByLang :: App -> Int -> Int -> ST.CellLang -> IO () -> IO ()
dispatchByLang app gen _cid lang haskellAction =
    case lang of
        ST.Python -> void $ forkIO $ do
            executePythonCell app gen _cid
            whenCurrentGen app gen $ broadcast app EvExecutionDone
        ST.Haskell -> haskellAction

rerunBridgeCells :: App -> Int -> IO ()
rerunBridgeCells app gen = do
    nb <- readNotebook (appNotebook app)
    let hsCells = filter isBridgeDependent (nbCells nb)
    unless (null hsCells) $ do
        debugLog app $
            "[handler] Bridge changed, re-running "
                <> T.pack (show (length hsCells))
                <> " Haskell cells"
        loadSabelaPrelude app
        runCellList app gen hsCells

isBridgeDependent :: Cell -> Bool
isBridgeDependent c =
    cellType c == CodeCell
        && cellLang c == ST.Haskell
        && "_bridge_" `T.isInfixOf` cellSource c

executeSingleCell :: App -> Int -> Int -> IO ()
executeSingleCell app gen cid = do
    debugLog app "[handler] executeSingleCell"
    nb <- readNotebook (appNotebook app)
    let allCode = haskellCodeCells nb
        plan = computeFullExecutionPlan allCode nb
    ok <- ensureSessionAlive app gen (collectMetadata nb)
    when ok $ executeSingleCellPlan app gen cid allCode plan
    whenCurrentGen app gen $ broadcast app EvExecutionDone

executeSingleCellPlan :: App -> Int -> Int -> [Cell] -> ExecutionPlan -> IO ()
executeSingleCellPlan app gen cid allCode plan =
    case find (\c -> cellId c == cid) allCode of
        Just cell ->
            whenCurrentGen app gen $
                if cellInSkipSet cid plan
                    then broadcastPlanErrors app plan (Just cid)
                    else runAndBroadcast app gen cell
        Nothing -> pure ()

cellInSkipSet :: Int -> ExecutionPlan -> Bool
cellInSkipSet cid plan =
    S.member cid (epCycleIds plan `S.union` M.keysSet (epRedefErrors plan))

executeFullRestart :: App -> Int -> IO ()
executeFullRestart app gen = do
    debugLog app "[handler] executeFullRestart: killing session, running all"
    whenCurrentGen app gen $ do
        nb <- readNotebook (appNotebook app)
        let allCode = haskellCodeCells nb
        killAllSessions app
        whenCurrentGen app gen $ do
            ok <- installAndRestart app gen (collectMetadata nb)
            when ok $ executeFullPlan app gen allCode nb
        whenCurrentGen app gen $
            executeNonHaskellCells app gen

executeFullPlan :: App -> Int -> [Cell] -> Notebook -> IO ()
executeFullPlan app gen allCode nb = do
    let plan = computeFullExecutionPlan allCode nb
    broadcastPlanErrors app plan Nothing
    runCellList app gen (epCellsToRun plan)

executeNonHaskellCells :: App -> Int -> IO ()
executeNonHaskellCells app gen = do
    debugLog app "[handler] executeNonHaskellCells: starting"
    whenCurrentGen app gen $ do
        debugLog app "[handler] executeNonHaskellCells: running Python cells"
        oldBridge <- getBridgeValues (appBridge app)
        executePythonCells app gen
        newBridge <- getBridgeValues (appBridge app)
        when (oldBridge /= newBridge) $ rerunBridgeCells app gen
    whenCurrentGen app gen $ broadcast app EvExecutionDone

executeAffected :: App -> Int -> Int -> IO ()
executeAffected app gen editedCid = do
    debugLog app $
        "[handler] executeAffected: editedCid=" <> T.pack (show editedCid)
    nb <- readNotebook (appNotebook app)
    sessionReady <- isSessionUpToDate app nb
    if sessionReady
        then executeIncrementalPlan app gen editedCid nb
        else executeFullRestartPlan app gen nb

isSessionUpToDate :: App -> Notebook -> IO Bool
isSessionUpToDate app nb = do
    installed <- getHaskellDeps (appDeps app)
    instExts <- getHaskellExts (appDeps app)
    mSess <- getHaskellSession (appSessions app)
    let needed = collectMetadata nb
        match = depsMatch needed installed instExts (envGlobalDeps (appEnv app))
    case mSess of
        Just _ | match -> pure True
        _ -> pure False

executeIncrementalPlan :: App -> Int -> Int -> Notebook -> IO ()
executeIncrementalPlan app gen editedCid nb = do
    let allCode = haskellCodeCells nb
        plan = computeExecutionPlan editedCid allCode nb
    logExecutionPlan app allCode plan
    broadcastPlanErrors app plan Nothing
    runCellList app gen (epCellsToRun plan)
    whenCurrentGen app gen $ broadcast app EvExecutionDone

executeFullRestartPlan :: App -> Int -> Notebook -> IO ()
executeFullRestartPlan app gen nb = do
    debugLog app "[handler] No session or deps changed -> full restart"
    let allCode = haskellCodeCells nb
    ok <- installAndRestart app gen (collectMetadata nb)
    when ok $ executeFullPlan app gen allCode nb
    whenCurrentGen app gen $ broadcast app EvExecutionDone

logExecutionPlan :: App -> [Cell] -> ExecutionPlan -> IO ()
logExecutionPlan app allCode plan = do
    debugLog app $
        T.pack $
            "[handler] All code cells: " ++ show (map cellId allCode)
    debugLog app $
        T.pack $
            "[handler] WILL RUN: " ++ show (map cellId (epCellsToRun plan))
    debugLog app $
        T.pack $
            "[handler] Cycle cells: " ++ show (S.toList (epCycleIds plan))
    debugLog app $
        T.pack $
            "[handler] Redef cells: " ++ show (M.keys (epRedefErrors plan))
    forM_ allCode $ \c -> logCellDeps app c

logCellDeps :: App -> Cell -> IO ()
logCellDeps app c = do
    let (defs, uses) = Topo.cellNames (cellSource c)
        usesPreview = take 10 (S.toList uses) ++ ["..." | S.size uses > 10]
    debugLog app $
        T.pack $
            "[handler]   cell "
                ++ show (cellId c)
                ++ " defines="
                ++ show (S.toList defs)
                ++ " uses="
                ++ show usesPreview

broadcastPlanErrors :: App -> ExecutionPlan -> Maybe Int -> IO ()
broadcastPlanErrors app plan filterCid = do
    broadcastRedefErrors app plan filterCid
    broadcastCycleErrors app plan filterCid

broadcastRedefErrors :: App -> ExecutionPlan -> Maybe Int -> IO ()
broadcastRedefErrors app plan filterCid = do
    let redefMap = filterByCell filterCid (epRedefErrors plan)
    forM_ (M.toList redefMap) $ \(cid, names) ->
        broadcastCellError
            app
            cid
            (redefinitionErrorMsg (epDefMap plan) (epCellPositions plan) cid names)

broadcastCycleErrors :: App -> ExecutionPlan -> Maybe Int -> IO ()
broadcastCycleErrors app plan filterCid = do
    let cycleIds = filterCycleIds filterCid (epCycleIds plan)
    unless (S.null cycleIds) $ do
        nb <- readNotebook (appNotebook app)
        let cells = nbCells nb
            msg =
                cycleErrorMsg
                    (epCellPositions plan)
                    cycleIds
                    cells
                    (epDefMap plan)
        forM_ (S.toList cycleIds) $ \cid -> broadcastCellError app cid msg

filterByCell :: Maybe Int -> M.Map Int a -> M.Map Int a
filterByCell Nothing m = m
filterByCell (Just cid) m = M.filterWithKey (\k _ -> k == cid) m

filterCycleIds :: Maybe Int -> S.Set Int -> S.Set Int
filterCycleIds Nothing s = s
filterCycleIds (Just cid) s = S.intersection s (S.singleton cid)

runCellList :: App -> Int -> [Cell] -> IO ()
runCellList app gen cells =
    forM_ cells $ \cell ->
        whenCurrentGen app gen $ runAndBroadcast app gen cell
