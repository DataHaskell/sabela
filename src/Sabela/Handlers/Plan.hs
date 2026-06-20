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
    executeRunAll,
    isSessionUpToDate,

    -- * Sub-pieces (exposed for the entry-points module and tests)
    rerunBridgeCells,
    runPlanPhases,
) where

import Control.Concurrent (forkIO)
import Control.Monad (forM_, unless, void, when)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

import Sabela.Compiled (CompilePlan (..))
import Sabela.Deps (collectMetadata)
import Sabela.Handlers.Compile (CompileOutcome (..), runCompilePhase)
import Sabela.Handlers.Exec (runAndBroadcast)
import Sabela.Handlers.Lifecycle (
    ensureSessionAlive,
    installAndRestart,
    killAllSessions,
    loadSabelaPrelude,
    sessionMetaMatches,
 )
import Sabela.Handlers.PlanErrors (broadcastPlanErrors)
import Sabela.Handlers.PostCompile (runCellList, runPostCompile)
import Sabela.Handlers.Python (
    executePythonCell,
    executePythonCells,
    executeStalePythonCells,
 )
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
    computeStaleExecutionPlan,
    haskellCodeCells,
 )
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.BridgeStore (getBridgeValues)
import Sabela.State.NotebookStore (readNotebook)
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
                    else
                        if M.member cid (cpCellModule (epCompilePlan plan))
                            then do
                                outcome <-
                                    runCompilePhase app gen (epCompilePlan plan) [cell]
                                runPostCompile app gen plan outcome []
                            else runAndBroadcast app gen cell
        Nothing -> pure ()

cellInSkipSet :: Int -> ExecutionPlan -> Bool
cellInSkipSet cid plan =
    S.member cid $
        epCycleIds plan
            `S.union` M.keysSet (epRedefErrors plan)
            `S.union` M.keysSet (cpViolations (epCompilePlan plan))

{- | Run-all entry: when the live session is current, only stale cells
(changed or errored) and their dependents re-run; otherwise the full
restart path runs everything against a fresh interpreter.
-}
executeRunAll :: App -> Int -> IO ()
executeRunAll app gen = do
    nb <- readNotebook (appNotebook app)
    sessionReady <- isSessionUpToDate app nb
    if sessionReady
        then executeStaleRun app gen nb
        else executeFullRestart app gen

executeStaleRun :: App -> Int -> Notebook -> IO ()
executeStaleRun app gen nb = do
    let allCode = haskellCodeCells nb
        plan = computeStaleExecutionPlan allCode nb
    debugLog app $
        T.pack $
            "[handler] executeStaleRun: " ++ show (map cellId (epCellsToRun plan))
    runPlanPhases app gen plan
    whenCurrentGen app gen $ executeStaleNonHaskell app gen

executeStaleNonHaskell :: App -> Int -> IO ()
executeStaleNonHaskell app gen = do
    whenCurrentGen app gen $ do
        oldBridge <- getBridgeValues (appBridge app)
        executeStalePythonCells app gen
        newBridge <- getBridgeValues (appBridge app)
        when (oldBridge /= newBridge) $ rerunBridgeCells app gen
    whenCurrentGen app gen $ broadcast app EvExecutionDone

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
    runPlanPhases app gen plan

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
isSessionUpToDate app nb = sessionMetaMatches app (collectMetadata nb)

executeIncrementalPlan :: App -> Int -> Int -> Notebook -> IO ()
executeIncrementalPlan app gen editedCid nb = do
    let allCode = haskellCodeCells nb
        plan = computeExecutionPlan editedCid allCode nb
    logExecutionPlan app allCode plan
    runPlanPhases app gen plan
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

{- | Run a plan: plan-level errors, then the compile phase (generated
modules), then interpreted cells. Any actual reload — successful or not —
wiped every prompt binding, so the interpreted run set escalates to all
interpreted cells; a failed compile additionally skips the interpreted
cells that transitively depend on a compiled cell.
-}
runPlanPhases :: App -> Int -> ExecutionPlan -> IO ()
runPlanPhases app gen plan = do
    broadcastPlanErrors app plan Nothing
    outcome <-
        if null (epCompileCells plan)
            then pure CompileNoChange
            else runCompilePhase app gen (epCompilePlan plan) (epCompileCells plan)
    runPostCompile app gen plan outcome (epCellsToRun plan)
