{-# LANGUAGE OverloadedStrings #-}

module Sabela.Handlers.Plan (
    dispatchByLang,
    executeAffected,
    executeSingleCell,
    executeFullRestart,
    executeRestartOnly,
    executeRunAll,
    isSessionUpToDate,
    planInputs,
    rerunBridgeConsumers,
    runPlanPhases,
) where

import Control.Concurrent (forkIO)
import Control.Monad (unless, void, when)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

import Sabela.Compiled (CompilePlan (..))
import Sabela.Deps (collectMetadata)
import Sabela.Handlers.Compile (CompileOutcome (..), runCompilePhase)
import Sabela.Handlers.Exec (runAndBroadcast)
import Sabela.Handlers.Lifecycle (
    installAndRestartUnlocked,
    killAllSessionsUnlocked,
 )
import Sabela.Handlers.Plan.Inputs (isSessionUpToDate, planInputs)
import Sabela.Handlers.Plan.Log (logExecutionPlan)
import Sabela.Handlers.PlanErrors (broadcastPlanErrors)
import Sabela.Handlers.PostCompile (runPostCompile)
import Sabela.Handlers.Python (
    executePythonCell,
    executePythonCells,
    executeStalePythonCells,
 )
import Sabela.Handlers.Shared
import Sabela.Model (
    Cell (..),
    Notebook (..),
    NotebookEvent (..),
 )
import Sabela.Reactivity (
    EnvState (..),
    ExecutionPlan (..),
    ModuleState (..),
    bridgeConsumers,
    changedBridgeValues,
    computeExecutionPlanIn,
    computeFullExecutionPlan,
    computeRootedExecutionPlan,
    computeStaleExecutionPlanIn,
    haskellCodeCells,
 )
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.BridgeStore (getBridgeValues)
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (withHaskellLifecycle)

{- | Both branches fork. An edit arrives on a synchronous @PUT@ and can rebuild
the environment, so running inline held the response open for a whole package
install. Results reach the client over SSE, not through that response.
-}
dispatchByLang :: App -> Int -> Int -> ST.CellLang -> IO () -> IO ()
dispatchByLang app gen _cid lang haskellAction =
    void $ forkIO $ case lang of
        ST.Python -> do
            executePythonCell app gen _cid
            whenCurrentGen app gen $ broadcast app EvExecutionDone
        ST.Haskell -> haskellAction

{- | Re-run what Python just invalidated, through the one planner. An exported
value is an edge like any other: consumers come from the parser, not a substring
match, and only changed values are rooted; reachability does the rest.
-}
rerunBridgeConsumers :: App -> Int -> M.Map T.Text T.Text -> IO ()
rerunBridgeConsumers app gen before = do
    after <- getBridgeValues (appBridge app)
    nb <- readNotebook (appNotebook app)
    (env, mods) <- planInputs app nb
    let allCode = haskellCodeCells nb
        roots = bridgeConsumers (changedBridgeValues before after) allCode
    unless (S.null roots) $ do
        debugLog app $
            "[handler] bridge changed, re-running from "
                <> T.pack (show (S.toList roots))
        executePlan app gen nb (computeRootedExecutionPlan env mods roots allCode nb)

-- | The environment as the planner sees it: a node that is either current or not.

{- | Run one plan. A stale environment is rebuilt first, and because that makes
every cell a root the rest follows from ordinary reachability — nothing has to
remember to invalidate anything afterwards.
-}
executePlan :: App -> Int -> Notebook -> ExecutionPlan -> IO ()
executePlan app gen nb plan = do
    envOk <- if epRunEnv plan then runEnvNode app gen nb else pure True
    when envOk $ whenCurrentGen app gen $ runPlanPhases app gen plan

{- | Execute the environment node: replace the kernel with one built for the
notebook as it stands. Returns False if the rebuild failed or was superseded, in
which case no cell may run against it.
-}
runEnvNode :: App -> Int -> Notebook -> IO Bool
runEnvNode app gen nb =
    withHaskellLifecycle (appSessions app) $ do
        debugLog app "[handler] runEnvNode: rebuilding the kernel"
        killAllSessionsUnlocked app
        current <- isCurrentGen app gen
        if not current
            then pure False
            else installAndRestartUnlocked app gen (collectMetadata nb)

{- | An explicit Run of one cell. While the environment is current that means
exactly that cell; once it is not, the kernel has to be rebuilt first, and a
rebuilt kernel holds nobody's bindings — so the notebook runs, not just the cell.
-}
executeSingleCell :: App -> Int -> Int -> IO ()
executeSingleCell app gen cid = do
    debugLog app "[handler] executeSingleCell"
    nb <- readNotebook (appNotebook app)
    (env, mods) <- planInputs app nb
    let allCode = haskellCodeCells nb
    case (env, mods) of
        (EnvFresh, ModulesLoaded) ->
            executeSingleCellPlan app gen cid allCode (computeFullExecutionPlan allCode nb)
        _ ->
            executePlan app gen nb (computeStaleExecutionPlanIn env mods allCode nb)
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

executeRunAll :: App -> Int -> IO ()
executeRunAll app gen = do
    nb <- readNotebook (appNotebook app)
    (env, mods) <- planInputs app nb
    let allCode = haskellCodeCells nb
        plan = computeStaleExecutionPlanIn env mods allCode nb
    debugLog app $
        T.pack $
            "[handler] executeRunAll: env="
                ++ show env
                ++ " cells="
                ++ show (map cellId (epCellsToRun plan))
    executePlan app gen nb plan
    whenCurrentGen app gen $ executeNonHaskellFor plan app gen

{- | A rebuilt kernel invalidates the Python side too, so a plan that ran the
environment re-runs every non-Haskell cell rather than only the stale ones.
-}
executeNonHaskellFor :: ExecutionPlan -> App -> Int -> IO ()
executeNonHaskellFor plan
    | epRunEnv plan = executeNonHaskellCells
    | otherwise = executeStaleNonHaskell

executeStaleNonHaskell :: App -> Int -> IO ()
executeStaleNonHaskell app gen = do
    whenCurrentGen app gen $ do
        oldBridge <- getBridgeValues (appBridge app)
        executeStalePythonCells app gen
        rerunBridgeConsumers app gen oldBridge
    whenCurrentGen app gen $ broadcast app EvExecutionDone

{- | Rebuild and run everything: the same plan any caller gets when the
environment is stale, so there is no second execution path to keep in step.
-}
executeFullRestart :: App -> Int -> IO ()
executeFullRestart app gen = do
    debugLog app "[handler] executeFullRestart: rebuilding, running all"
    whenCurrentGen app gen $ do
        nb <- readNotebook (appNotebook app)
        let plan = computeStaleExecutionPlanIn EnvStale ModulesWiped (haskellCodeCells nb) nb
        executePlan app gen nb plan
        whenCurrentGen app gen $ executeNonHaskellCells app gen

{- | Rebuild the environment and run nothing. The caller has already invalidated
every code cell, so the notebook reports what it is: a fresh kernel holding none
of it. Restarting because a cell hangs must not re-run that cell.
-}
executeRestartOnly :: App -> Int -> IO ()
executeRestartOnly app gen = do
    debugLog app "[handler] executeRestartOnly: rebuilding, running nothing"
    whenCurrentGen app gen $ do
        nb <- readNotebook (appNotebook app)
        _ <- runEnvNode app gen nb
        broadcast app EvExecutionDone

executeNonHaskellCells :: App -> Int -> IO ()
executeNonHaskellCells app gen = do
    debugLog app "[handler] executeNonHaskellCells: starting"
    whenCurrentGen app gen $ do
        debugLog app "[handler] executeNonHaskellCells: running Python cells"
        oldBridge <- getBridgeValues (appBridge app)
        executePythonCells app gen
        rerunBridgeConsumers app gen oldBridge
    whenCurrentGen app gen $ broadcast app EvExecutionDone

{- | An edit. The notebook is read once and the environment resolved against
that same read, so the decision and the plan cannot disagree — they used to be
two unsynchronised reads straddling a generation bump.
-}
executeAffected :: App -> Int -> Int -> IO ()
executeAffected app gen editedCid = do
    debugLog app $
        "[handler] executeAffected: editedCid=" <> T.pack (show editedCid)
    nb <- readNotebook (appNotebook app)
    (env, mods) <- planInputs app nb
    let allCode = haskellCodeCells nb
        plan = computeExecutionPlanIn env mods editedCid allCode nb
    logExecutionPlan app allCode plan
    executePlan app gen nb plan
    whenCurrentGen app gen $ broadcast app EvExecutionDone

{- | The compile phase runs even with no compile cells in the plan: a kernel
still holding orphaned modules needs the reconciling unload, or ModulesWiped
reports a pending wipe forever and every edit re-runs the whole notebook.
-}
runPlanPhases :: App -> Int -> ExecutionPlan -> IO ()
runPlanPhases app gen plan = do
    broadcastPlanErrors app plan Nothing
    outcome <-
        if epRunsAnything plan
            then runCompilePhase app gen (epCompilePlan plan) (epCompileCells plan)
            else pure CompileNoChange
    runPostCompile app gen plan outcome (epCellsToRun plan)

epRunsAnything :: ExecutionPlan -> Bool
epRunsAnything plan =
    not (null (epCellsToRun plan)) || not (null (epCompileCells plan))
