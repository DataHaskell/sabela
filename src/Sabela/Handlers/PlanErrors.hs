{-# LANGUAGE OverloadedStrings #-}

{- | Broadcasting plan-level errors (cycles, redefinitions, compile-mode
violations) to their cells, optionally filtered to a single cell for the
run-one-cell path. Split out of "Sabela.Handlers.Plan" for the module-size
cap.
-}
module Sabela.Handlers.PlanErrors (
    broadcastPlanErrors,
) where

import Control.Monad (forM_, unless)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

import Sabela.Compiled (CompilePlan (..))
import Sabela.Handlers.Shared
import Sabela.Model (CellError (..), Notebook (..))
import Sabela.Reactivity (
    ExecutionPlan (..),
    cycleErrorMsg,
    redefinitionErrorMsg,
 )
import Sabela.State (App (..))
import Sabela.State.NotebookStore (readNotebook)

broadcastPlanErrors :: App -> ExecutionPlan -> Maybe Int -> IO ()
broadcastPlanErrors app plan filterCid = do
    broadcastRedefErrors app plan filterCid
    broadcastCycleErrors app plan filterCid
    broadcastCompileViolations app plan filterCid

broadcastCompileViolations :: App -> ExecutionPlan -> Maybe Int -> IO ()
broadcastCompileViolations app plan filterCid = do
    let vmap = filterByCell filterCid (cpViolations (epCompilePlan plan))
    forM_ (M.toList vmap) $ \(cid, errs) ->
        broadcastCellErrorWith
            app
            cid
            (T.intercalate "\n\n" (map ceMessage errs))
            errs

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
