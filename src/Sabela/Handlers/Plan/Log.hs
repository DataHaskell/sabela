{-# LANGUAGE OverloadedStrings #-}

-- | Debug rendering of a computed plan: what will run, what is skipped, and
-- the dependency names behind each decision.
module Sabela.Handlers.Plan.Log (
    logExecutionPlan,
) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

import Sabela.Handlers.Shared (debugLog)
import Sabela.Model (Cell (..))
import Sabela.Reactivity (ExecutionPlan (..))
import Sabela.State (App)
import qualified Sabela.Topo as Topo

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
