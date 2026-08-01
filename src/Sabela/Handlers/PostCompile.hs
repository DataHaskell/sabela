{-# LANGUAGE OverloadedStrings #-}

module Sabela.Handlers.PostCompile (
    runPostCompile,
    runCellList,
) where

import Control.Monad (forM_)
import qualified Data.Set as S

import Sabela.Handlers.Compile (CompileOutcome (..), compiledDependents)
import Sabela.Handlers.Exec (runAndBroadcast)
import Sabela.Handlers.Shared
import Sabela.Model (Cell (..))
import Sabela.Reactivity (
    ExecutionPlan (..),
    haskellCodeCells,
 )
import Sabela.State (App (..))
import Sabela.State.NotebookStore (readNotebook)

runPostCompile ::
    App -> Int -> ExecutionPlan -> CompileOutcome -> [Cell] -> IO ()
runPostCompile app gen plan outcome planned =
    case outcome of
        CompileNoChange -> runCellList app gen planned
        CompileNoSession -> runBlockedPartition app gen plan planned
        CompileReloaded -> runCellList app gen planned
        CompileFailed -> runBlockedPartition app gen plan planned

runBlockedPartition :: App -> Int -> ExecutionPlan -> [Cell] -> IO ()
runBlockedPartition app gen plan cells = do
    nb <- readNotebook (appNotebook app)
    let blocked =
            compiledDependents
                (epCompilePlan plan)
                (epDefMap plan)
                (haskellCodeCells nb)
    forM_ cells $ \c ->
        whenCurrentGen app gen $
            if S.member (cellId c) blocked
                then
                    broadcastCellError
                        app
                        (cellId c)
                        "did not run: a compiled cell failed to build (fix it and re-run)"
                else runAndBroadcast app gen c

runCellList :: App -> Int -> [Cell] -> IO ()
runCellList app gen cells =
    forM_ cells $ \cell ->
        whenCurrentGen app gen $ runAndBroadcast app gen cell
