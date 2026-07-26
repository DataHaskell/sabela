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
    escalatedCellsToRun,
    haskellCodeCells,
    markAllInterpretedDirty,
 )
import Sabela.State (App (..))
import Sabela.State.NotebookStore (modifyNotebook, readNotebook)
import Sabela.State.SessionManager (getHaskellSession)

runPostCompile ::
    App -> Int -> ExecutionPlan -> CompileOutcome -> [Cell] -> IO ()
runPostCompile app gen plan outcome planned =
    case outcome of
        CompileNoChange -> runCellList app gen planned
        CompileNoSession -> runBlockedPartition app gen plan planned
        CompileReloaded -> escalateAfterWipe app >>= runCellList app gen
        CompileFailed -> escalateAfterWipe app >>= runBlockedPartition app gen plan

escalateAfterWipe :: App -> IO [Cell]
escalateAfterWipe app = do
    modifyNotebook (appNotebook app) markAllInterpretedDirty
    mSess <- getHaskellSession (appSessions app)
    case mSess of
        Nothing -> pure []
        Just _ -> escalatedCellsToRun <$> readNotebook (appNotebook app)

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
