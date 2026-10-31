{-# LANGUAGE OverloadedStrings #-}

module Sabela.Handlers.PostCompile (
    runPostCompile,
    runCellList,
) where

import Control.Monad (foldM_)
import qualified Data.Set as S
import Data.Time (UTCTime, getCurrentTime)

import Sabela.Handlers.Compile (CompileOutcome (..), compiledDependents)
import Sabela.Handlers.Exec (runAndBroadcast)
import Sabela.Handlers.Shared
import Sabela.Model (Cell (..))
import Sabela.Reactivity (
    ExecutionPlan (..),
    haskellCodeCells,
 )
import Sabela.Reactivity.Upstream (
    failedUpstream,
    settledFailures,
    upstreamFailureMsg,
 )
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession)

runPostCompile ::
    App -> Int -> ExecutionPlan -> CompileOutcome -> [Cell] -> IO ()
runPostCompile app gen plan outcome planned =
    case outcome of
        CompileNoChange -> runCellList app gen plan planned
        CompileNoSession -> runBlockedPartition app gen plan planned
        CompileReloaded -> runCellList app gen plan planned
        CompileFailed -> runBlockedPartition app gen plan planned

runBlockedPartition :: App -> Int -> ExecutionPlan -> [Cell] -> IO ()
runBlockedPartition app gen plan cells = do
    nb <- readNotebook (appNotebook app)
    let blocked =
            compiledDependents
                (epCompilePlan plan)
                (epDefMap plan)
                (haskellCodeCells nb)
    runCellsSuspending app gen plan blocked cells

runCellList :: App -> Int -> ExecutionPlan -> [Cell] -> IO ()
runCellList app gen plan = runCellsSuspending app gen plan S.empty

{- | Runs cells in plan order, suspending each one whose upstream failed —
earlier in this run or settled from before — instead of computing from stale
bindings. @compileBlocked@ cells wait on a compiled module that did not build.
-}
runCellsSuspending ::
    App -> Int -> ExecutionPlan -> S.Set Int -> [Cell] -> IO ()
runCellsSuspending app gen plan compileBlocked cells = do
    nb <- readNotebook (appNotebook app)
    started <- getCurrentTime
    foldM_ (step started) (settledFailures (haskellCodeCells nb) cells) cells
  where
    step started failed c = do
        current <- isCurrentGen app gen
        stopped <- interruptedSince app started
        if not current || stopped
            then pure failed
            else runOne failed c
    runOne failed c
        | S.member (cellId c) compileBlocked = do
            broadcastCellError
                app
                (cellId c)
                "did not run: a compiled cell failed to build (fix it and re-run)"
            pure (S.insert (cellId c) failed)
        | Just up <- failedUpstream (epDeps plan) failed (cellId c) = do
            broadcastCellError app (cellId c) (upstreamFailureMsg (epCellPositions plan) up)
            pure (S.insert (cellId c) failed)
        | otherwise = do
            ok <- runAndBroadcast app gen c
            pure (if ok then failed else S.insert (cellId c) failed)

{- | Has the kernel been interrupted since @t@? A Stop ends the whole plan, not
just the cell it caught: the remaining cells stay stale instead of running.
-}
interruptedSince :: App -> UTCTime -> IO Bool
interruptedSince app t =
    getHaskellSession (appSessions app)
        >>= maybe (pure False) (`ST.sbRequestStale` t)
