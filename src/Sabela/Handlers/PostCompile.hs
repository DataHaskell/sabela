{-# LANGUAGE OverloadedStrings #-}

{- | What runs after the compile phase. Any actual @:load@ — successful or
not — reset GHCi's interactive context and wiped every prompt binding, so
the interpreted run set escalates from the plan's own cells to all
interpreted cells; a failed build additionally blocks the interpreted
cells that transitively depend on a compiled cell.
-}
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

{- | Dispatch on the compile phase's outcome. @planned@ is the plan's own
interpreted run set, used when no reload invalidated the wider session.
-}
runPostCompile ::
    App -> Int -> ExecutionPlan -> CompileOutcome -> [Cell] -> IO ()
runPostCompile app gen plan outcome planned =
    case outcome of
        CompileNoChange -> runCellList app gen planned
        CompileNoSession -> runBlockedPartition app gen plan planned
        CompileReloaded -> escalateAfterWipe app >>= runCellList app gen
        CompileFailed -> escalateAfterWipe app >>= runBlockedPartition app gen plan

{- | A reload wiped the prompt context: mark every interpreted cell dirty
first (so an interrupted recovery stays truthfully stale and the next
run-all picks it up), then return the full interpreted run set. With no
live session (compile-phase crash) there is nothing to run against.
-}
escalateAfterWipe :: App -> IO [Cell]
escalateAfterWipe app = do
    modifyNotebook (appNotebook app) markAllInterpretedDirty
    mSess <- getHaskellSession (appSessions app)
    case mSess of
        Nothing -> pure []
        Just _ -> escalatedCellsToRun <$> readNotebook (appNotebook app)

{- | Run @cells@, but error out the interpreted cells that transitively
depend on a compiled cell — their modules failed to build.
-}
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
