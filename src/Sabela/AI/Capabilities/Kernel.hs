{-# LANGUAGE OverloadedStrings #-}

{- | Kernel-control tools that keep an agent from getting stuck behind a
busy or wedged kernel: a lock-free @kernel_status@, an @interrupt@, an
async @kernel_restart@, a bounded @await_idle@ long-poll, and a one-call
@export_notebook@ (so syncing the whole notebook is a single request, not
N @read_cell@ round-trips).
-}
module Sabela.AI.Capabilities.Kernel (
    execKernelStatus,
    execInterrupt,
    execKernelRestart,
    execAwaitIdle,
    execExportNotebook,
    haskellKernelBusy,
    kernelStateBefore,
    awaitIdleBudgetUs,
) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Aeson (Value, object, (.=))
import Data.IORef (readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)

import Sabela.AI.KernelState (KernelState, kernelStateJSON, kernelStateOf)
import Sabela.AI.Types (ToolOutcome, okOutcome, toolOutcomeValue)
import Sabela.Handlers (ReactiveNotebook (..))
import Sabela.Model
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.EventBus (
    AwaitResult (..),
    EventBus (..),
    awaitExecutionDone,
 )
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession)

-- | Lock-free: is the Haskell kernel currently running a cell or query?
haskellKernelBusy :: App -> IO Bool
haskellKernelBusy app =
    getHaskellSession (appSessions app) >>= maybe (pure False) ST.sbBusy

{- | Lock-free kernel status. Always answers — even while a cell holds the
run-lock — so a driver can tell "busy" (a slow cell) from "wedged" (the
server itself is unresponsive) without taking the lock.
-}
execKernelStatus :: App -> IO ToolOutcome
execKernelStatus app = do
    mSess <- getHaskellSession (appSessions app)
    busy <- maybe (pure False) ST.sbBusy mSess
    gen <- maybe (pure 0) ST.sbSessionGen mSess
    compiling <- readIORef (appBuilding app)
    ebGen <- readIORef (ebGeneration (appEvents app))
    let kstate = kernelStateOf (isJust mSess) gen busy compiling
    pure $
        okOutcome $
            object
                [ "state" .= kernelStateJSON kstate
                , "ksGen" .= gen
                , "ebGeneration" .= ebGen
                ]

{- | The typed 'KernelState' and the @ebGeneration@ fence from the same
lock-free reads 'execKernelStatus' uses, captured BEFORE a dispatch so the
status tool and the provenance log agree on the kernel-before snapshot.
-}
kernelStateBefore :: App -> IO (KernelState, Int)
kernelStateBefore app = do
    mSess <- getHaskellSession (appSessions app)
    busy <- maybe (pure False) ST.sbBusy mSess
    gen <- maybe (pure 0) ST.sbSessionGen mSess
    compiling <- readIORef (appBuilding app)
    ebGen <- readIORef (ebGeneration (appEvents app))
    pure (kernelStateOf (isJust mSess) gen busy compiling, ebGen)

-- | Abort the running cell (group SIGINT); no-op when idle.
execInterrupt :: App -> IO ToolOutcome
execInterrupt app = do
    mSess <- getHaskellSession (appSessions app)
    maybe (pure ()) ST.sbInterrupt mSess
    pure $ okOutcome $ object ["interrupted" .= True]

{- | Restart the kernel without a human. Forked so the response returns
immediately; the caller then polls 'execKernelStatus' until it is ready.
-}
execKernelRestart :: ReactiveNotebook -> IO ToolOutcome
execKernelRestart rn = do
    void (forkIO (rnRestartKernel rn))
    pure $ okOutcome $ object ["restartInitiated" .= True]

-- | Server-side bound on a single 'execAwaitIdle' long-poll (~45s).
awaitIdleBudgetUs :: Int
awaitIdleBudgetUs = 45000000

{- | Bounded lock-free long-poll that settles on @EvExecutionDone@ for the
cascade in flight (not a @running == false@ sample — the cascade releases
the run-lock between cells). When the kernel is already idle at entry there
is no fence to wait for, so it returns immediately. Its own kill-aware
timeout returns a terminal state on kernel death so the poll cannot itself
wedge; the caller re-loops on a non-@settled@ outcome.
-}
execAwaitIdle :: App -> IO ToolOutcome
execAwaitIdle app = do
    busy <- haskellKernelBusy app
    building <- readIORef (appBuilding app)
    if not (busy || building)
        then awaitResult "idle" =<< awaitIdleState app
        else do
            res <-
                awaitExecutionDone
                    (appEvents app)
                    awaitIdleBudgetUs
                    (haskellKernelAlive app)
            awaitResult (awaitTag res) =<< awaitIdleState app

-- | Lock-free: is a Haskell session attached (kernel not torn down)?
haskellKernelAlive :: App -> IO Bool
haskellKernelAlive app = isJust <$> getHaskellSession (appSessions app)

awaitTag :: AwaitResult -> Text
awaitTag AwaitSettled = "settled"
awaitTag AwaitKernelDead = "kernelDead"
awaitTag AwaitTimedOut = "timedOut"

{- | The @await_idle@ result: the long-poll @waited@ tag plus a fresh
kernel-status snapshot, so the caller sees the kernel's terminal state in
the same reply and re-loops only when @waited@ is not @settled@/@idle@.
-}
awaitResult :: Text -> Value -> IO ToolOutcome
awaitResult tag status =
    pure $ okOutcome $ object ["waited" .= tag, "status" .= status]

-- | Snapshot of the live kernel status, shared by 'execAwaitIdle'.
awaitIdleState :: App -> IO Value
awaitIdleState app = toolOutcomeValue <$> execKernelStatus app

{- | Return every cell's source in one call, so a full notebook sync is a
single request rather than N @read_cell@ calls (which flood the bridge).
-}
execExportNotebook :: App -> Value -> IO ToolOutcome
execExportNotebook app _input = do
    nb <- readNotebook (appNotebook app)
    let cells = zipWith cellJson [1 :: Int ..] (nbCells nb)
    pure $ okOutcome $ object ["title" .= nbTitle nb, "cells" .= cells]
  where
    cellJson pos c =
        object
            [ "id" .= cellId c
            , "position" .= pos
            , "type" .= cellType c
            , "lang" .= cellLang c
            , "source" .= cellSource c
            ]
