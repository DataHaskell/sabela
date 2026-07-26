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
    interruptOutcome,
    restartOutcome,
    execAwaitIdle,
    execExportNotebook,
    haskellKernelBusy,
    haskellKernelOccupied,
    kernelStateBefore,
    awaitIdleBudgetUs,
    awaitTag,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void, when)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.IORef (readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.Clock (getMonotonicTimeNSec)

import Sabela.AI.Capabilities.Edit.Ack (settledWritesField)
import Sabela.AI.Capabilities.KernelHealth (
    awaitIdleBudgetUsOf,
    noteSettled,
    resourceField,
    runningHolder,
 )
import Sabela.AI.KernelState (KernelState, kernelStateJSON, kernelStateOf)
import Sabela.AI.KernelVocab (
    Holding (..),
    LockOwner (..),
    ownerLabel,
    tagIdle,
    tagKernelDead,
    tagSettled,
    tagTimedOut,
 )
import Sabela.AI.Store (AIStore)
import Sabela.AI.Types (ToolOutcome, okOutcome, toolOutcomeValue)
import Sabela.Handlers (ReactiveNotebook (..))
import Sabela.Model
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.EventBus (
    AwaitResult (..),
    EventBus (..),
    awaitExecutionDoneCounting,
 )
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession)

-- | Lock-free: is the Haskell kernel currently running a cell or query?
haskellKernelBusy :: App -> IO Bool
haskellKernelBusy app =
    getHaskellSession (appSessions app) >>= maybe (pure False) ST.sbBusy

{- | Lock-free: is the kernel executing a cell OR compiling? The admission bounce
uses this, not 'haskellKernelBusy' — a build raises @appBuilding@ but not
@sbBusy@, so a busy-only check lets retries stack compiles behind the run-lock.
-}
haskellKernelOccupied :: App -> IO Bool
haskellKernelOccupied app = do
    busy <- haskellKernelBusy app
    building <- readIORef (appBuilding app)
    pure (busy || building)

{- | Lock-free kernel status. Always answers — even while a cell holds the
run-lock — so a driver can tell "busy" (a slow cell) from "wedged" (the
server itself is unresponsive) without taking the lock.
-}
execKernelStatus :: App -> IO ToolOutcome
execKernelStatus app = do
    mSess <- getHaskellSession (appSessions app)
    busy <- maybe (pure False) ST.sbBusy mSess
    gen <- maybe (pure 0) ST.sbSessionGen mSess
    ebGen <- readIORef (ebGeneration (appEvents app))
    -- Derive both the building flag and its elapsed ms from ONE read of
    -- appBuildingSince, so status never reports state=building with no buildingMs
    -- (or the reverse) when a build starts/ends between two separate reads.
    mSince <- readIORef (appBuildingSince app)
    now <- getMonotonicTimeNSec
    let compiling = isJust mSince
        buildingMs = (\t0 -> (now - t0) `div` 1000000) <$> mSince
        kstate = kernelStateOf (isJust mSess) gen busy compiling
    pure $
        okOutcome $
            object $
                [ "state" .= kernelStateJSON kstate
                , "ksGen" .= gen
                , "ebGeneration" .= ebGen
                ]
                    ++ ["buildingMs" .= ms | Just ms <- [buildingMs]]

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

{- | Abort the running cell (group SIGINT) and report the OUTCOME, not the
attempt: @interrupted@ is true only when the lock actually released inside
the grace window. An uninterruptible holder — a @cabal install@ subprocess
is the legitimate case — answers false and names what still holds it, so the
model never plans against an interrupt that interrupted nothing (G8).
-}
execInterrupt :: App -> AIStore -> IO ToolOutcome
execInterrupt app store = do
    mSess <- getHaskellSession (appSessions app)
    maybe (pure ()) ST.sbInterrupt mSess
    interruptOutcome <$> awaitRelease app store controlGraceRounds

{- | The interrupt verdict as a function of what still holds the lock, so the
honesty law is checkable without a wedged kernel (@false-interrupt@).
-}
interruptOutcome :: Maybe Holding -> ToolOutcome
interruptOutcome still = okOutcome $ object $ case still of
    Nothing -> ["interrupted" .= True]
    Just (Holding owner ms) ->
        [ "interrupted" .= False
        , "holder" .= ownerLabel owner
        , "elapsedMs" .= ms
        , "detail"
            .= ( "the interrupt did not release the lock; "
                    <> ownerLabel owner
                    <> " still holds it. A dependency install cannot be \
                       \interrupted — wait for it, or kernel_restart."
               )
        ]

{- | Poll until the run lock is free, up to @n@ grace rounds; 'Nothing' when
it released, else the holder that outlasted the window.
-}
awaitRelease :: App -> AIStore -> Int -> IO (Maybe Holding)
awaitRelease app store = go
  where
    go n = do
        occupied <- haskellKernelOccupied app
        holder <- runningHolder app store
        if not (occupied || isJust holder)
            then pure Nothing
            else
                if n <= 0
                    then pure (holderOr occupied holder)
                    else threadDelay controlGraceDelayUs >> go (n - 1)
    holderOr True Nothing = Just (Holding (OwnedByOp "a run you did not start") 0)
    holderOr _ h = h

-- | ~2s of grace for a control operation to take effect before it answers.
controlGraceRounds :: Int
controlGraceRounds = 20

controlGraceDelayUs :: Int
controlGraceDelayUs = 100000

{- | Hard-reset the kernel: force-kill the process (bypassing the run-lock a
wedged cell holds) and respawn clean, reusing the env without rebuilding and
without re-running cells. Reports the restart's OUTCOME: a restart that
leaves the kernel cold is a failure verdict, never a bare
@restartInitiated@ the model can mistake for a working kernel (G8).
-}
execKernelRestart :: App -> ReactiveNotebook -> IO ToolOutcome
execKernelRestart app rn = do
    void (forkIO (rnRestartKernel rn))
    restartOutcome <$> awaitKernelBack app restartGraceRounds

{- | The restart verdict as a function of whether the kernel came back. A
restart that leaves it cold is a failure verdict, never a bare
@restartInitiated@ the model can mistake for a working kernel
(@restart-into-death@).
-}
restartOutcome :: Bool -> ToolOutcome
restartOutcome alive =
    okOutcome $
        object $
            ["restartInitiated" .= True, "restarted" .= alive]
                <> ["detail" .= restartFailedDetail | not alive]

restartFailedDetail :: Text
restartFailedDetail =
    "the kernel did not come back within the restart window and is still \
    \cold; this is an infrastructure fault, not something to retry blindly."

-- | Poll until a Haskell session is attached again, up to @n@ rounds (~10s).
awaitKernelBack :: App -> Int -> IO Bool
awaitKernelBack app = go
  where
    go n = do
        alive <- haskellKernelAlive app
        if alive
            then pure True
            else
                if n <= 0
                    then pure False
                    else threadDelay controlGraceDelayUs >> go (n - 1)

restartGraceRounds :: Int
restartGraceRounds = 100

-- | Server-side bound on a single 'execAwaitIdle' long-poll (~45s).
awaitIdleBudgetUs :: Int
awaitIdleBudgetUs = 45000000

{- | Bounded lock-free long-poll that settles on @EvExecutionDone@ for the
cascade in flight (not a @running == false@ sample — the cascade releases
the run-lock between cells). When the kernel is already idle at entry there
is no fence to wait for, so it returns immediately. Its own kill-aware
timeout returns a terminal state on kernel death so the poll cannot itself
wedge; the caller re-loops on a non-@settled@ outcome. Settled-but-
undelivered write acks reconcile here, exactly once (R6.1). A settle opens
the post-settled consistency window (R6.4); a timeout attaches the bounded
@resource@ runaway line when the evidence warrants it (R6.5).
-}
execAwaitIdle :: App -> AIStore -> IO ToolOutcome
execAwaitIdle app store = do
    occupied <- haskellKernelOccupied app
    -- Idle by the SAME evidence the admission bounce uses: a cascade releases
    -- the run-lock between cells, so sbBusy alone reported idle while the next
    -- write still bounced on a registered running write (live_test8).
    holder <- runningHolder app store
    if not (occupied || isJust holder)
        then finishAwait tagIdle []
        else do
            budgetUs <- awaitIdleBudgetUsOf awaitIdleBudgetUs
            (res, seen) <-
                awaitExecutionDoneCounting
                    (appEvents app)
                    budgetUs
                    (haskellKernelAlive app)
            resource <-
                if res == AwaitTimedOut
                    then resourceField app store seen
                    else pure []
            finishAwait (awaitTag res) resource
  where
    finishAwait tag extra = do
        when (tag == tagIdle || tag == tagSettled) (noteSettled app store)
        writes <- settledWritesField store
        status <- awaitIdleState app
        awaitResult tag status (writes <> extra)

-- | Lock-free: is a Haskell session attached (kernel not torn down)?
haskellKernelAlive :: App -> IO Bool
haskellKernelAlive app = isJust <$> getHaskellSession (appSessions app)

-- | Await tags drawn from the closed vocabulary ("Sabela.AI.KernelVocab").
awaitTag :: AwaitResult -> Text
awaitTag AwaitSettled = tagSettled
awaitTag AwaitKernelDead = tagKernelDead
awaitTag AwaitTimedOut = tagTimedOut

{- | The @await_idle@ result: the long-poll @waited@ tag plus a fresh
kernel-status snapshot, so the caller sees the kernel's terminal state in
the same reply and re-loops only when @waited@ is not @settled@/@idle@.
The @writes@ pairs carry any newly settled write-ack reconciliations.
-}
awaitResult :: Text -> Value -> [Pair] -> IO ToolOutcome
awaitResult tag status writes =
    pure $ okOutcome $ object (["waited" .= tag, "status" .= status] <> writes)

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
