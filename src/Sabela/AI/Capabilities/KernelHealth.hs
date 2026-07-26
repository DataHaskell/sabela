{-# LANGUAGE OverloadedStrings #-}

{- | Kernel-health bookkeeping shared by the ack and await surfaces: the
post-settled consistency window's settle fence (R6.4), the busy evidence the
admission window consults, and the @resource@ runaway diagnostic's evidence
assembly (R6.5). Heap samples are attached when a backend can report them;
the current wiring supplies wall-clock and progress evidence.
-}
module Sabela.AI.Capabilities.KernelHealth (
    noteSettled,
    busyEvidenceNow,
    cellOwner,
    runningHolder,
    resourceField,
    awaitIdleBudgetUsOf,
) where

import Data.Aeson ((.=))
import Data.Aeson.Types (Pair)
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Sabela.AI.KernelVocab (
    BusyEvidence (..),
    Holding (..),
    LockOwner (..),
    ownerLabel,
 )
import Sabela.AI.Resource (
    ResourceEvidence (..),
    resourceLine,
    resourceWallBudgetMs,
 )
import Sabela.AI.Store (AIStore (..))
import Sabela.AI.WriteRegistry (
    PendingWrite (..),
    elapsedMsOf,
    firstRunningWrite,
 )
import Sabela.Model (Cell (..), Notebook (..))
import Sabela.State (App (..))
import Sabela.State.EventBus (EventBus (..))
import Sabela.State.NotebookStore (readNotebook)

{- | Record the settle fence: the current eventboard generation was observed
settled\/idle, opening the consistency window and clearing the runaway clock.
-}
noteSettled :: App -> AIStore -> IO ()
noteSettled app store = do
    gen <- readIORef (ebGeneration (appEvents app))
    writeIORef (aiSettledGen store) (Just gen)
    writeIORef (aiBusySince store) Nothing

{- | One lock-free busy observation for the admission window: the occupancy
sample, the settle fence, the current generation, and the locking holder
(from the write registry) when one is known.
-}
busyEvidenceNow :: App -> AIStore -> IO Bool -> IO BusyEvidence
busyEvidenceNow app store occupied = do
    occ <- occupied
    sg <- readIORef (aiSettledGen store)
    cur <- readIORef (ebGeneration (appEvents app))
    h <- runningHolder app store
    pure (BusyEvidence occ sg cur h)

{- | The lock's holder, when a registered write is running. A cell id is only
claimed when the notebook actually contains that cell; a write still in
flight before its cell commits is named as the operation it is, so no busy
verdict can cite a phantom cell (G8, @phantom-cell-zero@).
-}
runningHolder :: App -> AIStore -> IO (Maybe Holding)
runningHolder app store = do
    mPw <- firstRunningWrite (aiWriteReg store)
    case mPw of
        Nothing -> pure Nothing
        Just pw -> do
            ms <- elapsedMsOf pw
            owner <- cellOwner app (pwCellId pw)
            pure (Just (Holding owner ms))

{- | A raw cell id as a lock owner: a cell only when the notebook actually
contains it. The one chokepoint both busy paths share, so no verdict can
cite the phantom @cellId: 0@ of live_test10.
-}
cellOwner :: App -> Int -> IO LockOwner
cellOwner app cid = do
    nb <- readNotebook (appNotebook app)
    pure $
        if any ((== cid) . cellId) (nbCells nb)
            then OwnedByCell cid
            else OwnedByOp uncommittedWrite

{- | The holder label for a registered write whose cell is not in the
notebook: the live_test10 state, where an install held the lock for a cell
that never committed and every verdict cited @cellId: 0@.
-}
uncommittedWrite :: Text
uncommittedWrite = "a pending write (installing dependencies or compiling)"

{- | The @resource@ pair for a timed-out await (R6.5): assemble the evidence
(wall clock from the running write or the first busy observation, progress
from the poll's observed events) and emit the ONE bounded line when the
runaway trigger fires; @[]@ otherwise.
-}
resourceField :: App -> AIStore -> Int -> IO [Pair]
resourceField app store eventsSeen = do
    now <- getMonotonicTimeNSec
    since <- readIORef (aiBusySince store)
    case since of
        Nothing -> writeIORef (aiBusySince store) (Just now)
        Just _ -> pure ()
    mHolder <- runningHolder app store
    let sinceMs =
            maybe 0 (\t0 -> fromIntegral ((now - t0) `div` 1000000)) since
        elapsed = max sinceMs (maybe 0 hdElapsedMs mHolder)
        evidence = ResourceEvidence elapsed [] eventsSeen
    budget <- resourceWallBudgetMs
    pure
        [ "resource" .= line
        | Just line <- [resourceLine budget (ownerLabel . hdOwner <$> mHolder) evidence]
        ]

{- | The @await_idle@ long-poll budget (~45s); @SABELA_AWAIT_IDLE_SECS@
overrides so tests and short-budget drivers can shrink the window.
-}
awaitIdleBudgetUsOf :: Int -> IO Int
awaitIdleBudgetUsOf dfltUs = do
    m <- lookupEnv "SABELA_AWAIT_IDLE_SECS"
    pure (maybe dfltUs (* 1000000) (readMaybe =<< m))
