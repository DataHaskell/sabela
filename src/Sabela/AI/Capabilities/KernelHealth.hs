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
    resourceField,
    awaitIdleBudgetUsOf,
) where

import Data.Aeson ((.=))
import Data.Aeson.Types (Pair)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Sabela.AI.KernelVocab (BusyEvidence (..))
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
import Sabela.State (App (..))
import Sabela.State.EventBus (EventBus (..))

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
    h <- runningHolder store
    pure (BusyEvidence occ sg cur h)

-- | The running registered write as (cell id, elapsed ms), when there is one.
runningHolder :: AIStore -> IO (Maybe (Int, Int))
runningHolder store = do
    mPw <- firstRunningWrite (aiWriteReg store)
    case mPw of
        Nothing -> pure Nothing
        Just pw -> do
            ms <- elapsedMsOf pw
            pure (Just (pwCellId pw, ms))

{- | The @resource@ pair for a timed-out await (R6.5): assemble the evidence
(wall clock from the running write or the first busy observation, progress
from the poll's observed events) and emit the ONE bounded line when the
runaway trigger fires; @[]@ otherwise.
-}
resourceField :: AIStore -> Int -> IO [Pair]
resourceField store eventsSeen = do
    now <- getMonotonicTimeNSec
    since <- readIORef (aiBusySince store)
    case since of
        Nothing -> writeIORef (aiBusySince store) (Just now)
        Just _ -> pure ()
    mHolder <- runningHolder store
    let sinceMs =
            maybe 0 (\t0 -> fromIntegral ((now - t0) `div` 1000000)) since
        elapsed = max sinceMs (maybe 0 snd mHolder)
        evidence = ResourceEvidence elapsed [] eventsSeen
    budget <- resourceWallBudgetMs
    pure
        [ "resource" .= line
        | Just line <- [resourceLine budget (fst <$> mHolder) evidence]
        ]

{- | The @await_idle@ long-poll budget (~45s); @SABELA_AWAIT_IDLE_SECS@
overrides so tests and short-budget drivers can shrink the window.
-}
awaitIdleBudgetUsOf :: Int -> IO Int
awaitIdleBudgetUsOf dfltUs = do
    m <- lookupEnv "SABELA_AWAIT_IDLE_SECS"
    pure (maybe dfltUs (* 1000000) (readMaybe =<< m))
