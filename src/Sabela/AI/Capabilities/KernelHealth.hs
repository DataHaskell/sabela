{-# LANGUAGE OverloadedStrings #-}

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

noteSettled :: App -> AIStore -> IO ()
noteSettled app store = do
    gen <- readIORef (ebGeneration (appEvents app))
    writeIORef (aiSettledGen store) (Just gen)
    writeIORef (aiBusySince store) Nothing

busyEvidenceNow :: App -> AIStore -> IO Bool -> IO BusyEvidence
busyEvidenceNow app store occupied = do
    occ <- occupied
    sg <- readIORef (aiSettledGen store)
    cur <- readIORef (ebGeneration (appEvents app))
    h <- runningHolder app store
    pure (BusyEvidence occ sg cur h)

runningHolder :: App -> AIStore -> IO (Maybe Holding)
runningHolder app store = do
    mPw <- firstRunningWrite (aiWriteReg store)
    case mPw of
        Nothing -> pure Nothing
        Just pw -> do
            ms <- elapsedMsOf pw
            owner <- cellOwner app (pwCellId pw)
            pure (Just (Holding owner ms))

cellOwner :: App -> Int -> IO LockOwner
cellOwner app cid = do
    nb <- readNotebook (appNotebook app)
    pure $
        if any ((== cid) . cellId) (nbCells nb)
            then OwnedByCell cid
            else OwnedByOp uncommittedWrite

uncommittedWrite :: Text
uncommittedWrite = "a pending write (installing dependencies or compiling)"

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

awaitIdleBudgetUsOf :: Int -> IO Int
awaitIdleBudgetUsOf dfltUs = do
    m <- lookupEnv "SABELA_AWAIT_IDLE_SECS"
    pure (maybe dfltUs (* 1000000) (readMaybe =<< m))
