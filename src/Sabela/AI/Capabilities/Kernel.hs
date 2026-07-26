{-# LANGUAGE OverloadedStrings #-}

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

haskellKernelBusy :: App -> IO Bool
haskellKernelBusy app =
    getHaskellSession (appSessions app) >>= maybe (pure False) ST.sbBusy

haskellKernelOccupied :: App -> IO Bool
haskellKernelOccupied app = do
    busy <- haskellKernelBusy app
    building <- readIORef (appBuilding app)
    pure (busy || building)

execKernelStatus :: App -> IO ToolOutcome
execKernelStatus app = do
    mSess <- getHaskellSession (appSessions app)
    busy <- maybe (pure False) ST.sbBusy mSess
    gen <- maybe (pure 0) ST.sbSessionGen mSess
    ebGen <- readIORef (ebGeneration (appEvents app))
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

kernelStateBefore :: App -> IO (KernelState, Int)
kernelStateBefore app = do
    mSess <- getHaskellSession (appSessions app)
    busy <- maybe (pure False) ST.sbBusy mSess
    gen <- maybe (pure 0) ST.sbSessionGen mSess
    compiling <- readIORef (appBuilding app)
    ebGen <- readIORef (ebGeneration (appEvents app))
    pure (kernelStateOf (isJust mSess) gen busy compiling, ebGen)

execInterrupt :: App -> AIStore -> IO ToolOutcome
execInterrupt app store = do
    mSess <- getHaskellSession (appSessions app)
    maybe (pure ()) ST.sbInterrupt mSess
    interruptOutcome <$> awaitRelease app store controlGraceRounds

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

controlGraceRounds :: Int
controlGraceRounds = 20

controlGraceDelayUs :: Int
controlGraceDelayUs = 100000

execKernelRestart :: App -> ReactiveNotebook -> IO ToolOutcome
execKernelRestart app rn = do
    void (forkIO (rnRestartKernel rn))
    restartOutcome <$> awaitKernelBack app restartGraceRounds

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

awaitIdleBudgetUs :: Int
awaitIdleBudgetUs = 45000000

execAwaitIdle :: App -> AIStore -> IO ToolOutcome
execAwaitIdle app store = do
    occupied <- haskellKernelOccupied app
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

haskellKernelAlive :: App -> IO Bool
haskellKernelAlive app = isJust <$> getHaskellSession (appSessions app)

awaitTag :: AwaitResult -> Text
awaitTag AwaitSettled = tagSettled
awaitTag AwaitKernelDead = tagKernelDead
awaitTag AwaitTimedOut = tagTimedOut

awaitResult :: Text -> Value -> [Pair] -> IO ToolOutcome
awaitResult tag status writes =
    pure $ okOutcome $ object (["waited" .= tag, "status" .= status] <> writes)

awaitIdleState :: App -> IO Value
awaitIdleState app = toolOutcomeValue <$> execKernelStatus app

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
