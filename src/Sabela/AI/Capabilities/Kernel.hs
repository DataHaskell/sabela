{-# LANGUAGE OverloadedStrings #-}

{- | Kernel-control tools that keep an agent from getting stuck behind a
busy or wedged kernel: a lock-free @kernel_status@, an @interrupt@, an
async @kernel_restart@, and a one-call @export_notebook@ (so syncing the
whole notebook is a single request, not N @read_cell@ round-trips).
-}
module Sabela.AI.Capabilities.Kernel (
    execKernelStatus,
    execInterrupt,
    execKernelRestart,
    execExportNotebook,
    haskellKernelBusy,
) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Aeson (Value, object, (.=))
import Data.Maybe (isJust)
import Data.Text (Text)

import Sabela.AI.Types (ToolOutcome, okOutcome)
import Sabela.Handlers (ReactiveNotebook (..))
import Sabela.Model
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
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
    pure $
        okOutcome $
            object
                [ "kernel" .= (if isJust mSess then ("alive" :: Text) else "absent")
                , "running" .= busy
                , "sessionGen" .= gen
                ]

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
