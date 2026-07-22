{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The single-cell execution primitive the repair cascade drives: dispatch a
forced run, await its @EvCellResult@ broadcast, and classify aborts. Split from
"Sabela.AI.Capabilities.Edit.Run" to keep both under the module-size cap.
-}
module Sabela.AI.Capabilities.Edit.Exec (
    executeCell,
    abortCancelled,
    abortSuperseded,
    abortTimedOut,
) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, readTChan)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import System.Timeout (timeout)

import Sabela.AI.Types
import Sabela.Anthropic.Types (CancelToken, isCancelled)
import Sabela.Handlers (ReactiveNotebook (..))
import Sabela.Model
import qualified Sabela.SessionTypes as ST
import Sabela.State

executeCell ::
    App ->
    ReactiveNotebook ->
    Int ->
    CancelToken ->
    IO (Either Text ExecutionResult)
executeCell app rn cid cancelTok = do
    reqTime <- getCurrentTime
    resultVar <- newEmptyMVar
    listenerThread <- forkIO $ do
        chan <- subscribeBroadcast (appEvents app)
        let loop = do
                ev <- atomically $ readTChan chan
                case ev of
                    EvCellResult rid outputs err errs warns
                        | rid == cid ->
                            putMVar resultVar (ExecutionResult outputs err errs warns)
                    _ -> loop
        loop
    rnRunCellForced rn cid
    mResult <- timeout 130000000 (takeMVar resultVar)
    killThread listenerThread
    cancelled <- isCancelled cancelTok
    stale <- requestStale app reqTime
    if
        | cancelled -> pure (Left abortCancelled)
        | stale -> pure (Left abortSuperseded)
        | otherwise -> case mResult of
            Nothing -> pure (Left abortTimedOut)
            Just r -> pure (Right r)

{- | The three @executeCell@ @Left@ strings, named so 'Sabela.AI.CellResult'
and its tests map the real producer output, not a re-typed literal.
-}
abortCancelled, abortSuperseded, abortTimedOut :: Text
abortCancelled = "Cancelled"
abortSuperseded = "Request superseded by a kernel interrupt"
abortTimedOut = "Cell execution timed out (>120s)"

-- | Did the Haskell kernel interrupt after this request was stamped?
requestStale :: App -> UTCTime -> IO Bool
requestStale app reqTime =
    getHaskellSession (appSessions app)
        >>= maybe (pure False) (`ST.sbRequestStale` reqTime)
