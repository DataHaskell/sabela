{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Synchronous cell-execution helpers split out of "Sabela.AI.Capabilities.Edit".

These are the pieces every mutating tool (@replace_cell_source@,
@insert_cell@) calls so the tool response carries the freshly-computed
execution summary, plus the @execute_cell@ tool itself. Kept as a
separate module because the listener-and-timeout pattern in 'executeCell'
is also a natural reuse point for any future REST blocking-run endpoint
(Kmett architectural follow-up #4).
-}
module Sabela.AI.Capabilities.Edit.Run (
    autoExecuteAfterMutation,
    execExecuteCell,
    executeCell,
    abortCancelled,
    abortSuperseded,
    abortTimedOut,
) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, readTChan)
import Data.Aeson (Value, toJSON, (.=))
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import System.Timeout (timeout)

import Sabela.AI.Capabilities.Util (fieldInt)
import Sabela.AI.CellResult (mergeToolOk, toCellResult)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Anthropic.Types (CancelToken, isCancelled)
import Sabela.Api (errorJson)
import Sabela.Handlers (ReactiveNotebook (..))
import Sabela.Model
import qualified Sabela.SessionTypes as ST
import Sabela.State

{- | Run a single cell via the reactive notebook and return the typed
'CellResult' JSON for embedding as the mutation-tool @execution@ summary.
The outcome sum (Succeeded/Raised/Rejected/Aborted) and the @ok@ boolean
ride on the same value the @execute_cell@ tool emits.

@_store@ is the (currently unused) carrier for the staged Output chokepoint:
per redesign §1.3 cell outputs route through @inlineOrStash@ behind the MV
slice, so @crOutputs@ inline raw here for now. The in-browser chat is still
bounded by 'Sabela.AI.Orchestrator.Compact'; the REST bridge ('aiToolH') is
deliberately un-stashed on this path until that slice lands.
-}
autoExecuteAfterMutation ::
    App -> AIStore -> ReactiveNotebook -> CancelToken -> Int -> IO Value
autoExecuteAfterMutation app _store rn cancelTok cid = do
    res <- executeCell app rn cid cancelTok
    pure (toJSON (toCellResult res (resultOutputs res)))

{- | @execute_cell@. @_store@ is the staged Output-chokepoint carrier — see
'autoExecuteAfterMutation'; @crOutputs@ inline raw on this path for now.
-}
execExecuteCell ::
    App -> AIStore -> ReactiveNotebook -> CancelToken -> Value -> IO ToolOutcome
execExecuteCell app _store rn cancelTok input = do
    let mcid = fieldInt "cell_id" input
    case mcid of
        Nothing -> pure (errOutcome (errorJson "cell_id required"))
        Just cid -> do
            result <- executeCell app rn cid cancelTok
            let cr = toCellResult result (resultOutputs result)
            pure (mergeToolOk cr ["cellId" .= cid])

-- | Outputs an @executeCell@ result carried; @[]@ for an abort 'Left'.
resultOutputs :: Either Text ExecutionResult -> [OutputItem]
resultOutputs (Left _) = []
resultOutputs (Right er) = erOutputs er

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
                    EvCellResult rid outputs err errs
                        | rid == cid ->
                            putMVar resultVar (ExecutionResult outputs err errs)
                    _ -> loop
        loop
    rnRunCell rn cid
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
