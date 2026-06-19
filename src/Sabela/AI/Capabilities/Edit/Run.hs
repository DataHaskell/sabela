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
) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, readTChan)
import Data.Aeson (Value, object, (.=))
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import System.Timeout (timeout)

import Sabela.AI.Capabilities.Util (compactMaybeText, compactOutputs, fieldInt)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Anthropic.Types (CancelToken, isCancelled)
import Sabela.Api (errorJson)
import Sabela.Handlers (ReactiveNotebook (..))
import Sabela.Model
import qualified Sabela.SessionTypes as ST
import Sabela.State

{- | Run a single cell via the reactive notebook, compact the result, and
return a JSON summary suitable for embedding in a mutation-tool response.
Errors are surfaced; large outputs/errors are routed through the handle store.
-}
autoExecuteAfterMutation ::
    App -> AIStore -> ReactiveNotebook -> CancelToken -> Int -> IO Value
autoExecuteAfterMutation app store rn cancelTok cid = do
    res <- executeCell app rn cid cancelTok
    case res of
        Left err ->
            pure $
                object
                    [ "ran" .= True
                    , "ok" .= False
                    , "error" .= err
                    ]
        Right er -> do
            outputsField <- compactOutputs store (erOutputs er)
            errorField <- compactMaybeText store (erError er)
            let ok = null (erErrors er) && isNothing (erError er)
            pure $
                object
                    [ "ran" .= True
                    , "ok" .= ok
                    , "outputs" .= outputsField
                    , "error" .= errorField
                    , "errors" .= erErrors er
                    ]

execExecuteCell ::
    App -> AIStore -> ReactiveNotebook -> CancelToken -> Value -> IO ToolOutcome
execExecuteCell app store rn cancelTok input = do
    let mcid = fieldInt "cell_id" input
    case mcid of
        Nothing -> pure (errOutcome (errorJson "cell_id required"))
        Just cid -> do
            result <- executeCell app rn cid cancelTok
            case result of
                Left err -> pure (errOutcome (errorJson err))
                Right er -> do
                    outputsField <- compactOutputs store (erOutputs er)
                    errorField <- compactMaybeText store (erError er)
                    pure $
                        okOutcome $
                            object
                                [ "outputs" .= outputsField
                                , "error" .= errorField
                                , "errors" .= erErrors er
                                , "cellId" .= cid
                                ]

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
        | cancelled -> pure (Left "Cancelled")
        | stale -> pure (Left "Request superseded by a kernel interrupt")
        | otherwise -> case mResult of
            Nothing -> pure (Left "Cell execution timed out (>120s)")
            Just r -> pure (Right r)

-- | Did the Haskell kernel interrupt after this request was stamped?
requestStale :: App -> UTCTime -> IO Bool
requestStale app reqTime =
    getHaskellSession (appSessions app)
        >>= maybe (pure False) (`ST.sbRequestStale` reqTime)
