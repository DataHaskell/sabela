{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The write-ack seam (R6.1/R6.2/R6.4): a durable mutation write is acked
within 'writeAckDeadlineUs' whether or not its execution has settled; a
byte-identical retry answers idempotently from the registry instead of
duplicating the cell; a kernel-needing call during one's own in-flight write
bounces with the writing cell named; @await_idle@ reconciles settled writes
exactly once via 'settledWritesField'.
-}
module Sabela.AI.Capabilities.Edit.Ack (
    ackWriteAndRun,
    writeGate,
    writeAckDeadlineUs,
    writeSettleGraceUs,
    settledWritesField,
) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Types (Pair)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Sabela.AI.Capabilities.Edit.Run (autoExecuteAfterMutation)
import Sabela.AI.Capabilities.KernelHealth (noteSettled)
import Sabela.AI.Doc (cellHash)
import Sabela.AI.Store (AIStore, aiWriteReg)
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.AI.WriteAck
import Sabela.AI.WriteRegistry
import Sabela.Anthropic.Types (CancelToken)
import Sabela.Handlers (ReactiveNotebook)
import Sabela.Model (Cell (..), lookupCell)
import Sabela.State (App (..), readNotebook)

{- | Seconds a mutation write may spend executing before the tool response
stops waiting and acks @executing@ (default 25s, well under the 60s transport
timeout); @SABELA_WRITE_ACK_SECS@ overrides.
-}
writeAckDeadlineUs :: IO Int
writeAckDeadlineUs = do
    m <- lookupEnv "SABELA_WRITE_ACK_SECS"
    let secs = fromMaybe 25 (m >>= readMaybe) :: Double
    pure (round (secs * 1000000))

-- | How long @await_idle@ waits for a still-running write's summary to settle.
writeSettleGraceUs :: Int
writeSettleGraceUs = 15000000

{- | Register the just-landed cell and race its execution against the ack
deadline: settled in time answers @completed@ inline; otherwise the response
is the immediate @executing@ ack and @await_idle@ reconciles later. A cell
class that never executes settles at once (so its retry still dedupes).
-}
ackWriteAndRun ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    CancelToken ->
    Value ->
    Cell ->
    Bool ->
    [Text] ->
    IO ToolOutcome
ackWriteAndRun app store rn cancelTok input cell runnable notes = do
    pw <- registerWrite (aiWriteReg store) (writeIdentity input) (cellId cell)
    if not runnable
        then do
            settleWrite pw Null
            markDelivered pw
            pure (okOutcome (ackJson cell AckCompleted (Just Null) False notes))
        else do
            void . forkIO $ do
                r <-
                    try
                        (autoExecuteAfterMutation app store rn cancelTok (cellId cell))
                settleWrite pw (either exceptionSummary id r)
            deadline <- writeAckDeadlineUs
            mSummary <- awaitWriteSettled pw deadline
            case mSummary of
                Just s -> do
                    markDelivered pw
                    -- The inline-completed ack IS a settle observation: open
                    -- the post-settled consistency window (R6.4).
                    noteSettled app store
                    pure (okOutcome (ackJson cell AckCompleted (Just s) False notes))
                Nothing ->
                    pure
                        ( okOutcome
                            ( ackJson
                                cell
                                AckExecuting
                                Nothing
                                False
                                (notes <> [landedNote (cellId cell)])
                            )
                        )

-- | A crashed execution thread still settles, as a bounded error summary.
exceptionSummary :: SomeException -> Value
exceptionSummary e =
    object
        [ "ok" .= False
        , "error" .= T.take 200 (T.pack (show e))
        ]

ackJson :: Cell -> AckStatus -> Maybe Value -> Bool -> [Text] -> Value
ackJson cell status mExec dup notes =
    writeAckJson
        WriteAck
            { waCellId = cellId cell
            , waStatus = status
            , waHash = Just (cellHash cell)
            , waExecution = mExec
            , waDuplicate = dup
            , waNote =
                if null notes then Nothing else Just (T.unwords notes)
            }

{- | The pre-dispatch write gate. A byte-identical retry of a registered write
whose cell still exists answers idempotently (R6.2); any other kernel-needing
call while a write is still executing bounces naming the writing cell and its
elapsed time (R6.4). 'Nothing' lets the dispatch proceed normally.
-}
writeGate ::
    App -> AIStore -> Bool -> Bool -> Value -> IO (Maybe ToolOutcome)
writeGate app store isInsert isKernelTool input
    | isInsert = do
        mDup <- dedupe
        case mDup of
            Just out -> pure (Just out)
            Nothing -> bounceIfOwnWriteRunning
    | isKernelTool = bounceIfOwnWriteRunning
    | otherwise = pure Nothing
  where
    reg = aiWriteReg store
    key = writeIdentity input
    dedupe = do
        mPw <- lookupWrite reg key
        case mPw of
            Nothing -> pure Nothing
            Just pw -> do
                nb <- readNotebook (appNotebook app)
                case lookupCell (pwCellId pw) nb of
                    Nothing -> dropWrite reg key >> pure Nothing
                    Just cell -> Just <$> duplicateOutcome cell pw
    duplicateOutcome cell pw = do
        mSummary <- peekSettled pw
        pure . okOutcome $ case mSummary of
            Just s ->
                ackJson cell AckCompleted (Just s) True [dupSettledNote (cellId cell)]
            Nothing ->
                ackJson cell AckExecuting Nothing True [dupRunningNote (cellId cell)]
    bounceIfOwnWriteRunning = do
        mRunning <- firstRunningWrite reg
        case mRunning of
            Nothing -> pure Nothing
            Just pw -> do
                ms <- elapsedMsOf pw
                pure (Just (errOutcome (busyAckJson (BusyAck (pwCellId pw) ms))))

{- | The @await_idle@ reconciliation field: every settled-but-undelivered
write as @{cellId, status, execution}@, delivered exactly once; empty when
nothing is pending (the field is then omitted entirely).
-}
settledWritesField :: AIStore -> IO [Pair]
settledWritesField store = do
    ws <- drainSettledWrites (aiWriteReg store) writeSettleGraceUs
    pure
        ( [ "writes"
                .= [ writeAckJson
                        WriteAck
                            { waCellId = cid
                            , waStatus = AckCompleted
                            , waHash = Nothing
                            , waExecution = Just s
                            , waDuplicate = False
                            , waNote = Nothing
                            }
                   | (cid, s) <- ws
                   ]
          | not (null ws)
          ]
        )
