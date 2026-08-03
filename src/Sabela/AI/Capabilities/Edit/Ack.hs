{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.AI.Capabilities.Edit.Ack (
    ackWriteAndRun,
    writeGate,
    writeAckDeadlineUs,
    writeSettleGraceUs,
    settledWritesField,
    withNote,
) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Sabela.AI.Capabilities.Edit.Run (autoExecuteAfterMutation)
import Sabela.AI.Capabilities.KernelHealth (noteSettled)
import Sabela.AI.Capabilities.Try.Payload.Checked (
    RunRecord (..),
    runRecordOf,
 )
import Sabela.AI.Doc (cellHash)
import Sabela.AI.Store (AIStore, aiWriteReg)
import Sabela.AI.Types (ToolOutcome (..), errOutcome, okOutcome)
import Sabela.AI.WriteAck
import Sabela.AI.WriteRegistry
import Sabela.Anthropic.Types (CancelToken)
import Sabela.Handlers (ReactiveNotebook)
import Sabela.Model (Cell (..), lookupCell)
import Sabela.State (App (..), readNotebook)

writeAckDeadlineUs :: IO Int
writeAckDeadlineUs = do
    m <- lookupEnv "SABELA_WRITE_ACK_SECS"
    let secs = fromMaybe 25 (m >>= readMaybe) :: Double
    pure (round (secs * 1000000))

writeSettleGraceUs :: Int
writeSettleGraceUs = 15000000

{- | Notes are asked for rather than given: what a write may claim about its
cell depends on whether this acknowledgement ends up carrying a run of it, and
that is only known here.
-}
ackWriteAndRun ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    CancelToken ->
    Value ->
    Cell ->
    Bool ->
    (RunRecord -> [Text]) ->
    IO ToolOutcome
ackWriteAndRun app store rn cancelTok input cell runnable notesFor = do
    pw <- registerWrite (aiWriteReg store) (writeIdentity input) (cellId cell)
    if not runnable
        then do
            settleWrite pw Null
            markDelivered pw
            pure
                ( okOutcome
                    (ackJson cell AckCompleted (Just Null) False (notesFor RunNotAttempted))
                )
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
                    noteSettled app store
                    pure
                        ( okOutcome
                            (ackJson cell AckCompleted (Just s) False (notesFor (runRecordOf s)))
                        )
                Nothing ->
                    pure
                        ( okOutcome
                            ( ackJson
                                cell
                                AckExecuting
                                Nothing
                                False
                                (notesFor RunUnderway <> [landedNote (cellId cell)])
                            )
                        )

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

{- | Adds a note to whatever the write came back as. Notes accumulate and
apply to a refusal too: a disclosure the caller loses is a disclosure that
never happened, and a rejected write is the one that most needs it.
-}
withNote :: Text -> ToolOutcome -> ToolOutcome
withNote n out = case out of
    ToolOk v -> ToolOk (annotate v)
    ToolErr v -> ToolErr (annotate v)
  where
    annotate (Object o) =
        Object (KM.insert "note" (String (joined (existing o))) o)
    annotate v = v
    existing o = case KM.lookup "note" o of
        Just (String s) -> s
        _ -> ""
    joined earlier
        | T.null earlier = n
        | n `T.isInfixOf` earlier = earlier
        | otherwise = earlier <> " " <> n
