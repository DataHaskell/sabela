{-# LANGUAGE OverloadedStrings #-}

{- | One wall-clock budget shared by every command a pure evaluation sends, so
the admission check, the run and the closing fingerprint cannot each spend the
caller's full timeout in turn.
-}
module Sabela.Session.Query.PureEval.Deadline (
    EvalDeadline (..),
    newEvalDeadline,
    remainingDeadlineUs,
    deadlineCommand,
    fingerprintCommand,
    deadlineMessage,
    commandTimedOut,
    timeoutRecovery,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

import Sabela.Session (Session, runBlockStreamingUnlockedWithTimeout)
import Sabela.Session.Query.Command (queryTimeoutUs)
import Sabela.SessionTypes (PureEvalRecovery (..))

data EvalDeadline = EvalDeadline
    { evalDeadlineNs :: Word64
    , evalDeadlineBudgetUs :: Int
    }

{- | The grace a fingerprint gets after the deadline has already expired: the
run is over either way, and a listing is still needed to judge it.
-}
fingerprintRecoveryGraceUs :: Int
fingerprintRecoveryGraceUs = 1000000

newEvalDeadline :: Int -> IO EvalDeadline
newEvalDeadline requestedUs = do
    now <- getMonotonicTimeNSec
    let budgetUs = max 1 requestedUs
    pure
        EvalDeadline
            { evalDeadlineNs = now + fromIntegral budgetUs * 1000
            , evalDeadlineBudgetUs = budgetUs
            }

remainingDeadlineUs :: EvalDeadline -> IO (Maybe Int)
remainingDeadlineUs deadline = do
    now <- getMonotonicTimeNSec
    pure $
        if now >= evalDeadlineNs deadline
            then Nothing
            else
                Just
                    ( fromIntegral
                        ((evalDeadlineNs deadline - now + 999) `div` 1000)
                    )

lockedCommand :: Int -> Session -> Text -> IO (Text, Text)
lockedCommand budgetUs sess command =
    runBlockStreamingUnlockedWithTimeout budgetUs sess command (\_ -> pure ())

deadlineCommand :: EvalDeadline -> Int -> Session -> Text -> IO (Text, Text)
deadlineCommand deadline capUs sess command = do
    remaining <- remainingDeadlineUs deadline
    case remaining of
        Nothing -> pure ("", deadlineMessage deadline)
        Just budgetUs -> lockedCommand (max 1 (min capUs budgetUs)) sess command

fingerprintCommand :: EvalDeadline -> Session -> IO (Bool, Text, Text)
fingerprintCommand deadline sess = do
    remaining <- remainingDeadlineUs deadline
    case remaining of
        Just budgetUs -> do
            (out, err) <-
                lockedCommand (max 1 (min queryTimeoutUs budgetUs)) sess ":show bindings"
            pure (False, out, err)
        Nothing -> do
            (out, err) <- lockedCommand fingerprintRecoveryGraceUs sess ":show bindings"
            pure (True, out, err)

deadlineMessage :: EvalDeadline -> Text
deadlineMessage deadline =
    "*** Execution timed out after "
        <> T.pack (show (evalDeadlineBudgetUs deadline))
        <> " microseconds; pure request deadline exhausted ***"

timeoutRecovery :: Text -> PureEvalRecovery
timeoutRecovery err
    | "pure request deadline exhausted" `T.isInfixOf` err = PureEvalNoRecovery
    | otherwise = PureEvalInterrupted

commandTimedOut :: Text -> Bool
commandTimedOut = T.isInfixOf "Execution timed out after"
