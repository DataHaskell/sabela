{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.WriteRegistry (
    WriteRegistry,
    PendingWrite (..),
    WriteState (..),
    WriteLiveness (..),
    newWriteRegistry,
    writeIdentity,
    registerWrite,
    attachRunner,
    livenessOf,
    forkGraceNs,
    settleWrite,
    markDelivered,
    lookupWrite,
    dropWrite,
    firstRunningWrite,
    firstRunningWriteWithin,
    elapsedMsOf,
    peekSettled,
    awaitWriteSettled,
    drainSettledWrites,
) where

import Control.Monad (unless)

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (
    STM,
    TVar,
    atomically,
    newTVar,
    newTVarIO,
    readTVar,
    readTVarIO,
    registerDelay,
    retry,
    writeTVar,
 )
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (ThreadStatus (..), threadStatus)

data WriteState = WriteRunning | WriteSettled Value Bool

data PendingWrite = PendingWrite
    { pwCellId :: Int
    , pwStartedNs :: Word64
    , pwState :: TVar WriteState
    , pwRunner :: TVar (Maybe ThreadId)
    }

{- | Whether a write is still being worked on. A write is live because someone
is working on it, not because it was once registered: an entry whose runner
thread is gone will never settle, and must not hold the door shut for the life
of the server.
-}
data WriteLiveness = WriteLive | WriteAbandoned | WriteDone
    deriving (Eq, Show)

{- | How long a write may claim to be running before the thread that will
settle it exists. It covers the fork alone, not the run.
-}
forkGraceNs :: Word64
forkGraceNs = 5 * 1000000000

newtype WriteRegistry = WriteRegistry (TVar (M.Map Text PendingWrite))

newWriteRegistry :: IO WriteRegistry
newWriteRegistry = WriteRegistry <$> newTVarIO M.empty

writeIdentity :: Value -> Text
writeIdentity input =
    T.intercalate
        "\US"
        [textField k input | k <- ["source", "cell_type", "language"]]
  where
    textField k (Object o) = case KM.lookup (Key.fromText k) o of
        Just (String s) -> s
        _ -> ""
    textField _ _ = ""

registerWrite :: WriteRegistry -> Text -> Int -> IO PendingWrite
registerWrite (WriteRegistry reg) key cid = do
    now <- getMonotonicTimeNSec
    atomically $ do
        st <- newTVar WriteRunning
        runner <- newTVar Nothing
        let pw = PendingWrite cid now st runner
        m <- readTVar reg
        writeTVar reg (M.insert key pw m)
        pure pw

{- | Names the thread that will settle this write. Until one is attached the
write is live on the clock alone, and only for 'forkGraceNs'.
-}
attachRunner :: PendingWrite -> ThreadId -> IO ()
attachRunner pw tid = atomically (writeTVar (pwRunner pw) (Just tid))

livenessOf :: Word64 -> PendingWrite -> IO WriteLiveness
livenessOf graceNs pw = do
    st <- readTVarIO (pwState pw)
    case st of
        WriteSettled _ _ -> pure WriteDone
        WriteRunning -> maybe unforked runnerGone =<< readTVarIO (pwRunner pw)
  where
    runnerGone tid = do
        s <- threadStatus tid
        pure $ case s of
            ThreadFinished -> WriteAbandoned
            ThreadDied -> WriteAbandoned
            _ -> WriteLive
    unforked = do
        now <- getMonotonicTimeNSec
        pure $
            if now - pwStartedNs pw >= graceNs
                then WriteAbandoned
                else WriteLive

settleWrite :: PendingWrite -> Value -> IO ()
settleWrite pw v = atomically $ writeTVar (pwState pw) (WriteSettled v False)

markDelivered :: PendingWrite -> IO ()
markDelivered pw = atomically $ do
    st <- readTVar (pwState pw)
    case st of
        WriteSettled v _ -> writeTVar (pwState pw) (WriteSettled v True)
        WriteRunning -> pure ()

{- | The write registered under this identity, if one is still being worked
on. An abandoned entry is dropped rather than answered, so re-issuing a write
whose handler died is a fresh write and not a duplicate of a ghost.
-}
lookupWrite :: WriteRegistry -> Text -> IO (Maybe PendingWrite)
lookupWrite r@(WriteRegistry reg) key = do
    mPw <- atomically (M.lookup key <$> readTVar reg)
    case mPw of
        Nothing -> pure Nothing
        Just pw -> do
            l <- livenessOf forkGraceNs pw
            if l == WriteAbandoned
                then dropWrite r key >> pure Nothing
                else pure (Just pw)

dropWrite :: WriteRegistry -> Text -> IO ()
dropWrite (WriteRegistry reg) key =
    atomically (readTVar reg >>= writeTVar reg . M.delete key)

{- | The oldest write still being worked on, pruning the ones nobody is. This
is what tells a caller the surface is busy, so an entry it keeps is an entry
that blocks every write until it settles.
-}
firstRunningWrite :: WriteRegistry -> IO (Maybe PendingWrite)
firstRunningWrite = firstRunningWriteWithin forkGraceNs

firstRunningWriteWithin :: Word64 -> WriteRegistry -> IO (Maybe PendingWrite)
firstRunningWriteWithin graceNs r@(WriteRegistry reg) = do
    entries <- atomically (M.toList <$> readTVar reg)
    tagged <- mapM tag entries
    mapM_ (dropWrite r) [k | (k, _, WriteAbandoned) <- tagged]
    pure (oldest [pw | (_, pw, WriteLive) <- tagged])
  where
    tag (k, pw) = (,,) k pw <$> livenessOf graceNs pw
    oldest pws = case sortOn pwStartedNs pws of
        (pw : _) -> Just pw
        [] -> Nothing

elapsedMsOf :: PendingWrite -> IO Int
elapsedMsOf pw = do
    now <- getMonotonicTimeNSec
    pure (fromIntegral ((now - pwStartedNs pw) `div` 1000000))

peekSettled :: PendingWrite -> IO (Maybe Value)
peekSettled pw = atomically $ do
    st <- readTVar (pwState pw)
    pure $ case st of
        WriteSettled v _ -> Just v
        WriteRunning -> Nothing

awaitWriteSettled :: PendingWrite -> Int -> IO (Maybe Value)
awaitWriteSettled pw us = do
    delay <- registerDelay us
    atomically $ do
        st <- readTVar (pwState pw)
        case st of
            WriteSettled v _ -> pure (Just v)
            WriteRunning -> do
                timedOut <- readTVar delay
                if timedOut then pure Nothing else retry

drainSettledWrites :: WriteRegistry -> Int -> IO [(Int, Value)]
drainSettledWrites r@(WriteRegistry reg) graceUs = do
    mRunning <- firstRunningWrite r
    case mRunning of
        Nothing -> pure ()
        Just _ -> do
            delay <- registerDelay graceUs
            atomically $ do
                pws <- M.elems <$> readTVar reg
                running <- filterSTM stillRunning pws
                timedOut <- readTVar delay
                unless (null running || timedOut) retry
    atomically $ do
        pws <- M.elems <$> readTVar reg
        undelivered <- filterSTM settledUndelivered pws
        mapM_ modifyState undelivered
        fmap (sortOn fst) (mapM summaryOf undelivered)
  where
    stillRunning pw = do
        st <- readTVar (pwState pw)
        pure $ case st of
            WriteRunning -> True
            WriteSettled _ _ -> False
    settledUndelivered pw = do
        st <- readTVar (pwState pw)
        pure $ case st of
            WriteSettled _ False -> True
            _ -> False
    modifyState pw = do
        st <- readTVar (pwState pw)
        case st of
            WriteSettled v _ -> writeTVar (pwState pw) (WriteSettled v True)
            WriteRunning -> pure ()
    summaryOf pw = do
        st <- readTVar (pwState pw)
        pure $ case st of
            WriteSettled v _ -> (pwCellId pw, v)
            WriteRunning -> (pwCellId pw, Null)

filterSTM :: (a -> STM Bool) -> [a] -> STM [a]
filterSTM p = foldr step (pure [])
  where
    step x acc = do
        keep <- p x
        rest <- acc
        pure (if keep then x : rest else rest)
