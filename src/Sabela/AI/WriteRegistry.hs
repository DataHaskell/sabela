{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.WriteRegistry (
    WriteRegistry,
    PendingWrite (..),
    WriteState (..),
    newWriteRegistry,
    writeIdentity,
    registerWrite,
    settleWrite,
    markDelivered,
    lookupWrite,
    dropWrite,
    firstRunningWrite,
    elapsedMsOf,
    peekSettled,
    awaitWriteSettled,
    drainSettledWrites,
) where

import Control.Monad (unless)

import Control.Concurrent.STM (
    STM,
    TVar,
    atomically,
    newTVar,
    newTVarIO,
    readTVar,
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

data WriteState = WriteRunning | WriteSettled Value Bool

data PendingWrite = PendingWrite
    { pwCellId :: Int
    , pwStartedNs :: Word64
    , pwState :: TVar WriteState
    }

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
        let pw = PendingWrite cid now st
        m <- readTVar reg
        writeTVar reg (M.insert key pw m)
        pure pw

settleWrite :: PendingWrite -> Value -> IO ()
settleWrite pw v = atomically $ writeTVar (pwState pw) (WriteSettled v False)

markDelivered :: PendingWrite -> IO ()
markDelivered pw = atomically $ do
    st <- readTVar (pwState pw)
    case st of
        WriteSettled v _ -> writeTVar (pwState pw) (WriteSettled v True)
        WriteRunning -> pure ()

lookupWrite :: WriteRegistry -> Text -> IO (Maybe PendingWrite)
lookupWrite (WriteRegistry reg) key =
    atomically (M.lookup key <$> readTVar reg)

dropWrite :: WriteRegistry -> Text -> IO ()
dropWrite (WriteRegistry reg) key =
    atomically (readTVar reg >>= writeTVar reg . M.delete key)

firstRunningWrite :: WriteRegistry -> IO (Maybe PendingWrite)
firstRunningWrite (WriteRegistry reg) = atomically $ do
    pws <- M.elems <$> readTVar reg
    running <- filterSTM isRunning pws
    pure $ case sortOn pwStartedNs running of
        (pw : _) -> Just pw
        [] -> Nothing
  where
    isRunning pw = do
        st <- readTVar (pwState pw)
        pure $ case st of
            WriteRunning -> True
            WriteSettled _ _ -> False

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
