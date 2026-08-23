{-# LANGUAGE OverloadedStrings #-}

{- | A write nobody is working on any more. The registry called a write running
until its runner settled it, so a handler that died mid-flight — a client that
hung up, a driver killed between the register and the fork — left an entry that
bounced every later write as busy for the life of the server.
-}
module Test.WriteAbandonSpec (spec) where

import Control.Concurrent (
    forkIO,
    killThread,
    newEmptyMVar,
    putMVar,
    takeMVar,
    threadDelay,
 )
import Data.Aeson (Value (..), object, (.=))
import Test.Hspec

import Sabela.AI.WriteRegistry (
    PendingWrite,
    WriteLiveness (..),
    attachRunner,
    drainSettledWrites,
    firstRunningWrite,
    firstRunningWriteWithin,
    livenessOf,
    lookupWrite,
    newWriteRegistry,
    pwCellId,
    registerWrite,
    settleWrite,
 )

summary :: Value
summary = object ["ok" .= True]

{- | Which cell a registry answer names, if it names one. 'PendingWrite' holds
a TVar and so cannot be shown; the cell id is what a caller is told anyway.
-}
cellRunning :: IO (Maybe PendingWrite) -> IO (Maybe Int)
cellRunning = fmap (fmap pwCellId)

-- | A write whose runner is blocked until the returned action releases it.
withBlockedRunner :: PendingWrite -> IO (IO ())
withBlockedRunner pw = do
    gate <- newEmptyMVar
    tid <- forkIO (takeMVar gate)
    attachRunner pw tid
    pure (putMVar gate ())

spec :: Spec
spec = do
    abandoned
    live

abandoned :: Spec
abandoned = describe "a write whose runner is gone" $ do
    it "no longer counts as running" $ do
        reg <- newWriteRegistry
        pw <- registerWrite reg "k" 3
        gate <- newEmptyMVar
        tid <- forkIO (takeMVar gate)
        attachRunner pw tid
        killThread tid
        cellRunning (firstRunningWrite reg) `shouldReturn` Nothing

    it "is pruned, so the same write can be issued again" $ do
        reg <- newWriteRegistry
        pw <- registerWrite reg "k" 3
        gate <- newEmptyMVar
        tid <- forkIO (takeMVar gate)
        attachRunner pw tid
        killThread tid
        _ <- firstRunningWrite reg
        cellRunning (lookupWrite reg "k") `shouldReturn` Nothing

    it "does not take a settled write with it" $ do
        reg <- newWriteRegistry
        pw <- registerWrite reg "k" 3
        tid <- forkIO (settleWrite pw summary)
        attachRunner pw tid
        threadDelay 20000
        cellRunning (firstRunningWrite reg) `shouldReturn` Nothing
        drainSettledWrites reg 0 `shouldReturn` [(3, summary)]

live :: Spec
live = describe "a write someone is still working on" $ do
    it "holds the door shut while its runner lives" $ do
        reg <- newWriteRegistry
        pw <- registerWrite reg "k" 7
        release <- withBlockedRunner pw
        cellRunning (firstRunningWrite reg) `shouldReturn` Just 7
        release

    it "counts as running before a runner is attached, inside the grace" $ do
        reg <- newWriteRegistry
        _ <- registerWrite reg "k" 7
        cellRunning (firstRunningWrite reg) `shouldReturn` Just 7

    it "stops counting once the grace for the fork has lapsed" $ do
        reg <- newWriteRegistry
        pw <- registerWrite reg "k" 7
        livenessOf 0 pw `shouldReturn` WriteAbandoned
        cellRunning (firstRunningWriteWithin 0 reg) `shouldReturn` Nothing
