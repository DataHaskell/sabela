{-# LANGUAGE OverloadedStrings #-}

module Test.AdmissionSpec (spec) where

import Control.Concurrent (
    forkIO,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    threadDelay,
    tryReadMVar,
 )
import Control.Concurrent.MVar (MVar)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (partition)
import Data.Maybe (isNothing, mapMaybe)
import Data.Word (Word64)

import Sabela.AI.Capabilities (needsKernel)
import Sabela.AI.Capabilities.ToolName (
    ToolName (..),
    parseToolName,
    toolWireName,
 )
import Sabela.AI.Capabilities.Tools (chatTools)
import Sabela.Anthropic.Types (ToolDef (..))
import Sabela.Session.Admission (Admission (..), admit)
import Test.Hspec

newKernelTools :: [ToolName]
newKernelTools = [KernelStatus, Interrupt, KernelRestart, ExportNotebook]

spec :: Spec
spec = do
    describe "kernel-control tool names" $ do
        it "every new tool parses from its wire name" $
            mapMaybe (parseToolName . toolWireName) newKernelTools
                `shouldBe` newKernelTools
        it "wire names are the documented strings" $
            map toolWireName newKernelTools
                `shouldBe` [ "kernel_status"
                           , "interrupt"
                           , "kernel_restart"
                           , "export_notebook"
                           ]
        it "all four appear in the chat catalogue" $
            let names = map tdName chatTools
             in map toolWireName newKernelTools
                    `shouldSatisfy` all (`elem` names)

    describe "needsKernel: which tools gate on the kernel" $ do
        it "a kernel-needing tool (ExecuteCell) needs the kernel" $
            needsKernel ExecuteCell `shouldBe` True
        it "a kernel-needing tool (CheckType) needs the kernel" $
            needsKernel CheckType `shouldBe` True
        it "a kernel-control tool (KernelStatus) does not gate on the kernel" $
            needsKernel KernelStatus `shouldBe` False
        it "a read-only tool (ListCells) does not gate on the kernel" $
            needsKernel ListCells `shouldBe` False

    describe "atomic admission at the run-lock (tryTakeMVar gate)" $ do
        it "two simultaneous executes yield exactly one Ran and one Busy" $ do
            (rans, busies) <- raceAdmit admitWith
            (length rans, length busies) `shouldBe` (1, 1)

        it "the loser's Busy reports a running cell id from the race" $ do
            (_, busies) <- raceAdmit admitWith
            all ((`elem` [1, 2]) . running) busies `shouldBe` True

        it "a lone admit on a free lock runs and never deadlocks" $ do
            lock <- newMVar ()
            reg <- newIORef Nothing
            out <- admit lock reg 7 (pure "done")
            out `shouldBe` Ran "done"
            again <- admit lock reg 7 (pure "again")
            again `shouldBe` Ran "again"

        it "the gate FAILS against the old check-then-acquire shape" $ do
            (rans, busies) <- raceAdmit checkThenAcquire
            (length rans, length busies) `shouldBe` (2, 0)

type Strategy =
    MVar () ->
    IORef (Maybe (Int, Word64)) ->
    IO () ->
    Int ->
    IO () ->
    IO (Admission Int)

raceAdmit :: Strategy -> IO ([Int], [Admission Int])
raceAdmit strat = do
    lock <- newMVar ()
    reg <- newIORef Nothing
    arrived <- newEmptyMVar
    proceed <- newEmptyMVar
    gate <- newEmptyMVar
    results <- newEmptyMVar
    let barrier = putMVar arrived () >> takeMVar proceed
        work = readMVar gate
        caller cid = strat lock reg barrier cid work >>= putMVar results
    _ <- forkIO (caller 1)
    _ <- forkIO (caller 2)
    takeMVar arrived
    takeMVar arrived
    putMVar proceed ()
    putMVar proceed ()
    threadDelay 20000
    putMVar gate ()
    outs <- mapM (const (takeMVar results)) [1 :: Int, 2]
    let (rans, busies) = partition isRan outs
    pure (mapMaybe ranId rans, busies)

admitWith :: Strategy
admitWith lock reg barrier cid work =
    barrier >> admit lock reg cid (work >> pure cid)

checkThenAcquire :: Strategy
checkThenAcquire lock reg barrier cid work = do
    busy <- isNothing <$> tryReadMVar lock
    barrier
    if busy
        then do
            held <- readIORef reg
            pure (Busy (maybe cid fst held) 0)
        else do
            takeMVar lock
            writeIORef reg (Just (cid, 0))
            r <- work >> pure cid
            writeIORef reg Nothing
            putMVar lock ()
            pure (Ran r)

isRan :: Admission a -> Bool
isRan Ran{} = True
isRan Busy{} = False

ranId :: Admission Int -> Maybe Int
ranId (Ran n) = Just n
ranId Busy{} = Nothing
