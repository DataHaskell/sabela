{-# LANGUAGE OverloadedStrings #-}

{- | Pins the P0 AI-bridge admission layer: the four kernel-control tools
('kernel_status', 'interrupt', 'kernel_restart', 'export_notebook') are in
the catalogue and round-trip through the dispatcher, the 'needsKernel'
classification (which tools gate on the kernel) is pinned, and the atomic
run-lock gate ('admit') admits exactly one of two simultaneous callers.
-}
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
import Data.Maybe (fromMaybe, isNothing, mapMaybe)

import Sabela.AI.Capabilities (needsKernel)
import Sabela.AI.Capabilities.ToolName (
    ToolName (..),
    parseToolName,
    toolWireName,
 )
import Sabela.AI.Capabilities.Tools (chatTools)
import Sabela.Anthropic.Types (ToolDef (..))
import Sabela.Session (Admission (..), admit)
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
        it "a kernel-needing tool (GhciQuery) needs the kernel" $
            needsKernel GhciQuery `shouldBe` True
        it "a kernel-control tool (KernelStatus) does not gate on the kernel" $
            needsKernel KernelStatus `shouldBe` False
        it "a read-only tool (ListCells) does not gate on the kernel" $
            needsKernel ListCells `shouldBe` False

    describe "atomic admission at the run-lock (tryTakeMVar gate)" $ do
        it "two simultaneous executes yield exactly one Ran and one Busy" $ do
            (rans, busies) <- raceAdmit admitWith
            (length rans, length busies) `shouldBe` (1, 1)

        it "the loser's Busy reports a running cell id from the race" $ do
            -- The atomic decision is exact (one Ran, one Busy); the holder id
            -- on Busy is the recorded holder, falling back to the caller's own
            -- candidate before the winner's write is visible. Either way it is
            -- one of the two racing ids, never a bogus value.
            (_, busies) <- raceAdmit admitWith
            all ((`elem` [1, 2]) . running) busies `shouldBe` True

        it "a lone admit on a free lock runs and never deadlocks" $ do
            lock <- newMVar ()
            reg <- newIORef Nothing
            out <- admit lock reg 7 (pure "done")
            out `shouldBe` Ran "done"
            -- the slot is restored, so a second admit succeeds too.
            again <- admit lock reg 7 (pure "again")
            again `shouldBe` Ran "again"

        it "the gate FAILS against the old check-then-acquire shape" $ do
            -- The discriminator: check-then-acquire lets BOTH callers see the
            -- lock free and then both stack behind it, so the busy count is 0.
            -- This is exactly what the (1,1) gate above rejects.
            (rans, busies) <- raceAdmit checkThenAcquire
            (length rans, length busies) `shouldBe` (2, 0)

------------------------------------------------------------------------
-- Atomic-admission gate harness
------------------------------------------------------------------------

{- | An admission strategy on ONE run-lock, parameterised by a barrier the
caller invokes between its busy decision and acquiring the slot. The real
atomic 'admit' ignores the barrier (it has no separate read); the old
check-then-acquire shape uses it to expose its TOCTOU window — both callers
pass the busy check before either takes the lock.
-}
type Strategy =
    MVar () -> IORef (Maybe Int) -> IO () -> Int -> IO () -> IO (Admission Int)

{- | Race two callers (ids 1 and 2) through a 'Strategy' on ONE run-lock. A
two-arrival barrier sits between the busy decision and the acquire; the
winner's work then blocks until both have decided, so the simultaneity is
real, not a stagger artefact. Returns the @Ran@ ids and the @Busy@ outcomes.
-}
raceAdmit :: Strategy -> IO ([Int], [Admission Int])
raceAdmit strat = do
    lock <- newMVar ()
    reg <- newIORef Nothing
    arrived <- newEmptyMVar -- each caller posts () on reaching the barrier
    proceed <- newEmptyMVar -- the test opens the barrier once both arrived
    gate <- newEmptyMVar -- a held work peeks this; filled once to free all
    results <- newEmptyMVar
    let barrier = putMVar arrived () >> takeMVar proceed
        work = readMVar gate -- peek (does not empty), so any holder unblocks
        caller cid = strat lock reg barrier cid work >>= putMVar results
    _ <- forkIO (caller 1)
    _ <- forkIO (caller 2)
    -- Both callers reach the barrier (and so have made their busy decision)
    -- before either acquires — the TOCTOU window the strategies differ on.
    takeMVar arrived
    takeMVar arrived
    putMVar proceed ()
    putMVar proceed ()
    threadDelay 20000 -- let the winner acquire and enter its held work
    putMVar gate () -- free every held work; serialized acquirers drain in turn
    outs <- mapM (const (takeMVar results)) [1 :: Int, 2]
    let (rans, busies) = partition isRan outs
    pure (mapMaybe ranId rans, busies)

{- | The real atomic gate: barrier (so both callers arrive together), then a
single 'tryTakeMVar' that decides busy and acquires in one step — exactly one
caller wins regardless of arrival order, so the gate is (1 Ran, 1 Busy).
-}
admitWith :: Strategy
admitWith lock reg barrier cid work =
    barrier >> admit lock reg cid (work >> pure cid)

{- | The OLD check-then-acquire shape: a non-blocking busy read, the barrier,
THEN a blocking acquire. Both simultaneous callers read \"free\" at the
barrier and then both stack behind the lock — so both eventually 'Ran' and
neither reports 'Busy'. The loser's blocking acquire returns only once the
winner releases.
-}
checkThenAcquire :: Strategy
checkThenAcquire lock reg barrier cid work = do
    busy <- isNothing <$> tryReadMVar lock
    barrier
    if busy
        then Busy . fromMaybe cid <$> readIORef reg
        else do
            takeMVar lock
            writeIORef reg (Just cid)
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
