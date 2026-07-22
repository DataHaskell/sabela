{- | Atomic run-lock admission: one 'tryTakeMVar' decides busy and acquires
the slot in a single step (§1.4 TOCTOU), recording the holder id and hold
instant so a bounce can name the locking cell and its elapsed time (R6.4).
-}
module Sabela.Session.Admission (
    Admission (..),
    admit,
) where

import Control.Concurrent (MVar, putMVar, tryTakeMVar)
import Control.Exception (finally, mask)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

{- | Outcome of an atomic admission attempt at the run-lock: 'Ran' carries the
action's result (the slot was free and we held it for the run); 'Busy' carries
the cell id already holding the lock (or our own candidate if no holder was
recorded) plus how long it has held it, so a denial can name both (R6.4). The
sum tag is the atomic decision, mapped to the busy 'ToolOutcome' by the
dispatcher, not a re-read of the @running@ boolean.
-}
data Admission a
    = Ran a
    | Busy {running :: !Int, runningForMs :: !Int}
    deriving (Eq, Show)

{- | Atomic admission: a SINGLE 'tryTakeMVar' on the run-lock decides busy and
acquires the slot in one step — no check-then-acquire gap where two callers
both observe \"free\" and then both stack behind the lock. On a free lock it
records the candidate id and hold instant, runs the action, and restores the
lock even on exception; on a held lock it reports 'Busy' with the recorded
holder id and its elapsed hold without ever blocking. A lone caller on a free
lock always 'Ran' and never deadlocks.
-}
admit ::
    MVar () -> IORef (Maybe (Int, Word64)) -> Int -> IO a -> IO (Admission a)
admit lock holderRef cid act = mask $ \restore -> do
    got <- tryTakeMVar lock
    case got of
        Nothing -> do
            held <- readIORef holderRef
            now <- getMonotonicTimeNSec
            let (hid, ms) = case held of
                    Just (h, t0) -> (h, fromIntegral ((now - t0) `div` 1000000))
                    Nothing -> (cid, 0)
            pure (Busy hid ms)
        Just () -> do
            now <- getMonotonicTimeNSec
            writeIORef holderRef (Just (cid, now))
            let release = writeIORef holderRef Nothing >> putMVar lock ()
            r <- restore act `finally` release
            pure (Ran r)
