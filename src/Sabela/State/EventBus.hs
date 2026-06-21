module Sabela.State.EventBus (
    EventBus (..),
    newEventBus,
    broadcast,
    bumpGeneration,
    isCurrentGen,
    whenCurrentGen,
    subscribeBroadcast,
    AwaitResult (..),
    awaitExecutionDone,
    debugLog,
) where

import Control.Concurrent.STM (
    TChan,
    atomically,
    dupTChan,
    newBroadcastTChanIO,
    readTChan,
    writeTChan,
 )
import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Sabela.Model (NotebookEvent (..))
import Sabela.State.Environment (Environment (..))
import System.IO (stderr)
import System.Timeout (timeout)

data EventBus = EventBus
    { ebBroadcast :: TChan NotebookEvent
    , ebGeneration :: IORef Int
    }

newEventBus :: IO EventBus
newEventBus = EventBus <$> newBroadcastTChanIO <*> newIORef 0

broadcast :: EventBus -> NotebookEvent -> IO ()
broadcast eb ev = atomically $ writeTChan (ebBroadcast eb) ev

bumpGeneration :: EventBus -> IO Int
bumpGeneration eb =
    atomicModifyIORef' (ebGeneration eb) (\g -> let g' = g + 1 in (g', g'))

isCurrentGen :: EventBus -> Int -> IO Bool
isCurrentGen eb gen = (== gen) <$> readIORef (ebGeneration eb)

whenCurrentGen :: EventBus -> Int -> IO () -> IO ()
whenCurrentGen eb gen action = do
    still <- isCurrentGen eb gen
    when still action

subscribeBroadcast :: EventBus -> IO (TChan NotebookEvent)
subscribeBroadcast eb = atomically $ dupTChan (ebBroadcast eb)

{- | Why 'awaitExecutionDone' returned. 'AwaitSettled' is the @EvExecutionDone@
fence firing for the cascade in flight; 'AwaitKernelDead' is the kill-aware
exit (the kernel went absent, so no fence will ever come); 'AwaitTimedOut' is
the bounded budget elapsing — the caller re-loops in all but 'AwaitSettled'.
-}
data AwaitResult
    = AwaitSettled
    | AwaitKernelDead
    | AwaitTimedOut
    deriving (Show, Eq)

{- | Lock-free bounded long-poll: subscribe, then block on the broadcast
channel until @EvExecutionDone@ fires (the cascade releases the run-lock
between cells, so a @running == false@ sample would settle too early —
the fence is the only reliable signal). Bounded by a wall-clock deadline
@budgetUs@ from now, so a flood of non-fence events cannot defeat the
ceiling; @kernelAlive@ is probed on every iteration (not only the quiet
branch) so a mid-stream kernel death returns 'AwaitKernelDead' at once.
-}
awaitExecutionDone :: EventBus -> Int -> IO Bool -> IO AwaitResult
awaitExecutionDone eb budgetUs kernelAlive = do
    chan <- subscribeBroadcast eb
    now <- getMonotonicTimeNSec
    loop chan (now + fromIntegral (max 0 budgetUs) * 1000)
  where
    sliceUs = max 1 (min budgetUs 200000)
    loop chan deadline = do
        alive <- kernelAlive
        if not alive
            then pure AwaitKernelDead
            else do
                remaining <- remainingUs deadline
                if remaining <= 0
                    then pure AwaitTimedOut
                    else do
                        mEv <-
                            timeout
                                (min sliceUs remaining)
                                (atomically (readTChan chan))
                        case mEv of
                            Just EvExecutionDone -> pure AwaitSettled
                            _ -> loop chan deadline

-- | Microseconds left until @deadline@ (a monotonic-clock instant in ns).
remainingUs :: Word64 -> IO Int
remainingUs deadline = do
    now <- getMonotonicTimeNSec
    pure $ if now >= deadline then 0 else fromIntegral ((deadline - now) `div` 1000)

debugLog :: Environment -> Text -> IO ()
debugLog env msg = when (envDebugLog env) $ TIO.hPutStrLn stderr msg
