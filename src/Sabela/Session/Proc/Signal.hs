{-# LANGUAGE CPP #-}

{- | The spawned session record and the platform-specific leaves that signal
it. On POSIX these signal the leader's process group; on Windows they act on
the process handle (TerminateProcess, and Ctrl-Break for an interrupt).
Everything that sequences them lives in "Sabela.Session.Proc".
-}
module Sabela.Session.Proc.Signal (
    ProcSession (..),
    ProcessGroupID,
    rawKill,
    termGroupQuiet,
    killGroupQuiet,
    intGroupQuiet,
    closeQuiet,
    quiet,
) where

import Control.Concurrent.MVar (MVar)
import Control.Exception (SomeException, try, uninterruptibleMask_)
import Control.Monad (forM_, void)
import Data.Unique (Unique)
import Sabela.Session.Reader (OutQueue)
import System.IO (Handle, hClose)
import System.Process (ProcessHandle, getPid, waitForProcess)
import System.Timeout (timeout)

#if defined(mingw32_HOST_OS)
import Data.Word (Word32)
import System.Process (interruptProcessGroupOf, terminateProcess)
#else
import System.Posix.Signals (
    Signal,
    sigINT,
    sigKILL,
    sigTERM,
    signalProcessGroup,
 )
import System.Posix.Types (ProcessGroupID)
#endif

#if defined(mingw32_HOST_OS)
-- | Windows has no POSIX process-group id; getPid yields a Word32.
type ProcessGroupID = Word32
#endif

{- | A spawned interpreter process: handles, its process group (captured
once at spawn, while the leader is alive), the output queue its reader
feeds, and the kill-lock serialising teardown.
-}
data ProcSession = ProcSession
    { psId :: Unique
    , psProc :: ProcessHandle
    , psPgid :: Maybe ProcessGroupID
    , psKillLock :: MVar ()
    , psStdin :: Handle
    , psStdout :: Handle
    , psStderr :: Handle
    , psQueue :: OutQueue
    }

-- | Pre-registration failure path: forcibly kill the leftover tree, reap.
rawKill :: ProcessHandle -> IO ()
rawKill ph = uninterruptibleMask_ $ do
    rawKillTree ph
    quiet (void (waitForProcess ph))

#if defined(mingw32_HOST_OS)
-- | Windows has no process group to signal: terminate the handle.
rawKillTree :: ProcessHandle -> IO ()
rawKillTree ph = quiet (terminateProcess ph)
#else
-- | POSIX: group-KILL by the live handle's pid (the group leader).
rawKillTree :: ProcessHandle -> IO ()
rawKillTree ph = do
    mPid <- getPid ph
    forM_ mPid $ \pid -> quiet (signalProcessGroup sigKILL pid)
#endif

#if defined(mingw32_HOST_OS)
{- | Graceful, forcible, and interrupt signals to a session's tree.
Windows has no process-group signals: TERM and KILL both terminate the
process (TerminateProcess) and INT sends Ctrl-Break to its group.
-}
termGroupQuiet, killGroupQuiet, intGroupQuiet :: ProcSession -> IO ()
termGroupQuiet ps = quiet (terminateProcess (psProc ps))
killGroupQuiet ps = quiet (terminateProcess (psProc ps))
intGroupQuiet ps = quiet (interruptProcessGroupOf (psProc ps))
#else
{- | Graceful, forcible, and interrupt signals to a session's tree. POSIX
sends the signal to the leader's process group.
-}
termGroupQuiet, killGroupQuiet, intGroupQuiet :: ProcSession -> IO ()
termGroupQuiet = signalGroupQuiet sigTERM
killGroupQuiet = signalGroupQuiet sigKILL
intGroupQuiet = signalGroupQuiet sigINT

signalGroupQuiet :: Signal -> ProcSession -> IO ()
signalGroupQuiet sig ps =
    forM_ (psPgid ps) $ \pgid -> quiet (signalProcessGroup sig pgid)
#endif

closeQuiet :: Handle -> IO ()
closeQuiet h = void (timeout closeTimeoutUs (quiet (hClose h)))

quiet :: IO () -> IO ()
quiet act = void (try act :: IO (Either SomeException ()))

closeTimeoutUs :: Int
closeTimeoutUs = 1000000
