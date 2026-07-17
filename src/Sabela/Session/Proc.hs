{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

{- | Process-group lifecycle for interpreter subprocesses: masked spawn
into a registry, group signalling, and the single 'destroySession'
teardown chokepoint every kill path must route through.

The group-signalling leaves ('termGroupQuiet', 'killGroupQuiet',
'intGroupQuiet', 'rawKill') are the only platform-specific pieces: on
POSIX they signal the leader's process group; on Windows they act on the
process handle via the portable @process@ API (TerminateProcess and a
Ctrl-Break sent to the group). Everything above them is shared.
-}
module Sabela.Session.Proc (
    ProcSession (..),
    sessionProcessSpec,
    withSpawnedSession,
    destroySession,
    escalateKill,
    interruptGroup,
    forceKillGroup,
    killLeftoverSessions,
    configureSessionHandles,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (
    SomeException,
    mask,
    onException,
    try,
    uninterruptibleMask_,
 )
import Control.Monad (forM_, void)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Unique (Unique, newUnique)
import Sabela.Session.Reader (OutQueue, drainToEof, newOutQueue, readLoop)
import System.IO (
    BufferMode (LineBuffering),
    Handle,
    hClose,
    hSetBinaryMode,
    hSetBuffering,
    hSetEncoding,
    hSetNewlineMode,
    mkTextEncoding,
    noNewlineTranslation,
 )
import System.IO.Unsafe (unsafePerformIO)
import System.Process (
    CreateProcess (create_group, cwd, std_err, std_in, std_out),
    ProcessHandle,
    StdStream (CreatePipe),
    createProcess,
    getPid,
    getProcessExitCode,
    waitForProcess,
 )
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

{- | The sole way to build a session 'CreateProcess': piped std handles
and an own process group, so group signals can never reach the server.
-}
sessionProcessSpec :: Maybe FilePath -> CreateProcess -> CreateProcess
sessionProcessSpec mCwd cp =
    cp
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , cwd = mCwd
        , create_group = True
        }

{- | Spawn under mask, register before exceptions can land, run the
initialiser with exceptions restored, and tear down on any failure.
The returned session is always registered for the shutdown sweep.
-}
withSpawnedSession :: CreateProcess -> (ProcSession -> IO a) -> IO a
withSpawnedSession spec initialise = mask $ \restore -> do
    ps <- spawnRegistered spec
    restore (initialise ps) `onException` destroySession ps

spawnRegistered :: CreateProcess -> IO ProcSession
spawnRegistered spec = do
    (mIn, mOut, mErr, ph) <- createProcess spec
    ps <- flip onException (rawKill ph) $ do
        (hIn, hOut, hErr) <- case (mIn, mOut, mErr) of
            (Just a, Just b, Just c) -> pure (a, b, c)
            _ -> ioError (userError "session spawn: missing std pipe")
        pgid <- getPid ph
        q <- newOutQueue
        klock <- newMVar ()
        uid <- newUnique
        -- getPid is Maybe CPid on POSIX but Maybe Word32 on Windows; coerce to
        -- the field's ProcessGroupID so the spawn typechecks on both.
        let ps = ProcSession uid ph (fmap fromIntegral pgid) klock hIn hOut hErr q
        registryInsert ps
        pure ps
    flip onException (destroySession ps) $ do
        configureSessionHandles (psStdin ps) (psStdout ps) (psStderr ps)
        _ <- forkIO (readLoop (psStdout ps) (psQueue ps))
        pure ps

{- | Stdin gets transliterating UTF-8 (encode side); stdout/stderr are
read as raw bytes and decoded leniently by the reader.
-}
configureSessionHandles :: Handle -> Handle -> Handle -> IO ()
configureSessionHandles hIn hOut hErr = do
    hSetBuffering hIn LineBuffering
    hSetNewlineMode hOut noNewlineTranslation
    enc <- mkTextEncoding "UTF-8//TRANSLIT"
    hSetEncoding hIn enc
    forM_ [hOut, hErr] $ \h -> hSetBinaryMode h True

{- | Idempotent whole-tree teardown. The masked core probes under the
kill-lock (a reaped leader's pgid may be recycled, so it is then never
signalled), TERM→grace→KILL→reaps; drain and closes follow, bounded.
-}
destroySession :: ProcSession -> IO ()
destroySession ps = withMVar (psKillLock ps) $ \_ -> do
    uninterruptibleMask_ core
    _ <- timeout drainTimeoutUs (drainToEof (psQueue ps))
    mapM_ closeQuiet [psStdin ps, psStdout ps, psStderr ps]
  where
    core = do
        exited <- getProcessExitCode (psProc ps)
        case exited of
            Just _ -> pure ()
            Nothing -> do
                termGroupQuiet ps
                waitGraceFor ps gracePolls
                killGroupQuiet ps
        quiet (void (waitForProcess (psProc ps)))
        registryRemove (psId ps)

{- | The staged kill ladder behind the cell-timeout watchdog (P1
watchdog-respawn, stress cases 7–10, 13, 17). Escalates through the
portable group wrappers — INT → grace → TERM → grace → KILL — advancing a
rung only when the previous one was ignored, so a SIGINT-responsive
computation exits on the first rung while a signal-ignoring one is still
reaped by the final KILL.

On POSIX this is a true 3-rung INT/TERM/KILL ladder; on Windows it
collapses to 2 effective rungs (Ctrl-Break, then TerminateProcess, since
TERM≡KILL there). Probes under the kill-lock so a reaped (possibly
recycled) pgid is never signalled. The leader is not reaped here — the
caller respawns; 'destroySession' is the reaping chokepoint.
-}
escalateKill :: ProcSession -> IO ()
escalateKill ps = withMVar (psKillLock ps) $ \_ ->
    rung intGroupQuiet $
        rung termGroupQuiet $
            rung killGroupQuiet (pure ())
  where
    rung :: (ProcSession -> IO ()) -> IO () -> IO ()
    rung signal next = do
        exited <- getProcessExitCode (psProc ps)
        case exited of
            Just _ -> pure ()
            Nothing -> do
                signal ps
                waitGraceFor ps gracePolls
                next

{- | Poll the leader for up to @n@ grace ticks, stopping early once it has
exited. Shared by the teardown chokepoint and the escalation ladder so
both wait on the same budget between rungs.
-}
waitGraceFor :: ProcSession -> Int -> IO ()
waitGraceFor _ 0 = pure ()
waitGraceFor ps n = do
    exited <- getProcessExitCode (psProc ps)
    case exited of
        Just _ -> pure ()
        Nothing -> threadDelay gracePollUs >> waitGraceFor ps (n - 1)

{- | SIGINT the session's group to abort the current evaluation. Probes
under the kill-lock so a reaped (possibly recycled) pgid is never hit.
-}
interruptGroup :: ProcSession -> IO ()
interruptGroup ps = withMVar (psKillLock ps) $ \_ -> do
    exited <- getProcessExitCode (psProc ps)
    case exited of
        Just _ -> pure ()
        Nothing -> intGroupQuiet ps

{- | Forcibly KILL the session's group, for the orphan safeguard
('Sabela.Session.ParentPoller'): when the server is gone the poller runs
this once. Probes under the kill-lock so a reaped (possibly recycled) pgid
is never hit, and routes through the portable 'killGroupQuiet' wrapper.
-}
forceKillGroup :: ProcSession -> IO ()
forceKillGroup ps = withMVar (psKillLock ps) $ \_ -> do
    exited <- getProcessExitCode (psProc ps)
    case exited of
        Just _ -> pure ()
        Nothing -> killGroupQuiet ps

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

gracePolls, gracePollUs, drainTimeoutUs, closeTimeoutUs :: Int
gracePolls = 40
gracePollUs = 50000
drainTimeoutUs = 2000000
closeTimeoutUs = 1000000

{- | Live sessions by id, inserted at spawn and removed at reap, so the
shutdown sweep reclaims sessions that no manager slot references.
-}
{-# NOINLINE sessionRegistry #-}
sessionRegistry :: IORef (Map Unique ProcSession)
sessionRegistry = unsafePerformIO (newIORef Map.empty)

registryInsert :: ProcSession -> IO ()
registryInsert ps =
    atomicModifyIORef' sessionRegistry (\m -> (Map.insert (psId ps) ps m, ()))

registryRemove :: Unique -> IO ()
registryRemove uid =
    atomicModifyIORef' sessionRegistry (\m -> (Map.delete uid m, ()))

-- | Shutdown sweep: swap the registry out, then destroy every leftover.
killLeftoverSessions :: IO ()
killLeftoverSessions = do
    m <- atomicModifyIORef' sessionRegistry (Map.empty,)
    mapM_ destroySession (Map.elems m)
