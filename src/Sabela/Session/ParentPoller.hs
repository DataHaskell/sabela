{-# LANGUAGE CPP #-}

{- | Orphan safeguard: when the server dies ungracefully (SIGKILL / crash), a
child interpreter in its own process group can survive as an orphan. Each
platform reaps it differently:

* __POSIX__ — a separate @sh@ "reaper" process, a child of the server but a
  distinct process, so it outlives the server's death, notices it (a @kill -0@
  probe on the server pid fails), and group-kills the interpreter. It also
  self-exits when the interpreter dies first (graceful teardown).

* __Windows__ — a Job Object with @JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE@: the
  interpreter is assigned to a job whose handle the server holds open, so the
  OS kills the job (hence the interpreter) the moment the server's last handle
  closes — kernel-enforced, even on an ungraceful death. Raw @kernel32@ FFI
  (auto-linked); no @Win32@ package dependency.
-}
module Sabela.Session.ParentPoller (
    spawnParentPoller,
    spawnReaperFor,
) where

#if !defined(mingw32_HOST_OS)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import System.Posix.Process (getProcessID)
import System.Posix.Types (ProcessGroupID, ProcessID)
import System.Process (createProcess, proc, waitForProcess)
#else
import Control.Monad (void, when)
import Data.Bits ((.|.))
import Data.Word (Word32)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (pokeByteOff)
#endif

#if defined(mingw32_HOST_OS)
-- | Windows has no POSIX process ids; the reaper coerces them to Word32.
type ProcessGroupID = Word32
type ProcessID = Word32
#endif

{- | Install the orphan safeguard for an interpreter whose process group
leader pid is @pidLeader@ (equal to its pgid for an own-group session).
-}
spawnParentPoller :: ProcessGroupID -> IO ()
#if !defined(mingw32_HOST_OS)
spawnParentPoller pidLeader = getProcessID >>= \server -> spawnReaperFor server pidLeader

{- | Spawn a detached @sh@ reaper watching @serverPid@ and the interpreter's
group leader (@pgid@): it group-kills @pgid@ once the server dies, and
self-exits if the interpreter dies first. @serverPid@ is a parameter so this
is testable with a controllable stand-in server. POSIX only. -}
spawnReaperFor :: ProcessID -> ProcessGroupID -> IO ()
spawnReaperFor serverPid pgid = do
    (_, _, _, ph) <-
        createProcess
            (proc "sh" ["-c", reaperScript, "sabela-reaper", show serverPid, show pgid])
    void (forkIO (void (waitForProcess ph)))
  where
    reaperScript =
        "while kill -0 \"$1\" 2>/dev/null && kill -0 \"$2\" 2>/dev/null; "
            ++ "do sleep 0.5; done; kill -9 -\"$2\" 2>/dev/null"
#else
spawnParentPoller pidLeader = do
    let pid = fromIntegral pidLeader :: Word32
    hProc <- c_OpenProcess (processSetQuota .|. processTerminate) 0 pid
    when (hProc /= nullPtr) $ do
        hJob <- c_CreateJobObjectW nullPtr nullPtr
        when (hJob /= nullPtr) $ do
            allocaBytes jobInfoSize $ \p -> do
                fillBytes p 0 jobInfoSize
                pokeByteOff p limitFlagsOffset (killOnJobClose :: Word32)
                void
                    ( c_SetInformationJobObject
                        hJob
                        jobObjectExtendedLimitInformation
                        p
                        (fromIntegral jobInfoSize)
                    )
            void (c_AssignProcessToJobObject hJob hProc)
        -- Deliberately do NOT CloseHandle hJob: keeping the job handle open
        -- ties the job's lifetime to the server process. On server exit (incl.
        -- SIGKILL / crash) the OS closes it, and KILL_ON_JOB_CLOSE reaps the
        -- interpreter. The handle is reclaimed when the server process exits.
        void (c_CloseHandle hProc)

-- | This entry point is POSIX-only; on Windows the Job Object in
-- 'spawnParentPoller' is the whole mechanism, so the reaper is a no-op.
spawnReaperFor :: ProcessID -> ProcessGroupID -> IO ()
spawnReaperFor _ _ = pure ()

-- PROCESS_TERMINATE | PROCESS_SET_QUOTA — the access AssignProcessToJobObject needs.
processTerminate, processSetQuota :: Word32
processTerminate = 0x0001
processSetQuota = 0x0100

-- JobObjectExtendedLimitInformation (info class 9) + the kill-on-close flag.
jobObjectExtendedLimitInformation :: CInt
jobObjectExtendedLimitInformation = 9

killOnJobClose :: Word32
killOnJobClose = 0x2000

{- | 64-bit @JOBOBJECT_EXTENDED_LIMIT_INFORMATION@ layout (the GHC Windows
target is x64): total 144 bytes, with @BasicLimitInformation.LimitFlags@ at
offset 16 (after two 8-byte @LARGE_INTEGER@ time limits). -}
jobInfoSize, limitFlagsOffset :: Int
jobInfoSize = 144
limitFlagsOffset = 16

foreign import ccall unsafe "windows.h OpenProcess"
    c_OpenProcess :: Word32 -> CInt -> Word32 -> IO (Ptr ())

foreign import ccall unsafe "windows.h CreateJobObjectW"
    c_CreateJobObjectW :: Ptr () -> Ptr () -> IO (Ptr ())

foreign import ccall unsafe "windows.h SetInformationJobObject"
    c_SetInformationJobObject :: Ptr () -> CInt -> Ptr () -> Word32 -> IO CInt

foreign import ccall unsafe "windows.h AssignProcessToJobObject"
    c_AssignProcessToJobObject :: Ptr () -> Ptr () -> IO CInt

foreign import ccall unsafe "windows.h CloseHandle"
    c_CloseHandle :: Ptr () -> IO CInt
#endif
