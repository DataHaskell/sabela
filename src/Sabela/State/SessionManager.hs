{- | Session slots and the lifecycle mutex.

Lock order, outermost first. Acquiring against it deadlocks, and none of these
are reentrant:

> smHaskellLifecycle  >  smHaskell  >  nsNotebook
> sessLock            >  sessQueryLock
> psKillLock: a leaf. Never acquire any of the above while holding it.

Two consequences worth stating: a caller already inside 'withHaskellLifecycle'
must use the @Unlocked@ variant of anything that would retake it, and roots must
be computed under a notebook read but executed outside it.
-}
module Sabela.State.SessionManager (
    SessionManager (..),
    newSessionManager,
    getHaskellSession,
    takeHaskellSession,
    takeHaskellSessionIfSame,
    setHaskellSession,
    modifyHaskellSession,
    withHaskellLifecycle,
    lifecycleHolder,
    currentKernelEpoch,
    freshHaskellSessionGeneration,
    markHaskellContextReady,
    haskellContextReady,
    clearHaskellContextReady,
    installHaskellSession,
    recordHaskellEnv,
    haskellEnvOf,
    getPythonSession,
    setPythonSession,
    modifyPythonSession,
    forceResetAllSessions,
) where

import Control.Concurrent (ThreadId, forkIO, myThreadId, threadDelay)
import Control.Concurrent.MVar (
    MVar,
    modifyMVar,
    modifyMVar_,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    tryTakeMVar,
    withMVar,
 )
import Control.Exception (SomeException, bracket_, finally, try)
import Control.Monad (void)
import Data.IORef (
    IORef,
    atomicModifyIORef',
    newIORef,
    readIORef,
    writeIORef,
 )
import Data.Unique (Unique)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Sabela.Deps (EnvSig)
import qualified Sabela.SessionTypes as ST
import System.IO (hPutStrLn, stderr)

data SessionManager = SessionManager
    { smHaskell :: MVar (Maybe ST.SessionBackend)
    , smHaskellLifecycle :: MVar ()
    , smHaskellGeneration :: IORef Int
    , smHaskellContext :: IORef (Maybe (Unique, Int))
    , smHaskellEnv :: IORef (Maybe (Unique, EnvSig))
    , smLifecycleHolder :: IORef (Maybe (ThreadId, Word64))
    , smPython :: MVar (Maybe ST.SessionBackend)
    }

newSessionManager :: IO SessionManager
newSessionManager =
    SessionManager
        <$> newMVar Nothing
        <*> newMVar ()
        <*> newIORef 0
        <*> newIORef Nothing
        <*> newIORef Nothing
        <*> newIORef Nothing
        <*> newMVar Nothing

getHaskellSession :: SessionManager -> IO (Maybe ST.SessionBackend)
getHaskellSession = readMVar . smHaskell

takeHaskellSession :: SessionManager -> IO (Maybe ST.SessionBackend)
takeHaskellSession sm =
    modifyMVar (smHaskell sm) $ \old -> do
        clearHaskellContextReady sm
        pure (Nothing, old)

takeHaskellSessionIfSame ::
    SessionManager -> Unique -> IO (Maybe ST.SessionBackend)
takeHaskellSessionIfSame sm expected =
    modifyMVar (smHaskell sm) $ \current ->
        case current of
            Just backend | ST.sbSessionId backend == expected ->
                do
                    clearHaskellContextReady sm
                    pure (Nothing, Just backend)
            _ -> pure (current, Nothing)

{- | Installing a kernel advances the epoch. It used to take the /kernel's own/
generation, which is 1 for every freshly spawned process, so the counter was
pinned at 1 and two restarts were indistinguishable to anything comparing it.
-}
setHaskellSession :: SessionManager -> Maybe ST.SessionBackend -> IO ()
setHaskellSession sm val = do
    case val of
        Just _ -> atomicModifyIORef' (smHaskellGeneration sm) (\n -> (n + 1, ()))
        Nothing -> pure ()
    clearHaskellContextReady sm
    modifyMVar_ (smHaskell sm) (\_ -> pure val)

{- | How many kernels this server has installed. Monotone for the life of the
process, so a client that saw @n@ and now sees @n+1@ knows its results belong to
a kernel that no longer exists.
-}
currentKernelEpoch :: SessionManager -> IO Int
currentKernelEpoch = readIORef . smHaskellGeneration

{- | Pure by construction: running IO while holding the slot would let a caller
block every other accessor, and would invite taking a lock below this one in the
order documented above.
-}
modifyHaskellSession ::
    SessionManager ->
    (Maybe ST.SessionBackend -> Maybe ST.SessionBackend) ->
    IO ()
modifyHaskellSession sm f =
    modifyMVar_ (smHaskell sm) $ \current -> do
        clearHaskellContextReady sm
        pure (f current)

{- | Serialise kernel lifecycle work, announcing an already-held lock. @App@
retains a reference, so a caller blocked here never raises
@BlockedIndefinitelyOnMVar@ and would otherwise just stop, silently.
-}
withHaskellLifecycle :: SessionManager -> IO a -> IO a
withHaskellLifecycle sm act = do
    self <- myThreadId
    readIORef (smLifecycleHolder sm) >>= mapM_ (announceWait self)
    withMVar (smHaskellLifecycle sm) . const $
        bracket_ (claim self) release act
  where
    claim self = do
        now <- getMonotonicTimeNSec
        writeIORef (smLifecycleHolder sm) (Just (self, now))
    release = writeIORef (smLifecycleHolder sm) Nothing
    announceWait self (holder, since) = do
        now <- getMonotonicTimeNSec
        hPutStrLn stderr $
            "[session] "
                <> show self
                <> " is waiting for the kernel lifecycle lock, held by "
                <> show holder
                <> " for "
                <> show ((now - since) `div` 1000000)
                <> "ms"

-- | Who holds the lifecycle lock, and since when (monotonic nanoseconds).
lifecycleHolder :: SessionManager -> IO (Maybe (ThreadId, Word64))
lifecycleHolder = readIORef . smLifecycleHolder

freshHaskellSessionGeneration :: SessionManager -> IO Int
freshHaskellSessionGeneration sm =
    atomicModifyIORef' (smHaskellGeneration sm) $ \generation ->
        let next = generation + 1
         in (next, next)

markHaskellContextReady ::
    SessionManager -> ST.SessionBackend -> Int -> IO ()
markHaskellContextReady sm backend eventGeneration =
    writeIORef
        (smHaskellContext sm)
        (Just (ST.sbSessionId backend, eventGeneration))

haskellContextReady ::
    SessionManager -> ST.SessionBackend -> Int -> IO Bool
haskellContextReady sm backend eventGeneration =
    (== Just (ST.sbSessionId backend, eventGeneration))
        <$> readIORef (smHaskellContext sm)

{- | Clearing the context also clears the recorded environment, so every site
that mutates the session slot forgets both without having to remember to.
-}
clearHaskellContextReady :: SessionManager -> IO ()
clearHaskellContextReady sm = do
    writeIORef (smHaskellContext sm) Nothing
    writeIORef (smHaskellEnv sm) Nothing

{- | Install a kernel together with the environment it was built for. These are
one operation: a session whose provenance is unknown reads as stale, so
installing one without recording the other produces a kernel nothing will use.
-}
installHaskellSession ::
    SessionManager -> ST.SessionBackend -> EnvSig -> IO ()
installHaskellSession sm backend sig = do
    setHaskellSession sm (Just backend)
    recordHaskellEnv sm backend sig

{- | Record what the now-current kernel was built from. Must be called /after/
'setHaskellSession', which clears the record as part of installing the slot.
Prefer 'installHaskellSession', which cannot be got in the wrong order.
-}
recordHaskellEnv :: SessionManager -> ST.SessionBackend -> EnvSig -> IO ()
recordHaskellEnv sm backend sig =
    writeIORef (smHaskellEnv sm) (Just (ST.sbSessionId backend, sig))

{- | The environment of the kernel that is running now. 'Nothing' means no kernel
has been recorded, which is what makes a failed or timed-out build read as stale
rather than clean.
-}
haskellEnvOf :: SessionManager -> IO (Maybe (Unique, EnvSig))
haskellEnvOf = readIORef . smHaskellEnv

getPythonSession :: SessionManager -> IO (Maybe ST.SessionBackend)
getPythonSession = readMVar . smPython

setPythonSession :: SessionManager -> Maybe ST.SessionBackend -> IO ()
setPythonSession sm val = modifyMVar_ (smPython sm) (\_ -> pure val)

modifyPythonSession ::
    SessionManager ->
    (Maybe ST.SessionBackend -> IO (Maybe ST.SessionBackend, a)) ->
    IO a
modifyPythonSession sm = modifyMVar (smPython sm)

forceResetAllSessions :: SessionManager -> IO ()
forceResetAllSessions sm = do
    clearHaskellContextReady sm
    done <- newEmptyMVar
    _ <- forkIO $ resetSlot "Python" (smPython sm) `finally` putMVar done ()
    resetSlot "Haskell" (smHaskell sm)
    takeMVar done
  where
    resetSlot label mv =
        forceResetMVar
            label
            mv
            (\s -> void (try (ST.sbClose s) :: IO (Either SomeException ())))

{- | Empty a session slot, closing its contents. Retries a briefly-held slot and
reports if it never wins: a kill that silently does nothing leaves a live kernel
installed, the state this module exists to prevent.
-}
forceResetMVar :: String -> MVar (Maybe a) -> (a -> IO ()) -> IO ()
forceResetMVar label mv close = go resetPolls
  where
    go :: Int -> IO ()
    go 0 =
        hPutStrLn stderr $
            "[session] could not reset the "
                <> label
                <> " slot: still held after "
                <> show (resetPolls * resetPollUs `div` 1000)
                <> "ms; a session may still be running"
    go n = do
        taken <- tryTakeMVar mv
        case taken of
            Just (Just s) -> do
                _ <- try (close s) :: IO (Either SomeException ())
                putMVar mv Nothing
            Just Nothing -> putMVar mv Nothing
            Nothing -> threadDelay resetPollUs >> go (n - 1)

resetPolls, resetPollUs :: Int
resetPolls = 100
resetPollUs = 20000
