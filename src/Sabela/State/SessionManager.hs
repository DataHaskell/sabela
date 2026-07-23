module Sabela.State.SessionManager (
    SessionManager (..),
    newSessionManager,
    getHaskellSession,
    takeHaskellSession,
    takeHaskellSessionIfSame,
    setHaskellSession,
    modifyHaskellSession,
    withHaskellLifecycle,
    freshHaskellSessionGeneration,
    markHaskellContextReady,
    haskellContextReady,
    clearHaskellContextReady,
    getPythonSession,
    setPythonSession,
    modifyPythonSession,
    forceResetAllSessions,
) where

import Control.Concurrent (forkIO)
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
import Control.Exception (SomeException, finally, try)
import Control.Monad (void)
import Data.IORef (
    IORef,
    atomicModifyIORef',
    newIORef,
    readIORef,
    writeIORef,
 )
import Data.Unique (Unique)
import qualified Sabela.SessionTypes as ST

data SessionManager = SessionManager
    { smHaskell :: MVar (Maybe ST.SessionBackend)
    , smHaskellLifecycle :: MVar ()
    , smHaskellGeneration :: IORef Int
    , smHaskellContext :: IORef (Maybe (Unique, Int))
    , smPython :: MVar (Maybe ST.SessionBackend)
    }

newSessionManager :: IO SessionManager
newSessionManager =
    SessionManager
        <$> newMVar Nothing
        <*> newMVar ()
        <*> newIORef 0
        <*> newIORef Nothing
        <*> newMVar Nothing

getHaskellSession :: SessionManager -> IO (Maybe ST.SessionBackend)
getHaskellSession = readMVar . smHaskell

-- | Swap the slot to Nothing and return the old backend (close it outside).
takeHaskellSession :: SessionManager -> IO (Maybe ST.SessionBackend)
takeHaskellSession sm =
    modifyMVar (smHaskell sm) $ \old -> do
        clearHaskellContextReady sm
        pure (Nothing, old)

{- | Atomically detach one exact backend. A late crash/recovery callback must
never clear a replacement installed after the callback captured its handle.
-}
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

setHaskellSession :: SessionManager -> Maybe ST.SessionBackend -> IO ()
setHaskellSession sm val = do
    generation <- maybe (pure 0) ST.sbSessionGen val
    atomicModifyIORef'
        (smHaskellGeneration sm)
        (\known -> (max known generation, ()))
    clearHaskellContextReady sm
    modifyMVar_ (smHaskell sm) (\_ -> pure val)

modifyHaskellSession ::
    SessionManager ->
    (Maybe ST.SessionBackend -> IO (Maybe ST.SessionBackend)) ->
    IO ()
modifyHaskellSession sm action =
    modifyMVar_ (smHaskell sm) $ \current -> do
        replacement <- action current
        clearHaskellContextReady sm
        pure replacement

{- | Serialize process replacement/build operations without holding the
session-slot MVar across a slow Cabal build.
-}
withHaskellLifecycle :: SessionManager -> IO a -> IO a
withHaskellLifecycle sm = withMVar (smHaskellLifecycle sm) . const

{- | Allocate a process generation that is monotonic across whole-process
replacement, not merely an in-place backend reset.
-}
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

clearHaskellContextReady :: SessionManager -> IO ()
clearHaskellContextReady sm = writeIORef (smHaskellContext sm) Nothing

getPythonSession :: SessionManager -> IO (Maybe ST.SessionBackend)
getPythonSession = readMVar . smPython

setPythonSession :: SessionManager -> Maybe ST.SessionBackend -> IO ()
setPythonSession sm val = modifyMVar_ (smPython sm) (\_ -> pure val)

modifyPythonSession ::
    SessionManager ->
    (Maybe ST.SessionBackend -> IO (Maybe ST.SessionBackend, a)) ->
    IO a
modifyPythonSession sm = modifyMVar (smPython sm)

{- | Force-reset all sessions without blocking on MVars: tryTakeMVar so it
never deadlocks; slots it cannot grab are reclaimed by the caller's
registry sweep. The two backends close concurrently to bound shutdown.
-}
forceResetAllSessions :: SessionManager -> IO ()
forceResetAllSessions sm = do
    clearHaskellContextReady sm
    done <- newEmptyMVar
    _ <- forkIO $ resetSlot (smPython sm) `finally` putMVar done ()
    resetSlot (smHaskell sm)
    takeMVar done
  where
    resetSlot mv =
        forceResetMVar
            mv
            (\s -> void (try (ST.sbClose s) :: IO (Either SomeException ())))

{- | Close-and-clear one slot without blocking: a slot whose MVar is held
by another thread is skipped (at server exit the shutdown registry sweep
still reclaims it; runtime resets simply leave it to its holder).
-}
forceResetMVar :: MVar (Maybe a) -> (a -> IO ()) -> IO ()
forceResetMVar mv close = do
    taken <- tryTakeMVar mv
    case taken of
        Just (Just s) -> do
            _ <- try (close s) :: IO (Either SomeException ())
            putMVar mv Nothing
        Just Nothing -> putMVar mv Nothing
        Nothing -> pure ()
