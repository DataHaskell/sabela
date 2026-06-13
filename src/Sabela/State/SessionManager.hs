module Sabela.State.SessionManager (
    SessionManager (..),
    newSessionManager,
    getHaskellSession,
    takeHaskellSession,
    setHaskellSession,
    modifyHaskellSession,
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
 )
import Control.Exception (SomeException, finally, try)
import Control.Monad (void)
import qualified Sabela.SessionTypes as ST

data SessionManager = SessionManager
    { smHaskell :: MVar (Maybe ST.SessionBackend)
    , smPython :: MVar (Maybe ST.SessionBackend)
    }

newSessionManager :: IO SessionManager
newSessionManager =
    SessionManager
        <$> newMVar Nothing
        <*> newMVar Nothing

getHaskellSession :: SessionManager -> IO (Maybe ST.SessionBackend)
getHaskellSession = readMVar . smHaskell

-- | Swap the slot to Nothing and return the old backend (close it outside).
takeHaskellSession :: SessionManager -> IO (Maybe ST.SessionBackend)
takeHaskellSession sm = modifyMVar (smHaskell sm) (\old -> pure (Nothing, old))

setHaskellSession :: SessionManager -> Maybe ST.SessionBackend -> IO ()
setHaskellSession sm val = modifyMVar_ (smHaskell sm) (\_ -> pure val)

modifyHaskellSession ::
    SessionManager ->
    (Maybe ST.SessionBackend -> IO (Maybe ST.SessionBackend)) ->
    IO ()
modifyHaskellSession sm = modifyMVar_ (smHaskell sm)

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
