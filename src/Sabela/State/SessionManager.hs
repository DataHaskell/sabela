module Sabela.State.SessionManager (
    SessionManager (..),
    newSessionManager,
    getHaskellSession,
    setHaskellSession,
    modifyHaskellSession,
    getLeanSession,
    modifyLeanSession,
    getPythonSession,
    setPythonSession,
    modifyPythonSession,
    forceResetAllSessions,
) where

import Control.Concurrent.MVar (
    MVar,
    modifyMVar,
    modifyMVar_,
    newMVar,
    putMVar,
    readMVar,
    tryTakeMVar,
 )
import Control.Exception (SomeException, try)
import Control.Monad (forM_, void)
import Sabela.LeanRepl (LeanSession, closeLeanSession)
import qualified Sabela.SessionTypes as ST

data SessionManager = SessionManager
    { smHaskell :: MVar (Maybe ST.SessionBackend)
    , smLean :: MVar (Maybe LeanSession)
    -- ^ Unwrapped: executeLeanCells needs direct REPL access for env chaining.
    , smPython :: MVar (Maybe ST.SessionBackend)
    }

newSessionManager :: IO SessionManager
newSessionManager =
    SessionManager
        <$> newMVar Nothing
        <*> newMVar Nothing
        <*> newMVar Nothing

getHaskellSession :: SessionManager -> IO (Maybe ST.SessionBackend)
getHaskellSession = readMVar . smHaskell

setHaskellSession :: SessionManager -> Maybe ST.SessionBackend -> IO ()
setHaskellSession sm val = modifyMVar_ (smHaskell sm) (\_ -> pure val)

modifyHaskellSession ::
    SessionManager ->
    (Maybe ST.SessionBackend -> IO (Maybe ST.SessionBackend)) ->
    IO ()
modifyHaskellSession sm = modifyMVar_ (smHaskell sm)

getLeanSession :: SessionManager -> IO (Maybe LeanSession)
getLeanSession = readMVar . smLean

modifyLeanSession ::
    SessionManager -> (Maybe LeanSession -> IO (Maybe LeanSession, a)) -> IO a
modifyLeanSession sm = modifyMVar (smLean sm)

getPythonSession :: SessionManager -> IO (Maybe ST.SessionBackend)
getPythonSession = readMVar . smPython

setPythonSession :: SessionManager -> Maybe ST.SessionBackend -> IO ()
setPythonSession sm val = modifyMVar_ (smPython sm) (\_ -> pure val)

modifyPythonSession ::
    SessionManager ->
    (Maybe ST.SessionBackend -> IO (Maybe ST.SessionBackend, a)) ->
    IO a
modifyPythonSession sm = modifyMVar (smPython sm)

{- | Force-reset all sessions without blocking on MVars.
Uses tryTakeMVar so it never deadlocks, even if another thread holds the lock.
Closes any sessions it can grab, then puts Nothing back.
-}
forceResetAllSessions :: SessionManager -> IO ()
forceResetAllSessions sm = do
    forceResetMVar
        (smHaskell sm)
        (\s -> void (try (ST.sbClose s) :: IO (Either SomeException ())))
    forceResetMVarLean (smLean sm)
    forceResetMVar
        (smPython sm)
        (\s -> void (try (ST.sbClose s) :: IO (Either SomeException ())))

forceResetMVar :: MVar (Maybe a) -> (a -> IO ()) -> IO ()
forceResetMVar mv close = do
    taken <- tryTakeMVar mv
    case taken of
        Just (Just s) -> do
            _ <- try (close s) :: IO (Either SomeException ())
            putMVar mv Nothing
        Just Nothing -> putMVar mv Nothing
        Nothing -> pure () -- MVar is held by another thread; that thread will clean up

forceResetMVarLean :: MVar (Maybe LeanSession) -> IO ()
forceResetMVarLean mv = do
    taken <- tryTakeMVar mv
    case taken of
        Just (Just ls) -> do
            _ <- try (closeLeanSession ls) :: IO (Either SomeException ())
            putMVar mv Nothing
        Just Nothing -> putMVar mv Nothing
        Nothing -> pure ()
