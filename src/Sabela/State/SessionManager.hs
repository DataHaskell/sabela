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
) where

import Control.Concurrent.MVar (
    MVar,
    modifyMVar,
    modifyMVar_,
    newMVar,
    readMVar,
 )
import Sabela.LeanRepl (LeanSession)
import Sabela.SessionTypes (SessionBackend)

data SessionManager = SessionManager
    { smHaskell :: MVar (Maybe SessionBackend)
    , smLean :: MVar (Maybe LeanSession)
    -- ^ Unwrapped: executeLeanCells needs direct REPL access for env chaining.
    , smPython :: MVar (Maybe SessionBackend)
    }

newSessionManager :: IO SessionManager
newSessionManager =
    SessionManager
        <$> newMVar Nothing
        <*> newMVar Nothing
        <*> newMVar Nothing

getHaskellSession :: SessionManager -> IO (Maybe SessionBackend)
getHaskellSession = readMVar . smHaskell

setHaskellSession :: SessionManager -> Maybe SessionBackend -> IO ()
setHaskellSession sm val = modifyMVar_ (smHaskell sm) (\_ -> pure val)

modifyHaskellSession ::
    SessionManager -> (Maybe SessionBackend -> IO (Maybe SessionBackend)) -> IO ()
modifyHaskellSession sm = modifyMVar_ (smHaskell sm)

getLeanSession :: SessionManager -> IO (Maybe LeanSession)
getLeanSession = readMVar . smLean

modifyLeanSession ::
    SessionManager -> (Maybe LeanSession -> IO (Maybe LeanSession, a)) -> IO a
modifyLeanSession sm = modifyMVar (smLean sm)

getPythonSession :: SessionManager -> IO (Maybe SessionBackend)
getPythonSession = readMVar . smPython

setPythonSession :: SessionManager -> Maybe SessionBackend -> IO ()
setPythonSession sm val = modifyMVar_ (smPython sm) (\_ -> pure val)

modifyPythonSession ::
    SessionManager -> (Maybe SessionBackend -> IO (Maybe SessionBackend, a)) -> IO a
modifyPythonSession sm = modifyMVar (smPython sm)
