{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hub.Session (
    SessionManager (..),
    newSessionManager,
    getOrCreateSession,
    startSessionAsync,
    lookupBySessionId,
    insertSession,
    listSessions,
    cleanupSession,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Hub.Types
import System.IO (hPutStrLn, stderr)

data SessionManager = SessionManager
    { smSessions :: TVar (Map SessionId Session)
    , smEcs :: EcsBackend
    , smConfig :: HubConfig
    }

newSessionManager :: EcsBackend -> HubConfig -> IO SessionManager
newSessionManager ecs cfg = do
    sessions <- newTVarIO Map.empty
    pure
        SessionManager
            { smSessions = sessions
            , smEcs = ecs
            , smConfig = cfg
            }

-- | Look up a session by session ID. Updates lastActivity if found.
lookupBySessionId :: SessionManager -> SessionId -> IO (Maybe Session)
lookupBySessionId sm sid = do
    now <- getCurrentTime
    atomically $ do
        sessions <- readTVar (smSessions sm)
        case Map.lookup sid sessions of
            Just sess -> do
                let sess' = sess{sessionLastActivity = now}
                modifyTVar' (smSessions sm) $ Map.insert sid sess'
                pure (Just sess')
            Nothing -> pure Nothing

-- | Insert a new session mapping.
insertSession :: SessionManager -> SessionId -> Session -> IO ()
insertSession sm sid sess =
    atomically $ modifyTVar' (smSessions sm) $ Map.insert sid sess

{- | Start a session in the background. Inserts a SStarting entry immediately
so the session is visible to lookups, then spawns the task asynchronously.
-}
startSessionAsync :: SessionManager -> SessionId -> UserId -> IO ()
startSessionAsync sm sid uid = do
    now <- getCurrentTime
    let placeholder =
            Session
                { sessionTaskId = TaskId ""
                , sessionState = SStarting
                , sessionLastActivity = now
                , sessionUserId = uid
                }
    insertSession sm sid placeholder
    _ <- forkIO $ void $ createSession sm sid uid
    pure ()

-- | Get or create a session for a user. Returns the task IP.
getOrCreateSession ::
    SessionManager -> SessionId -> UserId -> IO (Either Text Text)
getOrCreateSession sm sid uid = do
    existing <- lookupBySessionId sm sid
    case existing of
        Just sess -> handleExisting sm sid sess
        Nothing -> createSession sm sid uid

handleExisting ::
    SessionManager -> SessionId -> Session -> IO (Either Text Text)
handleExisting sm sid sess = case sessionState sess of
    SReady ip -> pure (Right ip)
    SStarting -> waitForReady sm sid
    SStopping -> pure (Left "Session is shutting down, try again shortly")

createSession :: SessionManager -> SessionId -> UserId -> IO (Either Text Text)
createSession sm sid uid = do
    now <- getCurrentTime
    let cfg = hcTaskConfig (smConfig sm)
        ecs = smEcs sm
    result <- try $ ebRunTask ecs cfg uid
    case result of
        Left (e :: SomeException) ->
            pure $ Left ("Failed to start task: " <> T.pack (show e))
        Right taskId -> do
            let sess =
                    Session
                        { sessionTaskId = taskId
                        , sessionState = SStarting
                        , sessionLastActivity = now
                        , sessionUserId = uid
                        }
            insertSession sm sid sess
            logHub $ "Started task " <> taskIdText taskId <> " for " <> userIdText uid
            waitForReady sm sid

waitForReady :: SessionManager -> SessionId -> IO (Either Text Text)
waitForReady sm sid = go (0 :: Int)
  where
    maxAttempts = 60
    cfg = hcTaskConfig (smConfig sm)
    ecs = smEcs sm
    go n
        | n >= maxAttempts = do
            cleanupSession sm sid
            pure (Left "Timed out waiting for task to start")
        | otherwise = do
            mTaskId <- atomically $ do
                sessions <- readTVar (smSessions sm)
                pure $ sessionTaskId <$> Map.lookup sid sessions
            case mTaskId of
                Nothing -> pure (Left "Session was removed")
                Just taskId -> do
                    status <- try $ ebDescribeTask ecs cfg taskId
                    case status of
                        Left (e :: SomeException) -> do
                            logHub $ "Error polling task: " <> T.pack (show e)
                            threadDelay 5_000_000
                            go (n + 1)
                        Right (TaskRunning ip) -> do
                            now <- getCurrentTime
                            atomically $
                                modifyTVar' (smSessions sm) $
                                    Map.adjust
                                        ( \s ->
                                            s
                                                { sessionState = SReady ip
                                                , sessionLastActivity = now
                                                }
                                        )
                                        sid
                            logHub $ "Task ready at " <> ip
                            pure (Right ip)
                        Right TaskStopped -> do
                            cleanupSession sm sid
                            pure (Left "Task stopped unexpectedly")
                        Right TaskPending -> do
                            threadDelay 5_000_000
                            go (n + 1)

cleanupSession :: SessionManager -> SessionId -> IO ()
cleanupSession sm sid = do
    mSess <- atomically $ do
        sessions <- readTVar (smSessions sm)
        let mSess = Map.lookup sid sessions
        modifyTVar' (smSessions sm) $ Map.delete sid
        pure mSess
    case mSess of
        Just sess -> do
            let cfg = hcTaskConfig (smConfig sm)
            _ <-
                try $ ebStopTask (smEcs sm) cfg (sessionTaskId sess) ::
                    IO (Either SomeException ())
            logHub $ "Stopped task " <> taskIdText (sessionTaskId sess)
        Nothing -> pure ()

listSessions :: SessionManager -> IO (Map SessionId Session)
listSessions sm = readTVarIO (smSessions sm)

taskIdText :: TaskId -> Text
taskIdText (TaskId t) = t

userIdText :: UserId -> Text
userIdText (UserId t) = t

logHub :: Text -> IO ()
logHub msg = hPutStrLn stderr $ "[hub] " ++ T.unpack msg
