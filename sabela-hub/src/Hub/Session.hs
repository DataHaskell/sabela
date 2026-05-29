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
    cleanupByKey,
    purgeReattachPlaceholders,
    reattachSessions,
    logHub,
    sessionKeyText,
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
    { smSessions :: TVar (Map SessionKey Session)
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

{- | Look up a session by session ID. Updates lastActivity if found.
'ReattachPlaceholder' entries live under a different constructor of
'SessionKey' so a forged cookie cannot adopt one — the safety guard is
now structural (formerly a "reattach:"-prefix runtime check).
-}
lookupBySessionId :: SessionManager -> SessionId -> IO (Maybe Session)
lookupBySessionId sm sid = do
    let key = UserSession sid
    now <- getCurrentTime
    atomically $ do
        sessions <- readTVar (smSessions sm)
        case Map.lookup key sessions of
            Just sess -> do
                let sess' = sess{sessionLastActivity = now}
                modifyTVar' (smSessions sm) $ Map.insert key sess'
                pure (Just sess')
            Nothing -> pure Nothing

-- | Insert a new session mapping.
insertSession :: SessionManager -> SessionKey -> Session -> IO ()
insertSession sm key sess =
    atomically $ modifyTVar' (smSessions sm) $ Map.insert key sess

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
                , sessionKind = Authed
                , sessionIdleOverride = Nothing
                }
    insertSession sm (UserSession sid) placeholder
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
    SReady ip -> pure (Right (unTaskIp ip))
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
                        , sessionKind = Authed
                        , sessionIdleOverride = Nothing
                        }
            insertSession sm (UserSession sid) sess
            -- The container is per-user but sessions are per-SessionId; clear
            -- any reattach placeholder for this container so the real session
            -- (not the placeholder) drives its idle/liveness tracking.
            purgeReattachPlaceholders sm taskId
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
                pure $ sessionTaskId <$> Map.lookup (UserSession sid) sessions
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
                                        (UserSession sid)
                            logHub $ "Task ready at " <> unTaskIp ip
                            pure (Right (unTaskIp ip))
                        Right TaskStopped -> do
                            cleanupSession sm sid
                            pure (Left "Task stopped unexpectedly")
                        Right TaskPending -> do
                            threadDelay 5_000_000
                            go (n + 1)

cleanupSession :: SessionManager -> SessionId -> IO ()
cleanupSession sm sid = cleanupByKey sm (UserSession sid)

{- | Drop any session (real or placeholder) from the manager and stop its
backing container. The reaper uses this to clean up by 'SessionKey' so
both kinds of entry get the same idle-timeout treatment.
-}
cleanupByKey :: SessionManager -> SessionKey -> IO ()
cleanupByKey sm key = do
    mSess <- atomically $ do
        sessions <- readTVar (smSessions sm)
        let mSess = Map.lookup key sessions
        modifyTVar' (smSessions sm) $ Map.delete key
        pure mSess
    case mSess of
        Just sess -> do
            let cfg = hcTaskConfig (smConfig sm)
            _ <-
                try $ ebStopTask (smEcs sm) cfg (sessionTaskId sess) ::
                    IO (Either SomeException ())
            logHub $ "Stopped task " <> taskIdText (sessionTaskId sess)
        Nothing -> pure ()

listSessions :: SessionManager -> IO (Map SessionKey Session)
listSessions sm = readTVarIO (smSessions sm)

{- | Drop the reattach placeholder (see 'reattachSessions') for the given
task. Real sessions are left intact, and the container is not stopped —
the adopting session still uses it. Constant-time delete now that the
placeholder lives under a distinct 'SessionKey' constructor.
-}
purgeReattachPlaceholders :: SessionManager -> TaskId -> IO ()
purgeReattachPlaceholders sm tid =
    atomically $
        modifyTVar' (smSessions sm) $
            Map.delete (ReattachPlaceholder tid)

{- | Rebuild tracked sessions from containers already running at hub startup,
instead of reaping them. Keyed by a deterministic @reattach:<name>@ id so the
idle reaper and liveness reconciler manage them; a returning user re-auths and
'createSession' adopts the same container (and 'purgeOtherSessionsWithTask'
clears this placeholder). Intended for the Docker backend, where containers
outlive the hub process.
-}
reattachSessions :: SessionManager -> IO ()
reattachSessions sm = do
    let cfg = hcTaskConfig (smConfig sm)
    eTasks <-
        try (ebListRunningTasks (smEcs sm) cfg) ::
            IO (Either SomeException [TaskId])
    case eTasks of
        Left e -> logHub $ "Reattach skipped (list failed): " <> T.pack (show e)
        Right tasks -> do
            now <- getCurrentTime
            mapM_ (reattachOne now) tasks
            logHub $
                "Reattached " <> T.pack (show (length tasks)) <> " container(s)"
  where
    reattachOne now tid@(TaskId name) =
        insertSession sm (ReattachPlaceholder tid) $
            Session
                { sessionTaskId = tid
                , sessionState = SReady (TaskIp name)
                , sessionLastActivity = now
                , sessionUserId = UserId ""
                , sessionKind = Authed
                , sessionIdleOverride = Nothing
                }

taskIdText :: TaskId -> Text
taskIdText (TaskId t) = t

userIdText :: UserId -> Text
userIdText (UserId t) = t

{- | Render a 'SessionKey' for logs. Real sessions print their cookie ID;
placeholders print @reattach:<task>@ so the existing log shape stays
recognisable. The wire/in-cookie strings are unaffected.
-}
sessionKeyText :: SessionKey -> Text
sessionKeyText (UserSession (SessionId t)) = t
sessionKeyText (ReattachPlaceholder (TaskId t)) = "reattach:" <> t

logHub :: Text -> IO ()
logHub msg = hPutStrLn stderr $ "[hub] " ++ T.unpack msg
