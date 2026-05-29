{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Hub.Reaper (
    startReaper,
    sweepOrphans,
    reapIdle,
    reconcileLiveness,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time (diffUTCTime, getCurrentTime)
import Hub.Session
import Hub.Types
import System.IO (hPutStrLn, stderr)

-- | Start a background thread that stops idle sessions.
startReaper :: SessionManager -> IO ()
startReaper sm = do
    _ <- forkIO $ loop sm
    pure ()

loop :: SessionManager -> IO ()
loop sm = do
    threadDelay 60_000_000 -- 1 minute
    reapIdle sm
    reconcileLiveness sm
    loop sm

reapIdle :: SessionManager -> IO ()
reapIdle sm = do
    now <- getCurrentTime
    let globalTimeout = hcIdleTimeout (smConfig sm)
        isIdle sess =
            sessionState sess /= SStopping
                && diffUTCTime now (sessionLastActivity sess)
                    > fromMaybe globalTimeout (sessionIdleOverride sess)
    sessions <- readTVarIO (smSessions sm)
    mapM_ (reapOne sm) (Map.keys (Map.filter isIdle sessions))

{- | Stop tracking sessions whose backend container has gone away (OOM-killed,
crashed, externally removed). Without this a dead container leaves a session
stuck in 'SReady' and the proxy returns 502s forever. Complements the orphan
sweep (which runs once at startup) by re-checking liveness on every tick.
-}
reconcileLiveness :: SessionManager -> IO ()
reconcileLiveness sm = do
    let cfg = hcTaskConfig (smConfig sm)
        ecs = smEcs sm
    sessions <- readTVarIO (smSessions sm)
    mapM_ (checkLive cfg ecs) (Map.toList sessions)
  where
    checkLive cfg ecs (key, sess) = case sessionState sess of
        SReady _ -> do
            r <-
                try (ebDescribeTask ecs cfg (sessionTaskId sess)) ::
                    IO (Either SomeException TaskStatus)
            case r of
                Right TaskStopped -> do
                    hPutStrLn stderr $
                        "[hub] Liveness: container gone, reaping " ++ keyLabel key
                    cleanupByKey sm key
                _ -> pure ()
        _ -> pure ()

reapOne :: SessionManager -> SessionKey -> IO ()
reapOne sm key = do
    hPutStrLn stderr $ "[hub] Reaping idle session " ++ keyLabel key
    cleanupByKey sm key

{- | Short, log-friendly label for a 'SessionKey'. Cookie IDs and reattach
task IDs are both truncated so the log stays readable.
-}
keyLabel :: SessionKey -> String
keyLabel = T.unpack . T.take 16 . sessionKeyText

{- | Stop any RUNNING ECS task in the configured cluster/family that is not
tracked in the session map. Intended to run once at startup so that tasks
which outlived a previous hub instance get cleaned up — the in-memory
session map is not persisted, so they would otherwise live forever.
-}
sweepOrphans :: SessionManager -> IO ()
sweepOrphans sm = do
    let cfg = hcTaskConfig (smConfig sm)
        ecs = smEcs sm
    eTasks <- try (ebListRunningTasks ecs cfg) :: IO (Either SomeException [TaskId])
    case eTasks of
        Left e ->
            hPutStrLn stderr $ "[hub] Orphan sweep skipped (list failed): " ++ show e
        Right taskIds -> do
            tracked <- trackedTaskIds sm
            let orphans = filter (`Set.notMember` tracked) taskIds
            hPutStrLn stderr $
                "[hub] Orphan sweep: "
                    ++ show (length taskIds)
                    ++ " running, "
                    ++ show (length orphans)
                    ++ " orphan(s) to stop"
            mapM_ (stopOrphan ecs cfg) orphans

trackedTaskIds :: SessionManager -> IO (Set.Set TaskId)
trackedTaskIds sm = do
    sessions <- readTVarIO (smSessions sm)
    pure $ Set.fromList [sessionTaskId s | s <- Map.elems sessions]

stopOrphan :: EcsBackend -> TaskConfig -> TaskId -> IO ()
stopOrphan ecs cfg tid@(TaskId arn) = do
    hPutStrLn stderr $ "[hub] Stopping orphan task " ++ T.unpack (T.takeEnd 12 arn)
    r <- try (ebStopTask ecs cfg tid) :: IO (Either SomeException ())
    case r of
        Left e -> hPutStrLn stderr $ "[hub] Failed to stop orphan: " ++ show e
        Right () -> pure ()
