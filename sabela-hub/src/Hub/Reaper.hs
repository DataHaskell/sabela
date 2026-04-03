{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Hub.Reaper (
    startReaper,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
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
    loop sm

reapIdle :: SessionManager -> IO ()
reapIdle sm = do
    now <- getCurrentTime
    let timeout = hcIdleTimeout (smConfig sm)
    sessions <- readTVarIO (smSessions sm)
    let idle =
            Map.keys $
                Map.filter
                    ( \sess ->
                        sessionState sess /= SStopping
                            && diffUTCTime now (sessionLastActivity sess) > timeout
                    )
                    sessions
    mapM_ (reapOne sm) idle

reapOne :: SessionManager -> SessionId -> IO ()
reapOne sm sid = do
    hPutStrLn stderr $ "[hub] Reaping idle session " ++ sidLabel sid
    cleanupSession sm sid
  where
    sidLabel (SessionId s) = T.unpack (T.take 8 s) ++ "..."
