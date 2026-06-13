{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Long-lived GHCi session state and the cell entry points: marker
placement, drains, interrupts, and the timeout→interrupt→resync→destroy
pipeline. Spawn/teardown live in 'Sabela.Session.Proc' and ".Process".
-}
module Sabela.Session where

import Control.Concurrent (MVar, withMVar)
import Control.Exception (bracket_)
import Control.Monad (when)
import Data.IORef (
    IORef,
    atomicModifyIORef',
    atomicWriteIORef,
    readIORef,
    writeIORef,
 )
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Session.Drain (
    DrainResult (..),
    discardUntilMarker,
    drainUntilMarker,
 )
import Sabela.Session.Proc (
    ProcSession (..),
    destroySession,
    interruptGroup,
 )
import Sabela.Session.Reader (OutQueue, mkMarkerText)
import System.IO (Handle, hFlush, hPutStrLn)
import System.Process (ProcessHandle, getProcessExitCode)
import System.Timeout (timeout)

newtype Marker = Marker Text

markerText :: Marker -> Text
markerText (Marker t) = t

data Session = Session
    { sessProcSess :: ProcSession
    , sessLock :: MVar ()
    , sessErrBuf :: IORef [Text]
    , sessCounter :: IORef Int
    , sessConfig :: SessionConfig
    , sessErrCallback :: IORef (Text -> IO ())
    , sessBusy :: IORef Bool
    }

data SessionConfig = SessionConfig
    { scProjectDir :: FilePath
    , scWorkDir :: FilePath
    }
    deriving (Show, Eq)

sessStdin :: Session -> Handle
sessStdin = psStdin . sessProcSess

sessProc :: Session -> ProcessHandle
sessProc = psProc . sessProcSess

sessLines :: Session -> OutQueue
sessLines = psQueue . sessProcSess

executionTimeoutUs :: Int
executionTimeoutUs = 120 * 1000000

-- | How long the post-timeout resync waits for its fresh marker.
resyncTimeoutUs :: Int
resyncTimeoutUs = 5 * 1000000

runBlock :: Session -> Text -> IO (Text, Text)
runBlock sess block = runBlockStreaming sess block (\_ -> pure ())

{- | Run a cell's rendered block. On timeout: group SIGINT, then resync
on a fresh marker; if the session stays silent it is destroyed. EOF
mid-run (dead interpreter) also destroys and surfaces as a crash.
-}
runBlockStreaming :: Session -> Text -> (Text -> IO ()) -> IO (Text, Text)
runBlockStreaming sess block onLine = withMVar (sessLock sess) $ \_ -> do
    checkProcessAlive sess
    resetErrorBuffer sess
    mk <- getMarker sess
    mResult <-
        timeout executionTimeoutUs $ do
            mapM_ (sendRaw sess . T.unpack) (T.lines block)
            placeMarker sess mk
            bracket_ (setBusy sess True) (setBusy sess False) $
                drainUntilMarker (sessLines sess) (markerText mk) onLine
    finishRun sess mResult

finishRun :: Session -> Maybe DrainResult -> IO (Text, Text)
finishRun sess (Just (DrainOk out)) = do
    errLines <- readErrorBuffer sess
    pure (out, errLines)
finishRun sess (Just (DrainEof _)) = do
    destroySession (sessProcSess sess)
    ioError (userError "GHCi session ended unexpectedly mid-cell")
finishRun sess Nothing = do
    interruptSessionRaw sess
    mk2 <- getMarker sess
    synced <-
        timeout resyncTimeoutUs $ do
            placeMarker sess mk2
            discardUntilMarker (sessLines sess) (markerText mk2)
    case synced of
        Just True -> do
            errLines <- readErrorBuffer sess
            pure
                ( ""
                , errLines
                    <> "\n*** Execution timed out after 120 seconds; \
                       \computation interrupted ***"
                )
        _ -> do
            destroySession (sessProcSess sess)
            ioError
                ( userError
                    "Cell timed out and the session did not respond \
                    \to interrupt; session killed"
                )

-- | Raw group SIGINT, used by the internal timeout path unconditionally.
interruptSessionRaw :: Session -> IO ()
interruptSessionRaw = interruptGroup . sessProcSess

{- | Public interrupt: only signals while a cell run is actually
draining, so an idle click can never reach the interpreter's group.
-}
interruptIfBusy :: Session -> IO ()
interruptIfBusy sess = do
    busy <- readIORef (sessBusy sess)
    when busy $ interruptSessionRaw sess

setBusy :: Session -> Bool -> IO ()
setBusy sess = writeIORef (sessBusy sess)

{- | Reap-probe for a self-exited leader. This reaps outside the
kill-lock; 'destroySession' tolerates that by never signalling a pgid
once the probe there reports an exit.
-}
checkProcessAlive :: Session -> IO ()
checkProcessAlive sess = do
    mExit <- getProcessExitCode (sessProc sess)
    case mExit of
        Nothing -> pure ()
        Just code ->
            ioError $
                userError $
                    "GHCi process exited with " ++ show code

sendRaw :: Session -> String -> IO ()
sendRaw sess cmd = do
    hPutStrLn (sessStdin sess) cmd
    hFlush (sessStdin sess)

getMarker :: Session -> IO Marker
getMarker Session{sessCounter} = do
    n <- atomicModifyIORef' sessCounter (\i -> (i + 1, i))
    pure (Marker (mkMarkerText n))

placeMarker :: Session -> Marker -> IO ()
placeMarker sess (Marker mk) = sendRaw sess $ "putStrLn " ++ show (T.unpack mk)

resetErrorBuffer :: Session -> IO ()
resetErrorBuffer sess = atomicModifyIORef' (sessErrBuf sess) (const ([], ()))

readErrorBuffer :: Session -> IO Text
readErrorBuffer sess = fmap (T.strip . T.unlines . reverse) (readIORef (sessErrBuf sess))

clearErrCallback :: Session -> IO ()
clearErrCallback sess = atomicWriteIORef (sessErrCallback sess) (\_ -> pure ())
