{-# LANGUAGE OverloadedStrings #-}

module Sabela.Session where

import Control.Concurrent (MVar, withMVar)
import Control.Concurrent.STM (TVar, atomically, readTVarIO, writeTVar)
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
import Data.Time (UTCTime, getCurrentTime)
import Sabela.Session.Drain (
    DrainResult (..),
    discardUntilMarker,
    drainUntilMarker,
 )
import Sabela.Session.GhcProbe (jsonDiagnosticsSupported)
import Sabela.Session.Proc (
    ProcSession (..),
    destroySession,
    escalateKill,
    interruptGroup,
 )
import Sabela.Session.Reader (OutQueue, markerNonceBase, mkMarkerText)
import Sabela.Session.Timeout (
    readTimeoutConfig,
    tcExecutionUs,
    tcResyncUs,
    timedOutKilledMessage,
    timedOutMessage,
 )
import System.IO (Handle, hFlush, hPutStrLn)
import System.Process (
    ProcessHandle,
    getProcessExitCode,
 )
import System.Timeout (timeout)

newtype Marker = Marker Text

markerText :: Marker -> Text
markerText (Marker t) = t

{- | Who holds 'sessLock'. Queries take it too (they drain the same queue), so
"the lock is held" no longer means "a cell is running" — see 'isBusy'.
-}
data LockOwner = OwnerRun | OwnerQuery
    deriving (Eq, Show)

data Session = Session
    { sessProcSess :: ProcSession
    , sessLock :: MVar ()
    , sessQueryLock :: MVar ()
    , sessLockOwner :: TVar (Maybe LockOwner)
    , sessErrBuf :: IORef [Text]
    , sessCounter :: IORef Int
    , sessConfig :: SessionConfig
    , sessErrCallback :: IORef (Text -> IO ())
    , sessNonce :: Int
    , sessLastInterruptTime :: IORef (Maybe UTCTime)
    , sessionGen :: IORef Int
    , sessBaselineBindings :: IORef [Text]
    }

data SessionConfig = SessionConfig
    { scProjectDir :: FilePath
    , scWorkDir :: FilePath
    , scCabalStoreDir :: Maybe FilePath
    , scExecutionTimeoutUs :: Int
    , scResyncTimeoutUs :: Int
    , scJsonDiagnostics :: Bool
    }
    deriving (Eq, Show)

mkSessionConfig :: FilePath -> FilePath -> IO SessionConfig
mkSessionConfig projDir workDir = do
    tc <- readTimeoutConfig
    jsonDiag <- detectJsonDiagnostics
    pure
        SessionConfig
            { scProjectDir = projDir
            , scWorkDir = workDir
            , scCabalStoreDir = Nothing
            , scExecutionTimeoutUs = tcExecutionUs tc
            , scResyncTimeoutUs = tcResyncUs tc
            , scJsonDiagnostics = jsonDiag
            }

detectJsonDiagnostics :: IO Bool
detectJsonDiagnostics = jsonDiagnosticsSupported

sessStdin :: Session -> Handle
sessStdin = psStdin . sessProcSess

sessProc :: Session -> ProcessHandle
sessProc = psProc . sessProcSess

sessLines :: Session -> OutQueue
sessLines = psQueue . sessProcSess

executionTimeoutUs :: Session -> Int
executionTimeoutUs = scExecutionTimeoutUs . sessConfig

resyncTimeoutUs :: Session -> Int
resyncTimeoutUs = scResyncTimeoutUs . sessConfig

runBlock :: Session -> Text -> IO (Text, Text)
runBlock sess block = runBlockStreaming sess block (\_ -> pure ())

runBlockWithTimeout :: Int -> Session -> Text -> IO (Text, Text)
runBlockWithTimeout budgetUs sess block =
    runBlockStreamingWithTimeout budgetUs sess block (\_ -> pure ())

runBlockStreaming :: Session -> Text -> (Text -> IO ()) -> IO (Text, Text)
runBlockStreaming sess =
    runBlockStreamingWithTimeout (executionTimeoutUs sess) sess

runBlockStreamingWithTimeout ::
    Int -> Session -> Text -> (Text -> IO ()) -> IO (Text, Text)
runBlockStreamingWithTimeout budgetUs sess block onLine =
    withRunLock sess $
        runBlockStreamingUnlockedWithTimeout budgetUs sess block onLine

-- | Run a cell: the exclusive holder of the kernel.
withRunLock :: Session -> IO a -> IO a
withRunLock sess = withOwnedLock sess OwnerRun

{- | Run an editor/agent query. A query writes to stdin and drains the same
output queue as a cell, so it must hold the run lock as well, outermost —
two concurrent drains steal each other's markers.
-}
withQueryLocks :: Session -> IO a -> IO a
withQueryLocks sess act =
    withOwnedLock sess OwnerQuery (withMVar (sessQueryLock sess) (const act))

withOwnedLock :: Session -> LockOwner -> IO a -> IO a
withOwnedLock sess owner act =
    withMVar (sessLock sess) $ \_ ->
        bracket_ (setOwner (Just owner)) (setOwner Nothing) act
  where
    setOwner = atomically . writeTVar (sessLockOwner sess)

runBlockStreamingUnlockedWithTimeout ::
    Int -> Session -> Text -> (Text -> IO ()) -> IO (Text, Text)
runBlockStreamingUnlockedWithTimeout budgetUs sess block onLine = do
    checkProcessAlive sess
    resetErrorBuffer sess
    mk <- getMarker sess
    mResult <-
        timeout budgetUs $ do
            mapM_ (sendRaw sess . T.unpack) (T.lines block)
            placeMarker sess mk
            drainUntilMarker (sessLines sess) (markerText mk) onLine
    finishRunWithTimeout budgetUs sess mResult

finishRun :: Session -> Maybe DrainResult -> IO (Text, Text)
finishRun sess = finishRunWithTimeout (executionTimeoutUs sess) sess

finishRunWithTimeout :: Int -> Session -> Maybe DrainResult -> IO (Text, Text)
finishRunWithTimeout _budgetUs sess (Just (DrainOk out)) = do
    errLines <- readErrorBuffer sess
    pure (out, errLines)
finishRunWithTimeout _budgetUs sess (Just (DrainEof _)) = do
    destroySession (sessProcSess sess)
    ioError (userError "GHCi session ended unexpectedly mid-cell")
finishRunWithTimeout budgetUs sess Nothing = do
    interruptSessionRaw sess
    mk2 <- getMarker sess
    synced <-
        timeout (resyncTimeoutUs sess) $ do
            placeMarker sess mk2
            discardUntilMarker (sessLines sess) (markerText mk2)
    case synced of
        Just True -> do
            errLines <- readErrorBuffer sess
            pure
                ( ""
                , errLines <> timedOutMessage budgetUs
                )
        _ -> killAndRespawnWithTimeout budgetUs sess

killAndRespawn :: Session -> IO (Text, Text)
killAndRespawn sess = killAndRespawnWithTimeout (executionTimeoutUs sess) sess

killAndRespawnWithTimeout :: Int -> Session -> IO (Text, Text)
killAndRespawnWithTimeout budgetUs sess = do
    escalateKill (sessProcSess sess)
    destroySession (sessProcSess sess)
    ioError
        ( userError
            ( T.unpack
                (T.strip (timedOutKilledMessage budgetUs))
            )
        )

interruptSessionRaw :: Session -> IO ()
interruptSessionRaw = interruptGroup . sessProcSess

{- | Interrupt across the /whole/ run, not just the drain, so a signal arriving
mid-write or during resync is no longer dropped. Stays conditional: it reaches
the process group holding cabal and ghc, so an idle interrupt breaks the next.
-}
interruptIfBusy :: Session -> IO ()
interruptIfBusy sess = do
    busy <- isBusy sess
    when busy $ do
        interruptSessionRaw sess
        markInterrupt sess

markInterrupt :: Session -> IO ()
markInterrupt sess = do
    now <- getCurrentTime
    writeIORef (sessLastInterruptTime sess) (Just now)

isRequestStale :: Session -> UTCTime -> IO Bool
isRequestStale sess reqTime = do
    mLast <- readIORef (sessLastInterruptTime sess)
    pure $ maybe False (reqTime <) mLast

{- | Is a /cell/ running? Not merely "the run lock is held": an editor query
holds it too, and a keystroke must not make the kernel read as executing.
-}
isBusy :: Session -> IO Bool
isBusy sess = (== Just OwnerRun) <$> readTVarIO (sessLockOwner sess)

readSessionGen :: Session -> IO Int
readSessionGen = readIORef . sessionGen

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
getMarker sess = do
    n <- atomicModifyIORef' (sessCounter sess) (\i -> (i + 1, i))
    pure (Marker (mkMarkerText (sessNonce sess * markerNonceBase + n)))

placeMarker :: Session -> Marker -> IO ()
placeMarker sess (Marker mk) =
    sendRaw sess $
        ":cmd ((Prelude.>>) (Prelude.putStrLn "
            ++ show (T.unpack mk)
            ++ ") (Prelude.pure \"\"))"

resetErrorBuffer :: Session -> IO ()
resetErrorBuffer sess = atomicModifyIORef' (sessErrBuf sess) (const ([], ()))

readErrorBuffer :: Session -> IO Text
readErrorBuffer sess = fmap (T.strip . T.unlines . reverse) (readIORef (sessErrBuf sess))

clearErrCallback :: Session -> IO ()
clearErrCallback sess = atomicWriteIORef (sessErrCallback sess) (\_ -> pure ())
