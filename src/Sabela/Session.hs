{-# LANGUAGE OverloadedStrings #-}

{- | Long-lived GHCi session state and the cell entry points: marker
placement, drains, interrupts, and the timeout→interrupt→resync→destroy
pipeline. Spawn/teardown live in 'Sabela.Session.Proc' and ".Process".
-}
module Sabela.Session where

import Control.Concurrent (
    MVar,
    putMVar,
    tryReadMVar,
    tryTakeMVar,
    withMVar,
 )
import Control.Exception (SomeException, bracket_, finally, mask, try)
import Control.Monad (when)
import Data.Char (isDigit)
import Data.IORef (
    IORef,
    atomicModifyIORef',
    atomicWriteIORef,
    readIORef,
    writeIORef,
 )
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Time (UTCTime, getCurrentTime)
import Sabela.Session.Drain (
    DrainResult (..),
    discardUntilMarker,
    drainUntilMarker,
 )
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
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (Handle, hFlush, hPutStrLn)
import System.Process (
    ProcessHandle,
    getProcessExitCode,
    readProcessWithExitCode,
 )
import System.Timeout (timeout)

newtype Marker = Marker Text

markerText :: Marker -> Text
markerText (Marker t) = t

data Session = Session
    { sessProcSess :: ProcSession
    , sessLock :: MVar ()
    , sessQueryLock :: MVar ()
    -- ^ Serialises introspection queries off the cell run-lock (case 20).
    , sessErrBuf :: IORef [Text]
    , sessCounter :: IORef Int
    , sessConfig :: SessionConfig
    , sessErrCallback :: IORef (Text -> IO ())
    , sessBusy :: IORef Bool
    , sessNonce :: Int
    , sessLastInterruptTime :: IORef (Maybe UTCTime)
    -- ^ When the kernel last actually signalled an interrupt (case 25).
    , sessionGen :: IORef Int
    -- ^ Generation tag; bumps on restart so clients can discard stale results.
    , sessBaselineBindings :: IORef [Text]
    -- ^ Prelude-injected bindings, snapshotted at warmup so 'queryBindings' scrubs them.
    }

data SessionConfig = SessionConfig
    { scProjectDir :: FilePath
    , scWorkDir :: FilePath
    , scExecutionTimeoutUs :: Int
    -- ^ Per-cell execution budget (microseconds), from 'mkSessionConfig'.
    , scResyncTimeoutUs :: Int
    -- ^ Post-timeout resync window (microseconds).
    , scJsonDiagnostics :: Bool
    -- ^ Session GHC supports @-fdiagnostics-as-json@ (GHC ≥ 9.8).
    }
    deriving (Show, Eq)

{- | Build a 'SessionConfig', reading the execution budget from
@SABELA_CELL_TIMEOUT_SECONDS@ (default 120s). The single chokepoint every
real caller routes through, so the live session and exports never drift.
-}
mkSessionConfig :: FilePath -> FilePath -> IO SessionConfig
mkSessionConfig projDir workDir = do
    tc <- readTimeoutConfig
    jsonDiag <- detectJsonDiagnostics
    pure
        SessionConfig
            { scProjectDir = projDir
            , scWorkDir = workDir
            , scExecutionTimeoutUs = tcExecutionUs tc
            , scResyncTimeoutUs = tcResyncUs tc
            , scJsonDiagnostics = jsonDiag
            }

{- | Does the session GHC support @-fdiagnostics-as-json@ (added in GHC 9.8.1)?
Probe the resolved compiler — the @GHC@ env override 'ghciProcessSpec' honours,
else @ghc@ — once at config time; assume no on any failure (textual fallback).
-}
detectJsonDiagnostics :: IO Bool
detectJsonDiagnostics = do
    ghc <- fromMaybe "ghc" <$> lookupEnv "GHC"
    res <-
        try (readProcessWithExitCode ghc ["--numeric-version"] "") ::
            IO (Either SomeException (ExitCode, String, String))
    pure $ case res of
        Right (ExitSuccess, out, _) -> versionAtLeast [9, 10] (parseVersion out)
        _ -> False

-- | The numeric components of a @ghc --numeric-version@ string, e.g. [9,12,2].
parseVersion :: String -> [Int]
parseVersion s =
    [ n
    | p <- T.splitOn "." (T.pack (takeWhile (\c -> isDigit c || c == '.') s))
    , Right (n, _) <- [TR.decimal p]
    ]

-- | Lexicographic @>=@ on version components, padding the candidate with zeros.
versionAtLeast :: [Int] -> [Int] -> Bool
versionAtLeast req v = take (length req) (v ++ repeat 0) >= req

sessStdin :: Session -> Handle
sessStdin = psStdin . sessProcSess

sessProc :: Session -> ProcessHandle
sessProc = psProc . sessProcSess

sessLines :: Session -> OutQueue
sessLines = psQueue . sessProcSess

-- | The session's configured per-cell execution budget, in microseconds.
executionTimeoutUs :: Session -> Int
executionTimeoutUs = scExecutionTimeoutUs . sessConfig

-- | How long the post-timeout resync waits for its fresh marker.
resyncTimeoutUs :: Session -> Int
resyncTimeoutUs = scResyncTimeoutUs . sessConfig

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
        timeout (executionTimeoutUs sess) $ do
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
        timeout (resyncTimeoutUs sess) $ do
            placeMarker sess mk2
            discardUntilMarker (sessLines sess) (markerText mk2)
    case synced of
        Just True -> do
            errLines <- readErrorBuffer sess
            pure
                ( ""
                , errLines <> timedOutMessage (executionTimeoutUs sess)
                )
        _ -> killAndRespawn sess

{- | The interrupt rung was ignored within the resync window: escalate the
kill ladder (INT → TERM → KILL via the portable group wrappers), reap, and
surface the killed-and-will-respawn notice. The handler layer catches this,
broadcasts the crash, and the next run spawns a fresh idle kernel — so the
kernel self-recovers with no human (stress cases 8–10, 17).
-}
killAndRespawn :: Session -> IO (Text, Text)
killAndRespawn sess = do
    escalateKill (sessProcSess sess)
    destroySession (sessProcSess sess)
    ioError
        ( userError
            ( T.unpack
                (T.strip (timedOutKilledMessage (executionTimeoutUs sess)))
            )
        )

-- | Raw group SIGINT, used by the internal timeout path unconditionally.
interruptSessionRaw :: Session -> IO ()
interruptSessionRaw = interruptGroup . sessProcSess

{- | Public interrupt: only signals while a cell run is actually
draining, so an idle click can never reach the interpreter's group.
Records the instant it actually signals, so a request stamped before
this interrupt can be dropped as stale ('isRequestStale').
-}
interruptIfBusy :: Session -> IO ()
interruptIfBusy sess = do
    busy <- readIORef (sessBusy sess)
    when busy $ do
        interruptSessionRaw sess
        markInterrupt sess

-- | Record that the kernel just interrupted, stamping the current instant.
markInterrupt :: Session -> IO ()
markInterrupt sess = do
    now <- getCurrentTime
    writeIORef (sessLastInterruptTime sess) (Just now)

{- | A request is stale when it was stamped before the kernel's last
interrupt — its queued run must not fire after the user interrupted.
-}
isRequestStale :: Session -> UTCTime -> IO Bool
isRequestStale sess reqTime = do
    mLast <- readIORef (sessLastInterruptTime sess)
    pure $ maybe False (reqTime <) mLast

setBusy :: Session -> Bool -> IO ()
setBusy sess = writeIORef (sessBusy sess)

{- | Lock-free busy check: the run-lock is empty while 'runBlockStreaming'
holds it, so an empty lock ('tryReadMVar' returns 'Nothing') means a cell or
query is running. Answers without ever blocking, so a status probe can run
while a cell is mid-flight.
-}
isBusy :: Session -> IO Bool
isBusy sess = isNothing <$> tryReadMVar (sessLock sess)

{- | Outcome of an atomic admission attempt at the run-lock: 'Ran' carries the
action's result (the slot was free and we held it for the run); 'Busy' carries
the cell id already holding the lock (or our own candidate if no holder was
recorded). The sum tag is the atomic decision, mapped to the busy 'ToolOutcome'
by the dispatcher, not a re-read of the @running@ boolean.
-}
data Admission a
    = Ran a
    | Busy {running :: !Int}
    deriving (Eq, Show)

{- | Atomic admission: a SINGLE 'tryTakeMVar' on the run-lock decides busy and
acquires the slot in one step — no check-then-acquire gap where two callers
both observe \"free\" and then both stack behind the lock. On a free lock it
records the candidate id, runs the action, and restores the lock even on
exception; on a held lock it reports 'Busy' with the recorded holder id without
ever blocking. A lone caller on a free lock always 'Ran' and never deadlocks.
-}
admit :: MVar () -> IORef (Maybe Int) -> Int -> IO a -> IO (Admission a)
admit lock holderRef cid act = mask $ \restore -> do
    got <- tryTakeMVar lock
    case got of
        Nothing -> do
            held <- readIORef holderRef
            pure (Busy (fromMaybe cid held))
        Just () -> do
            writeIORef holderRef (Just cid)
            let release = writeIORef holderRef Nothing >> putMVar lock ()
            r <- restore act `finally` release
            pure (Ran r)

{- | The live backend generation. Born at @firstSessionGen@; a restart
seeds a strictly-higher one so a client can discard a result tagged with an
older generation (stress case 36).
-}
readSessionGen :: Session -> IO Int
readSessionGen = readIORef . sessionGen

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
getMarker sess = do
    n <- atomicModifyIORef' (sessCounter sess) (\i -> (i + 1, i))
    pure (Marker (mkMarkerText (sessNonce sess * markerNonceBase + n)))

placeMarker :: Session -> Marker -> IO ()
placeMarker sess (Marker mk) = sendRaw sess $ "putStrLn " ++ show (T.unpack mk)

resetErrorBuffer :: Session -> IO ()
resetErrorBuffer sess = atomicModifyIORef' (sessErrBuf sess) (const ([], ()))

readErrorBuffer :: Session -> IO Text
readErrorBuffer sess = fmap (T.strip . T.unlines . reverse) (readIORef (sessErrBuf sess))

clearErrCallback :: Session -> IO ()
clearErrCallback sess = atomicWriteIORef (sessErrCallback sess) (\_ -> pure ())
