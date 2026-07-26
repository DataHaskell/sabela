{-# LANGUAGE OverloadedStrings #-}

module Sabela.Session where

import Control.Concurrent (
    MVar,
    putMVar,
    tryReadMVar,
    withMVar,
 )
import Control.Exception (SomeException, bracket_, try)
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
    , sessErrBuf :: IORef [Text]
    , sessCounter :: IORef Int
    , sessConfig :: SessionConfig
    , sessErrCallback :: IORef (Text -> IO ())
    , sessBusy :: IORef Bool
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
detectJsonDiagnostics = do
    ghc <- fromMaybe "ghc" <$> lookupEnv "GHC"
    res <-
        try (readProcessWithExitCode ghc ["--numeric-version"] "") ::
            IO (Either SomeException (ExitCode, String, String))
    pure $ case res of
        Right (ExitSuccess, out, _) -> versionAtLeast [9, 10] (parseVersion out)
        _ -> False

parseVersion :: String -> [Int]
parseVersion s =
    [ n
    | p <- T.splitOn "." (T.pack (takeWhile (\c -> isDigit c || c == '.') s))
    , Right (n, _) <- [TR.decimal p]
    ]

versionAtLeast :: [Int] -> [Int] -> Bool
versionAtLeast req v = take (length req) (v ++ repeat 0) >= req

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
    withMVar (sessLock sess) $ \_ ->
        runBlockStreamingUnlockedWithTimeout budgetUs sess block onLine

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
            bracket_ (setBusy sess True) (setBusy sess False) $
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

interruptIfBusy :: Session -> IO ()
interruptIfBusy sess = do
    busy <- readIORef (sessBusy sess)
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

setBusy :: Session -> Bool -> IO ()
setBusy sess = writeIORef (sessBusy sess)

isBusy :: Session -> IO Bool
isBusy sess = isNothing <$> tryReadMVar (sessLock sess)

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
