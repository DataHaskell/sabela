{-# LANGUAGE OverloadedStrings #-}

module Sabela.Session.Process (
    newSession,
    newSessionStreaming,
    resetSession,
    closeSession,
    ghciBackend,
    firstSessionGen,
    bumpSessionGen,
    ghciArgs,
    rtsGhcOptions,
    ghciProcessSpec,
    buildSessionState,
    buildSessionStateGen,
    mkSessionNonce,
    initializeGhci,
    startupErrorMessage,
    clearGhciPrompt,
    sendQuit,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newMVar)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Conc (getNumProcessors)
import Sabela.Session (
    Session (..),
    SessionConfig (..),
    getMarker,
    interruptIfBusy,
    isBusy,
    isRequestStale,
    markerText,
    placeMarker,
    readErrorBuffer,
    readSessionGen,
    runBlock,
    runBlockStreaming,
    sendRaw,
    sessLines,
    sessProc,
    sessStdin,
 )
import Sabela.Session.Drain (DrainResult (..), drainUntilMarker)
import Sabela.Session.ParentPoller (spawnParentPoller)
import Sabela.Session.Proc (
    ProcSession (..),
    destroySession,
    sessionProcessSpec,
    withSpawnedSession,
 )
import Sabela.Session.Query (
    evalPureLive,
    queryBindings,
    queryBrowse,
    queryComplete,
    queryDoc,
    queryHoleFits,
    queryInfo,
    queryKind,
    queryType,
 )
import Sabela.Session.Reader (errLoop)
import qualified Sabela.SessionTypes as ST
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (Handle, hClose, hFlush, hPutStrLn)
import System.Process (CreateProcess, proc, waitForProcess)
import System.Timeout (timeout)
import Text.Read (readMaybe)

newSession :: SessionConfig -> IO Session
newSession cfg = newSessionStreaming cfg (\_ -> pure ())

newSessionStreaming :: SessionConfig -> (Text -> IO ()) -> IO Session
newSessionStreaming = newSessionGen firstSessionGen

newSessionGen :: Int -> SessionConfig -> (Text -> IO ()) -> IO Session
newSessionGen gen cfg onStartupLine = do
    spec <- ghciProcessSpec cfg
    withSpawnedSession spec $ \ps -> do
        sess <- buildSessionStateGen gen cfg ps onStartupLine
        initializeGhci sess onStartupLine
        mapM_ spawnParentPoller (psPgid ps)
        pure sess

firstSessionGen :: Int
firstSessionGen = 1

bumpSessionGen :: Session -> IO Int
bumpSessionGen sess =
    atomicModifyIORef' (sessionGen sess) (\g -> (g + 1, g + 1))

ghciProcessSpec :: SessionConfig -> IO CreateProcess
ghciProcessSpec cfg = do
    mGhc <- lookupEnv "GHC"
    mHeap <- lookupEnv "SABELA_GHCI_MAXHEAP"
    caps <- resolveGhciCaps
    let compilerArgs = ["--with-compiler=" ++ ghc | ghc <- maybeToList mGhc]
        args = ghciArgs cfg (rtsGhcOptions caps mHeap) ++ compilerArgs
    pure (sessionProcessSpec (Just (scWorkDir cfg)) (proc "cabal" args))

resolveGhciCaps :: IO Int
resolveGhciCaps = do
    mCaps <- lookupEnv "SABELA_GHCI_CAPS"
    case mCaps >>= readMaybe of
        Just n | n > 0 -> pure n
        _ -> min detectedCapsCeiling <$> getNumProcessors

detectedCapsCeiling :: Int
detectedCapsCeiling = 8

ghciArgs :: SessionConfig -> String -> [String]
ghciArgs cfg rtsOpts =
    storeArgs
        ++ [ "repl"
           , "exe:main"
           , "--project-dir=" ++ scProjectDir cfg
           , "--builddir=" ++ scProjectDir cfg </> "dist-newstyle"
           , "-v1"
           , "--repl-options=-odir " ++ objDir ++ " -hidir " ++ objDir ++ jsonDiag
           , "--ghc-options=" ++ rtsOpts
           ]
  where
    storeArgs = maybe [] (\dir -> ["--store-dir=" ++ dir]) (scCabalStoreDir cfg)
    objDir = scProjectDir cfg </> "ghci-objs"
    jsonDiag
        | scJsonDiagnostics cfg = " -fdiagnostics-as-json"
        | otherwise = ""

rtsGhcOptions :: Int -> Maybe String -> String
rtsGhcOptions caps mHeap =
    concat
        [ "+RTS -N"
        , show n
        , " -A"
        , show (nurseryMb n)
        , "m -n4m -H1G"
        , heap
        , " -RTS"
        ]
  where
    n = max 1 caps
    heap = case mHeap of
        Nothing -> " -M" ++ defaultMaxHeap
        Just "0" -> ""
        Just h -> " -M" ++ h

nurseryMb :: Int -> Int
nurseryMb caps = max nurseryFloorMb (nurseryTotalMb `div` max 1 caps)

nurseryTotalMb :: Int
nurseryTotalMb = 512

nurseryFloorMb :: Int
nurseryFloorMb = 16

defaultMaxHeap :: String
defaultMaxHeap = "8g"

buildSessionState ::
    SessionConfig ->
    ProcSession ->
    (Text -> IO ()) ->
    IO Session
buildSessionState = buildSessionStateGen firstSessionGen

buildSessionStateGen ::
    Int ->
    SessionConfig ->
    ProcSession ->
    (Text -> IO ()) ->
    IO Session
buildSessionStateGen genTag cfg ps onStderrLine = do
    lock <- newMVar ()
    queryLock <- newMVar ()
    errBuf <- newIORef []
    counter <- newIORef 0
    cbRef <- newIORef onStderrLine
    busy <- newIORef False
    nonce <- mkSessionNonce
    lastInt <- newIORef Nothing
    gen <- newIORef genTag
    baseline <- newIORef []
    _ <- forkIO $ errLoop (psStderr ps) errBuf cbRef
    pure
        Session
            { sessProcSess = ps
            , sessLock = lock
            , sessQueryLock = queryLock
            , sessErrBuf = errBuf
            , sessCounter = counter
            , sessConfig = cfg
            , sessErrCallback = cbRef
            , sessBusy = busy
            , sessNonce = nonce
            , sessLastInterruptTime = lastInt
            , sessionGen = gen
            , sessBaselineBindings = baseline
            }

mkSessionNonce :: IO Int
mkSessionNonce = do
    t <- getPOSIXTime
    let ps = round (t * 1e12) :: Integer
    pure (fromIntegral (ps `mod` 900000000000 + 100000000000))

initializeGhci :: Session -> (Text -> IO ()) -> IO ()
initializeGhci sess onLine = do
    clearGhciPrompt sess
    forceUtf8Output sess
    sendRaw sess (":cd " ++ scWorkDir (sessConfig sess))
    mk <- getMarker sess
    placeMarker sess mk
    r <- drainUntilMarker (sessLines sess) (markerText mk) onLine
    case r of
        DrainOk _ -> pure ()
        DrainEof _ -> do
            threadDelay startupErrSettleUs
            detail <- readErrorBuffer sess
            ioError (userError (startupErrorMessage detail))

startupErrorMessage :: Text -> String
startupErrorMessage detail
    | T.null (T.strip detail) = base
    | otherwise = base ++ ":\n" ++ T.unpack (T.strip detail)
  where
    base = "GHCi exited during startup"

startupErrSettleUs :: Int
startupErrSettleUs = 200000

clearGhciPrompt :: Session -> IO ()
clearGhciPrompt sess = mapM_ (sendRaw sess) [":set prompt \"\"", ":set prompt-cont \"\""]

forceUtf8Output :: Session -> IO ()
forceUtf8Output sess =
    let
        setUtf8 d = "System.IO.hSetEncoding System.IO." ++ d ++ " System.IO.utf8"
     in
        mapM_ (sendRaw sess . setUtf8) ["stdout", "stderr"]

resetSession :: Session -> IO Session
resetSession sess = do
    prevGen <- readSessionGen sess
    closeSession sess
    newSessionGen (prevGen + 1) (sessConfig sess) (\_ -> pure ())

closeSession :: Session -> IO ()
closeSession sess = do
    _ <- timeout quitWriteGraceUs (sendQuit (sessStdin sess))
    _ <- timeout quitGraceUs (waitForProcess (sessProc sess))
    destroySession (sessProcSess sess)

quitGraceUs, quitWriteGraceUs :: Int
quitGraceUs = 2000000
quitWriteGraceUs = 1000000

sendQuit :: Handle -> IO ()
sendQuit h =
    void
        ( try (hPutStrLn h ":quit" >> hFlush h >> hClose h) ::
            IO (Either SomeException ())
        )

ghciBackend :: Session -> ST.SessionBackend
ghciBackend sess =
    ST.SessionBackend
        { ST.sbSessionId = psId (sessProcSess sess)
        , ST.sbJsonDiagnostics = scJsonDiagnostics (sessConfig sess)
        , ST.sbRunBlock = runBlock sess
        , ST.sbRunBlockStreaming = runBlockStreaming sess
        , ST.sbClose = closeSession sess
        , ST.sbReset = ghciBackend <$> resetSession sess
        , ST.sbInterrupt = interruptIfBusy sess
        , ST.sbBusy = isBusy sess
        , ST.sbSessionGen = readSessionGen sess
        , ST.sbRequestStale = isRequestStale sess
        , ST.sbQueryComplete = queryComplete sess
        , ST.sbQueryType = queryType sess
        , ST.sbQueryInfo = queryInfo sess
        , ST.sbQueryKind = queryKind sess
        , ST.sbQueryBrowse = queryBrowse sess
        , ST.sbQueryDoc = queryDoc sess
        , ST.sbQueryHoleFits = queryHoleFits sess
        , ST.sbQueryBindings = queryBindings sess
        , ST.sbEvalPureLive = evalPureLive sess
        }
