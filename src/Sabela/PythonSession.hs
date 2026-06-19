{-# LANGUAGE OverloadedStrings #-}

{- | Python REPL session backend, with the same lifecycle guarantees as
the GHCi backend: own process group, bounded binary-safe capture,
busy-gated interrupt, and timeout→interrupt→resync→destroy.
-}
module Sabela.PythonSession (
    PythonSession,
    newPythonSession,
    closePythonSession,
    pythonBackend,
) where

import Control.Concurrent (MVar, forkIO, withMVar)
import Control.Concurrent.MVar (newMVar)
import Control.Exception (SomeException, bracket_, try)
import Control.Monad (void, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Sabela.Platform (systemPython, venvPythonPath)
import Sabela.Session.Drain (
    DrainResult (..),
    discardUntilMarker,
    drainUntilMarker,
 )
import Sabela.Session.Proc (
    ProcSession (..),
    destroySession,
    interruptGroup,
    sessionProcessSpec,
    withSpawnedSession,
 )
import Sabela.Session.Reader (OutQueue, errLoop, mkMarkerText)
import Sabela.Session.Timeout (
    TimeoutConfig (..),
    readTimeoutConfig,
    timedOutMessage,
 )
import qualified Sabela.SessionTypes as ST
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn)
import System.Process (proc, waitForProcess)
import System.Timeout (timeout)

newtype Marker = Marker Text

data PythonSession = PythonSession
    { pyProcSess :: ProcSession
    , pyLock :: MVar ()
    , pyErrBuf :: IORef [Text]
    , pyCounter :: IORef Int
    , pyWorkDir :: FilePath
    , pyBusy :: IORef Bool
    , pyTimeout :: TimeoutConfig
    -- ^ Execution/resync budget, read from @SABELA_CELL_TIMEOUT_SECONDS@.
    }

newPythonSession :: Maybe FilePath -> FilePath -> IO PythonSession
newPythonSession mVenvDir workDir = do
    python <- findPython mVenvDir workDir
    let spec = sessionProcessSpec (Just workDir) (proc python ["-u", "-i"])
    withSpawnedSession spec $ \ps -> do
        sess <- buildPythonState workDir ps
        initializePython sess
        pure sess

findPython :: Maybe FilePath -> FilePath -> IO FilePath
findPython (Just venvDir) _ = pure (venvPythonPath venvDir)
findPython Nothing workDir = do
    let venvPython = venvPythonPath (workDir </> ".venv")
    hasVenv <- doesFileExist venvPython
    pure $ if hasVenv then venvPython else systemPython

buildPythonState :: FilePath -> ProcSession -> IO PythonSession
buildPythonState workDir ps = do
    lock <- newMVar ()
    errBuf <- newIORef []
    counter <- newIORef 0
    busy <- newIORef False
    cbRef <- newIORef (\_ -> pure ())
    tc <- readTimeoutConfig
    _ <- forkIO $ errLoop (psStderr ps) errBuf cbRef
    pure
        PythonSession
            { pyProcSess = ps
            , pyLock = lock
            , pyErrBuf = errBuf
            , pyCounter = counter
            , pyWorkDir = workDir
            , pyBusy = busy
            , pyTimeout = tc
            }

initializePython :: PythonSession -> IO ()
initializePython sess = do
    sendRaw sess "import sys; sys.ps1 = ''; sys.ps2 = ''"
    mk <- getMarker sess
    sendRaw sess (T.unpack pythonPrelude)
    placeMarker sess mk
    r <- drainUntilMarker (psQueue (pyProcSess sess)) (markerOf mk) (\_ -> pure ())
    case r of
        DrainOk _ -> pure ()
        DrainEof _ -> ioError (userError "Python exited during startup")
  where
    markerOf (Marker t) = t

-- | Polite close: @exit()@, a short grace, then the teardown chokepoint.
closePythonSession :: PythonSession -> IO ()
closePythonSession sess = do
    _ <- timeout quitWriteGraceUs (quiet (sendRaw sess "exit()"))
    _ <- timeout quitGraceUs (waitForProcess (psProc (pyProcSess sess)))
    destroySession (pyProcSess sess)

quiet :: IO () -> IO ()
quiet act = void (try act :: IO (Either SomeException ()))

quitGraceUs, quitWriteGraceUs :: Int
quitGraceUs = 2000000
quitWriteGraceUs = 1000000

pythonBackend :: PythonSession -> ST.SessionBackend
pythonBackend sess =
    ST.SessionBackend
        { ST.sbSessionId = psId (pyProcSess sess)
        , ST.sbRunBlock = runBlock sess
        , ST.sbRunBlockStreaming = runBlockStreaming sess
        , ST.sbClose = closePythonSession sess
        , ST.sbReset = do
            closePythonSession sess
            pythonBackend <$> newPythonSession Nothing (pyWorkDir sess)
        , ST.sbInterrupt = interruptIfBusy sess
        , ST.sbBusy = pure False
        , ST.sbSessionGen = pure 0
        , ST.sbRequestStale = \_ -> pure False
        , ST.sbQueryComplete = \_ -> pure []
        , ST.sbQueryType = \_ -> pure ""
        , ST.sbQueryInfo = \_ -> pure ""
        , ST.sbQueryKind = \_ -> pure ""
        , ST.sbQueryBrowse = \_ -> pure ""
        , ST.sbQueryDoc = \_ -> pure ""
        }

interruptIfBusy :: PythonSession -> IO ()
interruptIfBusy sess = do
    busy <- readIORef (pyBusy sess)
    when busy $ interruptGroup (pyProcSess sess)

runBlock :: PythonSession -> Text -> IO (Text, Text)
runBlock sess block = runBlockStreaming sess block (\_ -> pure ())

executionTimeoutUs, resyncTimeoutUs :: PythonSession -> Int
executionTimeoutUs = tcExecutionUs . pyTimeout
resyncTimeoutUs = tcResyncUs . pyTimeout

{- | Run a cell. On timeout: group SIGINT (KeyboardInterrupt), resync on
a fresh marker, destroy if the interpreter stays silent — identical
semantics to the GHCi backend.
-}
runBlockStreaming :: PythonSession -> Text -> (Text -> IO ()) -> IO (Text, Text)
runBlockStreaming sess block onLine = withMVar (pyLock sess) $ \_ -> do
    resetErrorBuffer sess
    mk <- getMarker sess
    mResult <-
        timeout (executionTimeoutUs sess) $ do
            execViaFile sess block
            placeMarker sess mk
            bracket_ (setBusy sess True) (setBusy sess False) $
                drainUntilMarker (queue sess) (markerOf mk) onLine
    finishRun sess mResult
  where
    markerOf (Marker t) = t

finishRun :: PythonSession -> Maybe DrainResult -> IO (Text, Text)
finishRun sess (Just (DrainOk out)) = do
    errLines <- readErrorBuffer sess
    pure (out, errLines)
finishRun sess (Just (DrainEof _)) = do
    destroySession (pyProcSess sess)
    ioError (userError "Python session ended unexpectedly mid-cell")
finishRun sess Nothing = do
    interruptGroup (pyProcSess sess)
    mk2 <- getMarker sess
    synced <-
        timeout (resyncTimeoutUs sess) $ do
            placeMarker sess mk2
            discardUntilMarker (queue sess) (markerTextOf mk2)
    case synced of
        Just True -> do
            errLines <- readErrorBuffer sess
            pure
                ( ""
                , errLines <> timedOutMessage (executionTimeoutUs sess)
                )
        _ -> do
            destroySession (pyProcSess sess)
            ioError
                ( userError
                    "Cell timed out and the session did not respond \
                    \to interrupt; session killed"
                )
  where
    markerTextOf (Marker t) = t

queue :: PythonSession -> OutQueue
queue = psQueue . pyProcSess

setBusy :: PythonSession -> Bool -> IO ()
setBusy sess = writeIORef (pyBusy sess)

{- | Write the cell to a temp file and exec it. 'show' on the path
coincides with Python string-literal escaping because both the tmp dir
and the fixed filename are ASCII; revisit if either stops being ASCII.
-}
execViaFile :: PythonSession -> Text -> IO ()
execViaFile sess block = do
    let tmpPath = pyWorkDir sess </> ".sabela_cell.py"
    TIO.writeFile tmpPath block
    sendRaw sess $
        "exec(open(" ++ show tmpPath ++ ", encoding='utf-8').read(), globals())"

pythonPrelude :: Text
pythonPrelude =
    T.unlines
        [ "import sys, json, io, traceback"
        , "def displayHtml(s):"
        , "    print('<!-- MIME:text/html -->'); print(s)"
        , ""
        , "def displayMarkdown(s):"
        , "    print('<!-- MIME:text/markdown -->'); print(s)"
        , ""
        , "def displaySvg(s):"
        , "    print('<!-- MIME:image/svg+xml -->'); print(s)"
        , ""
        , "def displayLatex(s):"
        , "    print('<!-- MIME:text/latex -->'); print(s)"
        , ""
        , "def displayJson(s):"
        , "    print('<!-- MIME:application/json -->'); print(s)"
        , ""
        , "def displayImage(mime, b64):"
        , "    print(f'<!-- MIME:{mime};base64 -->'); print(b64)"
        , ""
        , "def exportBridge(name, val):"
        , "    print(f'<!-- MIME:EXPORT:{name} -->'); print(val)"
        , ""
        ]

sendRaw :: PythonSession -> String -> IO ()
sendRaw sess cmd = do
    hPutStrLn (psStdin (pyProcSess sess)) cmd
    hFlush (psStdin (pyProcSess sess))

getMarker :: PythonSession -> IO Marker
getMarker sess = do
    n <- atomicModifyIORef' (pyCounter sess) (\i -> (i + 1, i))
    pure (Marker (mkMarkerText n))

placeMarker :: PythonSession -> Marker -> IO ()
placeMarker sess (Marker mk) =
    sendRaw sess $ "print(" ++ show (T.unpack mk) ++ ")"

resetErrorBuffer :: PythonSession -> IO ()
resetErrorBuffer sess = atomicModifyIORef' (pyErrBuf sess) (const ([], ()))

readErrorBuffer :: PythonSession -> IO Text
readErrorBuffer sess =
    fmap (T.strip . T.unlines . reverse . filterPrompts) (readIORef (pyErrBuf sess))

filterPrompts :: [Text] -> [Text]
filterPrompts = filter (not . isPrompt)
  where
    isPrompt l =
        let s = T.strip l
         in s == ">>>" || s == "..." || s == ">>> " || s == "... "
