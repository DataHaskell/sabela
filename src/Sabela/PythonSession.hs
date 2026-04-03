{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Sabela.PythonSession (
    PythonSession,
    newPythonSession,
    closePythonSession,
    pythonBackend,
) where

import Control.Concurrent (MVar, forkIO, withMVar)
import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.STM (
    TBQueue,
    atomically,
    newTBQueueIO,
    readTBQueue,
    writeTBQueue,
 )
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Sabela.SessionTypes as ST
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (
    BufferMode (LineBuffering),
    Handle,
    hClose,
    hFlush,
    hGetLine,
    hIsEOF,
    hPutStrLn,
    hSetBuffering,
    hSetEncoding,
    utf8,
 )
import System.Process (
    CreateProcess (cwd, std_err, std_in, std_out),
    ProcessHandle,
    StdStream (CreatePipe),
    createProcess,
    proc,
    terminateProcess,
    waitForProcess,
 )
import System.Timeout (timeout)

newtype Marker = Marker Text

data PythonSession = PythonSession
    { pyLock :: MVar ()
    , pyStdin :: Handle
    , pyStdout :: Handle
    , pyStderr :: Handle
    , pyProc :: ProcessHandle
    , pyLines :: TBQueue Text
    , pyErrBuf :: IORef [Text]
    , pyCounter :: IORef Int
    , pyWorkDir :: FilePath
    }

newPythonSession :: Maybe FilePath -> FilePath -> IO PythonSession
newPythonSession mVenvDir workDir = do
    (hIn, hOut, hErr, ph) <- createPythonProcess mVenvDir workDir
    sess <- buildPythonState workDir hIn hOut hErr ph
    initializePython sess
    pure sess

createPythonProcess ::
    Maybe FilePath -> FilePath -> IO (Handle, Handle, Handle, ProcessHandle)
createPythonProcess mVenvDir workDir = do
    python <- findPython mVenvDir workDir
    let cp =
            (proc python ["-u", "-i"])
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                , cwd = Just workDir
                }
    (Just hIn, Just hOut, Just hErr, ph) <- createProcess cp
    mapM_
        (\h -> hSetBuffering h LineBuffering >> hSetEncoding h utf8)
        [hIn, hOut, hErr]
    pure (hIn, hOut, hErr, ph)

findPython :: Maybe FilePath -> FilePath -> IO FilePath
findPython (Just venvDir) _ = pure (venvDir </> "bin" </> "python3")
findPython Nothing workDir = do
    let venvPython = workDir </> ".venv" </> "bin" </> "python3"
    hasVenv <- doesFileExist venvPython
    pure $ if hasVenv then venvPython else "python3"

buildPythonState ::
    FilePath -> Handle -> Handle -> Handle -> ProcessHandle -> IO PythonSession
buildPythonState workDir hIn hOut hErr ph = do
    lock <- newMVar ()
    lineCh <- newTBQueueIO 256
    errBuf <- newIORef []
    counter <- newIORef 0
    _ <- forkIO $ readLoop hOut lineCh
    _ <- forkIO $ errLoop hErr errBuf
    pure
        PythonSession
            { pyLock = lock
            , pyStdin = hIn
            , pyStdout = hOut
            , pyStderr = hErr
            , pyProc = ph
            , pyLines = lineCh
            , pyErrBuf = errBuf
            , pyCounter = counter
            , pyWorkDir = workDir
            }

initializePython :: PythonSession -> IO ()
initializePython sess = do
    sendRaw sess "import sys; sys.ps1 = ''; sys.ps2 = ''"
    mk <- getMarker sess
    sendRaw sess (T.unpack pythonPrelude)
    placeMarker sess mk
    _ <- drainUntilMarker sess mk
    pure ()

closePythonSession :: PythonSession -> IO ()
closePythonSession PythonSession{pyStdin, pyProc} = do
    _ <- try (hPutStrLn pyStdin "exit()") :: IO (Either SomeException ())
    _ <- try (hFlush pyStdin) :: IO (Either SomeException ())
    _ <- try (hClose pyStdin) :: IO (Either SomeException ())
    _ <- try (terminateProcess pyProc) :: IO (Either SomeException ())
    _ <- waitForProcess pyProc
    pure ()

pythonBackend :: PythonSession -> ST.SessionBackend
pythonBackend sess =
    ST.SessionBackend
        { ST.sbRunBlock = runBlock sess
        , ST.sbRunBlockStreaming = runBlockStreaming sess
        , ST.sbClose = closePythonSession sess
        , ST.sbReset = do
            closePythonSession sess
            pythonBackend <$> newPythonSession Nothing (pyWorkDir sess)
        , ST.sbQueryComplete = \_ -> pure []
        , ST.sbQueryType = \_ -> pure ""
        , ST.sbQueryInfo = \_ -> pure ""
        , ST.sbQueryDoc = \_ -> pure ""
        }

runBlock :: PythonSession -> Text -> IO (Text, Text)
runBlock sess block = runBlockStreaming sess block (\_ -> pure ())

executionTimeoutUs :: Int
executionTimeoutUs = 120 * 1000000

runBlockStreaming :: PythonSession -> Text -> (Text -> IO ()) -> IO (Text, Text)
runBlockStreaming sess block onLine = withMVar (pyLock sess) $ \_ -> do
    resetErrorBuffer sess
    mk <- getMarker sess
    execViaFile sess block
    placeMarker sess mk
    mResult <-
        timeout executionTimeoutUs $
            drainUntilMarkerStreaming (pyLines sess) mk onLine
    collectPythonResult sess mResult

execViaFile :: PythonSession -> Text -> IO ()
execViaFile sess block = do
    let tmpPath = pyWorkDir sess </> ".sabela_cell.py"
    TIO.writeFile tmpPath block
    sendRaw sess $
        "exec(open(" ++ show tmpPath ++ ", encoding='utf-8').read(), globals())"

collectPythonResult :: PythonSession -> Maybe Text -> IO (Text, Text)
collectPythonResult sess (Just outLines) = do
    errLines <- readErrorBuffer sess
    pure (outLines, errLines)
collectPythonResult sess Nothing = do
    errLines <- readErrorBuffer sess
    pure ("", errLines <> "\n*** Execution timed out after 120 seconds ***")

pythonPrelude :: Text
pythonPrelude =
    T.unlines
        [ "import sys, json, io, traceback"
        , "def displayHtml(s):"
        , "    print('---MIME:text/html---'); print(s)"
        , ""
        , "def displayMarkdown(s):"
        , "    print('---MIME:text/markdown---'); print(s)"
        , ""
        , "def displaySvg(s):"
        , "    print('---MIME:image/svg+xml---'); print(s)"
        , ""
        , "def displayLatex(s):"
        , "    print('---MIME:text/latex---'); print(s)"
        , ""
        , "def displayJson(s):"
        , "    print('---MIME:application/json---'); print(s)"
        , ""
        , "def displayImage(mime, b64):"
        , "    print(f'---MIME:{mime};base64---'); print(b64)"
        , ""
        , "def exportBridge(name, val):"
        , "    print(f'---MIME:EXPORT:{name}---'); print(val)"
        , ""
        ]

sendRaw :: PythonSession -> String -> IO ()
sendRaw PythonSession{pyStdin} cmd = do
    hPutStrLn pyStdin cmd
    hFlush pyStdin

getMarker :: PythonSession -> IO Marker
getMarker PythonSession{pyCounter} = do
    n <- atomicModifyIORef' pyCounter (\i -> (i + 1, i))
    pure $ Marker $ "---SABELA_MARKER_" <> T.pack (show n) <> "---"

placeMarker :: PythonSession -> Marker -> IO ()
placeMarker sess (Marker mk) =
    sendRaw sess $ "print(" ++ show (T.unpack mk) ++ ")"

drainUntilMarker :: PythonSession -> Marker -> IO Text
drainUntilMarker PythonSession{pyLines} (Marker mk) =
    drainUntilMarkerStreaming pyLines (Marker mk) (\_ -> pure ())

drainUntilMarkerStreaming ::
    TBQueue Text -> Marker -> (Text -> IO ()) -> IO Text
drainUntilMarkerStreaming queue (Marker mk) onLine =
    fmap (T.strip . T.unlines) (go [])
  where
    go acc = do
        line <- atomically $ readTBQueue queue
        if T.isInfixOf mk line || T.isInfixOf eofText line
            then pure (reverse acc)
            else do
                onLine line
                go (line : acc)

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

readLoop :: Handle -> TBQueue Text -> IO ()
readLoop h ch = do
    _ <-
        try
            ( forever $ do
                eof <- hIsEOF h
                if eof
                    then atomically $ writeTBQueue ch eofText
                    else hGetLine h >>= atomically . writeTBQueue ch . T.pack
            ) ::
            IO (Either SomeException ())
    pure ()

maxErrLines :: Int
maxErrLines = 500

errLoop :: Handle -> IORef [Text] -> IO ()
errLoop h ref = do
    _ <-
        try
            ( forever $ do
                eof <- hIsEOF h
                if eof
                    then pure ()
                    else do
                        line <- hGetLine h
                        atomicModifyIORef' ref (\ls -> (take maxErrLines (T.pack line : ls), ()))
            ) ::
            IO (Either SomeException ())
    pure ()

eofText :: Text
eofText = "---EOF---"
