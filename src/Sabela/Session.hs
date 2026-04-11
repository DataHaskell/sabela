{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Sabela.Session where

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
import Data.IORef (
    IORef,
    atomicModifyIORef',
    atomicWriteIORef,
    newIORef,
    readIORef,
 )
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Sabela.SessionTypes as ST
import System.Environment (lookupEnv)
import System.Exit (ExitCode)
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
    getProcessExitCode,
    proc,
    terminateProcess,
    waitForProcess,
 )
import System.Timeout (timeout)

newtype Marker = Marker Text

data Session = Session
    { sessLock :: MVar ()
    , sessStdin :: Handle
    , sessStdout :: Handle
    , sessStderr :: Handle
    , sessProc :: ProcessHandle
    , sessLines :: TBQueue Text
    , sessErrBuf :: IORef [Text]
    , sessCounter :: IORef Int
    , sessConfig :: SessionConfig
    , sessErrCallback :: IORef (Text -> IO ())
    }

data SessionConfig = SessionConfig
    { scProjectDir :: FilePath
    , scWorkDir :: FilePath
    }
    deriving (Show, Eq)

newSession :: SessionConfig -> IO Session
newSession cfg = newSessionStreaming cfg (\_ -> pure ())

newSessionStreaming :: SessionConfig -> (Text -> IO ()) -> IO Session
newSessionStreaming cfg onStderrLine = do
    (hIn, hOut, hErr, ph) <- createGhciProcess cfg
    sess <- buildSessionState cfg hIn hOut hErr ph onStderrLine
    initializeGhci sess
    pure sess

createGhciProcess :: SessionConfig -> IO (Handle, Handle, Handle, ProcessHandle)
createGhciProcess cfg = do
    mGhc <- lookupEnv "GHC"
    let compilerArgs = ["--with-compiler=" ++ ghc | ghc <- maybeToList mGhc]
        args = ghciArgs cfg ++ compilerArgs
        cp =
            (proc "cabal" args)
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                , cwd = Just (scWorkDir cfg)
                }
    (Just hIn, Just hOut, Just hErr, ph) <- createProcess cp
    mapM_
        (\h -> hSetBuffering h LineBuffering >> hSetEncoding h utf8)
        [hIn, hOut, hErr]
    pure (hIn, hOut, hErr, ph)

ghciArgs :: SessionConfig -> [String]
ghciArgs cfg =
    [ "repl"
    , "exe:main"
    , "--project-dir=" ++ scProjectDir cfg
    , "-v1"
    , "--repl-options=-fobject-code -O2"
    , "--ghc-options=+RTS -N -A512m -n4m -H1G -RTS"
    , "-O2"
    ]

buildSessionState ::
    SessionConfig ->
    Handle ->
    Handle ->
    Handle ->
    ProcessHandle ->
    (Text -> IO ()) ->
    IO Session
buildSessionState cfg hIn hOut hErr ph onStderrLine = do
    lock <- newMVar ()
    lineCh <- newTBQueueIO 256
    errBuf <- newIORef []
    counter <- newIORef 0
    cbRef <- newIORef onStderrLine
    _ <- forkIO $ readLoop hOut lineCh
    _ <- forkIO $ errLoop hErr errBuf cbRef
    pure
        Session
            { sessLock = lock
            , sessStdin = hIn
            , sessStdout = hOut
            , sessStderr = hErr
            , sessProc = ph
            , sessLines = lineCh
            , sessErrBuf = errBuf
            , sessCounter = counter
            , sessConfig = cfg
            , sessErrCallback = cbRef
            }

initializeGhci :: Session -> IO ()
initializeGhci sess = do
    clearGhciPrompt sess
    sendRaw sess (":cd " ++ scWorkDir (sessConfig sess))
    mk <- getMarker sess
    placeMarker sess mk
    _ <- drainUntilMarker sess mk
    pure ()

resetSession :: Session -> IO Session
resetSession sess = do
    closeSession sess
    newSession (sessConfig sess)

closeSession :: Session -> IO ()
closeSession Session{sessStdin, sessProc} = do
    sendQuit sessStdin
    exited <- timeout 5000000 (waitForProcess sessProc)
    case exited of
        Just _ -> pure ()
        Nothing -> forceKill sessProc

sendQuit :: Handle -> IO ()
sendQuit h = do
    _ <- try (hPutStrLn h ":quit") :: IO (Either SomeException ())
    _ <- try (hFlush h) :: IO (Either SomeException ())
    _ <- try (hClose h) :: IO (Either SomeException ())
    pure ()

forceKill :: ProcessHandle -> IO ()
forceKill ph = do
    _ <- try (terminateProcess ph) :: IO (Either SomeException ())
    _ <- try (waitForProcess ph) :: IO (Either SomeException ExitCode)
    pure ()

runBlock :: Session -> Text -> IO (Text, Text)
runBlock sess block = runBlockStreaming sess block (\_ -> pure ())

executionTimeoutUs :: Int
executionTimeoutUs = 120 * 1000000

runBlockStreaming :: Session -> Text -> (Text -> IO ()) -> IO (Text, Text)
runBlockStreaming sess block onLine = withMVar (sessLock sess) $ \_ -> do
    checkProcessAlive sess
    resetErrorBuffer sess
    mk <- getMarker sess
    mResult <-
        timeout executionTimeoutUs $ do
            mapM_ (sendRaw sess . T.unpack) (T.lines block)
            placeMarker sess mk
            drainUntilMarkerStreaming (sessLines sess) mk onLine
    collectResult sess mResult

checkProcessAlive :: Session -> IO ()
checkProcessAlive sess = do
    mExit <- getProcessExitCode (sessProc sess)
    case mExit of
        Nothing -> pure ()
        Just code ->
            ioError $
                userError $
                    "GHCi process exited with " ++ show code

collectResult :: Session -> Maybe Text -> IO (Text, Text)
collectResult sess (Just outLines) = do
    errLines <- readErrorBuffer sess
    pure (outLines, errLines)
collectResult sess Nothing = do
    errLines <- readErrorBuffer sess
    pure ("", errLines <> "\n*** Execution timed out after 120 seconds ***")

drainUntilMarkerStreaming ::
    TBQueue Text -> Marker -> (Text -> IO ()) -> IO Text
drainUntilMarkerStreaming queue (Marker mk) onLine = fmap (T.strip . T.unlines) (go [])
  where
    go acc = do
        line <- atomically $ readTBQueue queue
        if T.isInfixOf mk line || T.isInfixOf eofText line
            then pure (reverse acc)
            else do
                onLine line
                go (line : acc)

queryComplete :: Session -> Text -> IO [Text]
queryComplete sess prefix = do
    res <- runQueryCommand sess (QueryComplete ("\"" <> prefix <> "\""))
    pure (concatMap parseCompletionLine (T.lines res))

parseCompletionLine :: Text -> [Text]
parseCompletionLine line =
    let stripped = T.strip line
     in case T.stripPrefix "\"" stripped of
            Just rest -> case T.stripSuffix "\"" rest of
                Just inner -> [inner]
                Nothing -> []
            Nothing -> []

queryType :: Session -> Text -> IO Text
queryType sess name = runQueryCommand sess (QueryType name)

queryInfo :: Session -> Text -> IO Text
queryInfo sess name = runQueryCommand sess (QueryInfo name)

queryDoc :: Session -> Text -> IO Text
queryDoc sess name = runQueryCommand sess (QueryDoc name)

runQueryCommand :: Session -> QueryCommand -> IO Text
runQueryCommand sess cmd = withMVar (sessLock sess) $ \_ -> do
    resetErrorBuffer sess
    mk <- getMarker sess
    sendRaw sess $ T.unpack $ toText cmd
    placeMarker sess mk
    outLines <- drainUntilMarker sess mk
    errLines <- readErrorBuffer sess
    pure $ if T.null (T.strip outLines) then errLines else T.strip outLines

data QueryCommand
    = QueryType Text
    | QueryInfo Text
    | QueryDoc Text
    | QueryComplete Text

toText :: QueryCommand -> Text
toText (QueryType t) = ":type " <> t
toText (QueryInfo t) = ":info " <> t
toText (QueryDoc t) = ":doc " <> t
toText (QueryComplete t) = ":complete repl " <> t

readLoop :: Handle -> TBQueue Text -> IO ()
readLoop h ch = do
    _ <-
        try
            ( forever
                (processHandle h (const (atomically $ writeTBQueue ch eofText)) writeOutput)
            ) ::
            IO (Either SomeException ())
    atomically $ writeTBQueue ch eofText
  where
    writeOutput :: Handle -> IO ()
    writeOutput h' = hGetLine h' >>= atomically . writeTBQueue ch . T.pack

maxErrLines :: Int
maxErrLines = 500

errLoop :: Handle -> IORef [Text] -> IORef (Text -> IO ()) -> IO ()
errLoop h ref cbRef = do
    _ <-
        try (forever (processHandle h (const (pure ())) writeErr)) ::
            IO (Either SomeException ())
    pure ()
  where
    writeErr :: Handle -> IO ()
    writeErr h' = do
        line <- hGetLine h'
        let t = T.pack line
        atomicModifyIORef' ref (\ls -> (take maxErrLines (t : ls), ()))
        cb <- readIORef cbRef
        cb t

processHandle :: Handle -> (Handle -> IO ()) -> (Handle -> IO ()) -> IO ()
processHandle h eofHandler contHandler = do
    eof <- hIsEOF h
    if eof then eofHandler h else contHandler h

sendRaw :: Session -> String -> IO ()
sendRaw Session{sessStdin} cmd = do
    hPutStrLn sessStdin cmd
    hFlush sessStdin

clearGhciPrompt :: Session -> IO ()
clearGhciPrompt sess = mapM_ (sendRaw sess) [":set prompt \"\"", ":set prompt-cont \"\""]

getMarker :: Session -> IO Marker
getMarker Session{sessCounter} = do
    n <- atomicModifyIORef' sessCounter (\i -> (i + 1, i))
    pure $ Marker $ "---SABELA_MARKER_" <> T.pack (show n) <> "---"

placeMarker :: Session -> Marker -> IO ()
placeMarker sess (Marker mk) = sendRaw sess $ "putStrLn " ++ show (T.unpack mk)

drainUntilMarker :: Session -> Marker -> IO Text
drainUntilMarker Session{sessLines} (Marker mk) = fmap (T.strip . T.unlines) (go [])
  where
    go :: [Text] -> IO [Text]
    go acc = do
        line <- atomically $ readTBQueue sessLines
        if T.isInfixOf mk line || T.isInfixOf eofText line
            then pure (reverse acc)
            else go (line : acc)

resetErrorBuffer :: Session -> IO ()
resetErrorBuffer sess = atomicModifyIORef' (sessErrBuf sess) (const ([], ()))

readErrorBuffer :: Session -> IO Text
readErrorBuffer sess = fmap (T.strip . T.unlines . reverse) (readIORef (sessErrBuf sess))

clearErrCallback :: Session -> IO ()
clearErrCallback sess = atomicWriteIORef (sessErrCallback sess) (\_ -> pure ())

eofText :: Text
eofText = "---EOF---"

ghciBackend :: Session -> ST.SessionBackend
ghciBackend sess =
    ST.SessionBackend
        { ST.sbRunBlock = runBlock sess
        , ST.sbRunBlockStreaming = runBlockStreaming sess
        , ST.sbClose = closeSession sess
        , ST.sbReset = ghciBackend <$> resetSession sess
        , ST.sbQueryComplete = queryComplete sess
        , ST.sbQueryType = queryType sess
        , ST.sbQueryInfo = queryInfo sess
        , ST.sbQueryDoc = queryDoc sess
        }
