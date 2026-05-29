{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Long-lived GHCi session state, the wire helpers that talk to it
(send raw lines, mint and place markers, drain queued stdout, capture
stderr into a ring), and the cell/query entry points the rest of the
backend calls. Process spawn and teardown live in 'Sabela.Session.Process'.
-}
module Sabela.Session where

import Control.Concurrent (MVar, withMVar)
import Control.Concurrent.STM (
    TBQueue,
    atomically,
    readTBQueue,
    writeTBQueue,
 )
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Data.IORef (
    IORef,
    atomicModifyIORef',
    atomicWriteIORef,
    readIORef,
 )
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (
    Handle,
    hFlush,
    hGetLine,
    hIsEOF,
    hPutStrLn,
 )
import System.Process (ProcessHandle, getProcessExitCode)
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
    -- Bang the accumulator so big cell outputs don't pile a thunk
    -- chain that only collapses at marker time.
    go !acc = do
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

queryKind :: Session -> Text -> IO Text
queryKind sess name = runQueryCommand sess (QueryKind name)

queryBrowse :: Session -> Text -> IO Text
queryBrowse sess mname = runQueryCommand sess (QueryBrowse mname)

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
    | QueryKind Text
    | QueryBrowse Text
    | QueryDoc Text
    | QueryComplete Text

toText :: QueryCommand -> Text
toText (QueryType t) = ":type " <> t
toText (QueryInfo t) = ":info " <> t
toText (QueryKind t) = ":kind " <> t
toText (QueryBrowse t) = ":browse " <> t
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
        -- Force the bounded prefix so the lazy @take@ doesn't pin the
        -- old tail beyond the cap.
        atomicModifyIORef'
            ref
            (\ls -> let !ls' = take maxErrLines (t : ls) in (ls', ()))
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
    -- Bang as in 'drainUntilMarkerStreaming'.
    go !acc = do
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
