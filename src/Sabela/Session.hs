{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Sabela.Session where

import Control.Concurrent (MVar, forkIO, newChan, readChan, withMVar)
import Control.Concurrent.Chan (Chan, writeChan)
import Control.Concurrent.MVar (newMVar)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (singleton)
import Data.Text (Text)
import qualified Data.Text as T
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
    CreateProcess (std_err, std_in, std_out),
    ProcessHandle,
    StdStream (CreatePipe),
    createProcess,
    proc,
    waitForProcess,
 )

newtype Marker = Marker Text

data Session = Session
    { sessLock :: MVar ()
    , sessStdin :: Handle
    , sessStdout :: Handle
    , sessStderr :: Handle
    , sessProc :: ProcessHandle
    , sessLines :: Chan Text
    , sessErrBuf :: IORef [Text]
    , sessCounter :: IORef Int
    , sessConfig :: SessionConfig
    }

data SessionConfig = SessionConfig
    { scDeps :: [Text]
    , scExts :: [Text]
    , scGhcOptions :: [Text]
    , scEnvFile :: Maybe FilePath
    }
    deriving (Show, Eq)

newSession :: SessionConfig -> IO Session
newSession cfg = do
    let envFlags = maybe [] ((["-package-env="] ++) . singleton) (scEnvFile cfg)
        extFlags = map (("-X" ++) . T.unpack) (scExts cfg)
        optFlags = map T.unpack (scGhcOptions cfg)
        args =
            ["--interactive", "-ignore-dot-ghci", "v0"]
                ++ envFlags
                ++ extFlags
                ++ optFlags
        cp =
            (proc "ghc" args)
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
    (Just hIn, Just hOut, Just hErr, ph) <- createProcess cp

    mapM_
        ( \h -> do
            hSetBuffering h LineBuffering
            hSetEncoding h utf8
        )
        [hIn, hOut, hErr]

    lock <- newMVar ()
    lineCh <- newChan
    errBuf <- newIORef []
    counter <- newIORef 0

    _ <- forkIO $ readLoop hOut lineCh
    _ <- forkIO $ errLoop hErr errBuf

    let sess =
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
                }

    clearGhciPrompt sess

    mk <- getMarker sess
    placeMarker sess mk

    _ <- drainUntilMarker sess mk

    pure sess

resetSession :: Session -> IO Session
resetSession sess = do
    closeSession sess
    newSession (sessConfig sess)

closeSession :: Session -> IO ()
closeSession Session{sessStdin, sessProc} = do
    _ <- try (hPutStrLn sessStdin ":quit") :: IO (Either SomeException ())
    _ <- try (hFlush sessStdin) :: IO (Either SomeException ())
    _ <- try (hClose sessStdin) :: IO (Either SomeException ())
    _ <- waitForProcess sessProc
    pure ()

runBlock :: Session -> Text -> IO (Text, Text)
runBlock sess block = withMVar (sessLock sess) $ \_ -> do
    resetErrorBuffer sess
    mk <- getMarker sess
    mapM_ (sendRaw sess . T.unpack) (T.lines block)
    placeMarker sess mk
    outLines <- drainUntilMarker sess mk
    errLines <- readErrorBuffer sess
    pure (outLines, errLines)

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
    atomicModifyIORef' (sessErrBuf sess) (const ([], ()))
    mk <- getMarker sess
    sendRaw sess $ T.unpack $ toText cmd
    placeMarker sess mk
    outLines <- drainUntilMarker sess mk
    errLines <- readErrorBuffer sess
    let out = T.strip outLines
    if T.null out
        then pure errLines
        else pure out

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

readLoop :: Handle -> Chan Text -> IO ()
readLoop h ch = do
    _ <-
        try (forever (processHandle h (const (writeChan ch eofText)) writeOutput)) ::
            IO (Either SomeException ())
    pure ()
  where
    writeOutput :: Handle -> IO ()
    writeOutput h' = hGetLine h' >>= writeChan ch . T.pack

errLoop :: Handle -> IORef [Text] -> IO ()
errLoop h ref = do
    _ <-
        try (forever (processHandle h (const (pure ())) writeErr)) ::
            IO (Either SomeException ())
    pure ()
  where
    writeErr :: Handle -> IO ()
    writeErr h' = do
        line <- hGetLine h'
        atomicModifyIORef' ref (\ls -> (T.pack line : ls, ()))

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
        line <- readChan sessLines
        if T.isInfixOf mk line || T.isInfixOf eofText line
            then pure (reverse acc)
            else go (line : acc)

resetErrorBuffer :: Session -> IO ()
resetErrorBuffer sess = atomicModifyIORef' (sessErrBuf sess) (const ([], ()))

readErrorBuffer :: Session -> IO Text
readErrorBuffer sess = fmap (T.strip . T.unlines . reverse) (readIORef (sessErrBuf sess))

eofText :: Text
eofText = "---EOF---"
