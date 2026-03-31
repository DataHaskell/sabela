{-# LANGUAGE OverloadedStrings #-}

module Sabela.LeanSession (
    LeanSession,
    newLeanSession,
    closeLeanSession,
    leanBackend,
    sendDocAndGetDiags,
) where

import Control.Concurrent (
    MVar,
    ThreadId,
    forkIO,
    killThread,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    threadDelay,
    tryPutMVar,
    tryTakeMVar,
    withMVar,
 )
import Control.Exception (SomeException, try)
import Control.Monad (forever, void)
import Data.Aeson (
    Value (..),
    object,
    (.=),
 )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (
    IORef,
    atomicModifyIORef',
    modifyIORef',
    newIORef,
    readIORef,
 )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.LeanLsp
import qualified Sabela.SessionTypes as ST
import System.Exit (ExitCode)
import System.IO (
    BufferMode (LineBuffering, NoBuffering),
    Handle,
    hClose,
    hPutStrLn,
    hSetBinaryMode,
    hSetBuffering,
    stderr,
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

data LeanSession = LeanSession
    { lsProcess :: ProcessHandle
    , lsStdin :: Handle
    , lsStdout :: Handle
    , lsStderr :: Handle
    , lsLock :: MVar ()
    , lsDocVersion :: IORef Int
    , lsDiagnostics :: MVar [Diagnostic]
    , lsRequestId :: IORef Int
    , lsPendingReqs :: IORef (Map Int (MVar Value))
    , lsProjectDir :: FilePath
    , lsDocUri :: Text
    , lsReaderThread :: ThreadId
    , lsFileDone :: MVar ()
    -- ^ Signalled when $/lean/fileProgress reports processing complete.
    , lsDiagVersion :: IORef Int
    -- ^ Bumped on each publishDiagnostics notification.
    }

newLeanSession :: FilePath -> IO LeanSession
newLeanSession projDir = do
    (hIn, hOut, hErr, ph) <- createLakeProcess projDir
    sess <- buildLeanState projDir hIn hOut hErr ph
    performLspHandshake sess
    pure sess

createLakeProcess :: FilePath -> IO (Handle, Handle, Handle, ProcessHandle)
createLakeProcess projDir = do
    let cp = (proc "lake" ["serve", "--"])
            { std_in = CreatePipe, std_out = CreatePipe
            , std_err = CreatePipe, cwd = Just projDir
            }
    (Just hIn, Just hOut, Just hErr, ph) <- createProcess cp
    mapM_ (`hSetBinaryMode` True) [hIn, hOut]
    hSetBuffering hIn NoBuffering
    hSetBuffering hOut NoBuffering
    hSetBuffering hErr LineBuffering
    pure (hIn, hOut, hErr, ph)

buildLeanState :: FilePath -> Handle -> Handle -> Handle -> ProcessHandle -> IO LeanSession
buildLeanState projDir hIn hOut hErr ph = do
    lock <- newMVar ()
    docVer <- newIORef 0
    diags <- newMVar []
    reqIdRef <- newIORef 0
    pending <- newIORef M.empty
    fileDone <- newEmptyMVar
    diagVer <- newIORef 0
    let docUri = "file://" <> T.pack projDir <> "/Scratch.lean"
    tid <- forkIO $ readerLoop hOut diags pending fileDone diagVer
    pure LeanSession
        { lsProcess = ph, lsStdin = hIn, lsStdout = hOut, lsStderr = hErr
        , lsLock = lock, lsDocVersion = docVer, lsDiagnostics = diags
        , lsRequestId = reqIdRef, lsPendingReqs = pending
        , lsProjectDir = projDir, lsDocUri = docUri
        , lsReaderThread = tid, lsFileDone = fileDone, lsDiagVersion = diagVer
        }

performLspHandshake :: LeanSession -> IO ()
performLspHandshake sess = do
    _ <- sendRequest sess mInitialize (initializeParams (lsDocUri sess))
    sendNotify sess mInitialized (object [])
    sendNotify sess mDidOpen (didOpenParams (lsDocUri sess) "")

closeLeanSession :: LeanSession -> IO ()
closeLeanSession sess = do
    _ <- try (killThread (lsReaderThread sess)) :: IO (Either SomeException ())
    _ <- try (sendLspMessage (lsStdin sess) (makeNotification mExit (object []))) :: IO (Either SomeException ())
    _ <- try (hClose (lsStdin sess)) :: IO (Either SomeException ())
    _ <- try (terminateProcess (lsProcess sess)) :: IO (Either SomeException ())
    void (try (waitForProcess (lsProcess sess)) :: IO (Either SomeException ExitCode))

leanBackend :: LeanSession -> ST.SessionBackend
leanBackend sess =
    ST.SessionBackend
        { ST.sbRunBlock = leanRunBlock sess
        , ST.sbRunBlockStreaming = \code _ -> leanRunBlock sess code
        , ST.sbClose = closeLeanSession sess
        , ST.sbReset = do
            closeLeanSession sess
            sess' <- newLeanSession (lsProjectDir sess)
            pure (leanBackend sess')
        , ST.sbQueryComplete = \_ -> pure []
        , ST.sbQueryType = \_ -> pure ""
        , ST.sbQueryInfo = \_ -> pure ""
        , ST.sbQueryDoc = \_ -> pure ""
        }

leanRunBlock :: LeanSession -> Text -> IO (Text, Text)
leanRunBlock sess doc = do
    diags <- sendDocAndGetDiags sess doc
    pure (diagsByKind isInfoSeverity diags, diagsByKind isErrorSeverity diags)

diagsByKind :: (Maybe DiagnosticSeverity -> Bool) -> [Diagnostic] -> Text
diagsByKind p = T.unlines . map diagMessage . filter (p . diagSeverity)

isInfoSeverity :: Maybe DiagnosticSeverity -> Bool
isInfoSeverity s = s `elem` [Just DsInformation, Just DsHint]

isErrorSeverity :: Maybe DiagnosticSeverity -> Bool
isErrorSeverity s = s `elem` [Just DsError, Just DsWarning, Nothing]

sendDocAndGetDiags :: LeanSession -> Text -> IO [Diagnostic]
sendDocAndGetDiags sess doc = do
    result <- try (sendDocAndGetDiagsUnsafe sess doc) :: IO (Either SomeException [Diagnostic])
    case result of
        Left _ -> pure []
        Right ds -> pure ds

sendDocAndGetDiagsUnsafe :: LeanSession -> Text -> IO [Diagnostic]
sendDocAndGetDiagsUnsafe sess doc = withMVar (lsLock sess) $ \_ -> do
    void $ tryTakeMVar (lsFileDone sess)
    sendDocChange sess doc
    waitForFileDone (lsFileDone sess)
    waitForDiagStable (lsDiagVersion sess)
    diags <- readMVar (lsDiagnostics sess)
    hPutStrLn stderr $ "[lean-lsp] collected " ++ show (length diags) ++ " diagnostics"
    pure diags

sendDocChange :: LeanSession -> Text -> IO ()
sendDocChange sess doc = do
    ver <- atomicModifyIORef' (lsDocVersion sess) (\v -> (v + 1, v + 1))
    sendNotify sess mDidChange (didChangeParams (lsDocUri sess) ver doc)

waitForDiagStable :: IORef Int -> IO ()
waitForDiagStable verRef = do
    v <- readIORef verRef
    settle (100 :: Int) (5 :: Int) v
  where
    settle 0 _ _ = hPutStrLn stderr "[lean-lsp] waitForDiagStable: max timeout"
    settle _ 0 _ = pure ()
    settle maxIter settleLeft lastVer = do
        threadDelay 100000
        v <- readIORef verRef
        if v /= lastVer
            then settle (maxIter - 1) 5 v
            else settle (maxIter - 1) (settleLeft - 1) v

waitForFileDone :: MVar () -> IO ()
waitForFileDone doneVar = go (300 :: Int) -- 300 * 100ms = 30s max
  where
    go 0 = pure () -- timeout, proceed with whatever diagnostics we have
    go n = do
        got <- tryTakeMVar doneVar
        case got of
            Just () -> pure ()
            Nothing -> do
                threadDelay 100000 -- 100ms
                go (n - 1)

readerLoop ::
    Handle -> MVar [Diagnostic] -> IORef (Map Int (MVar Value)) -> MVar () -> IORef Int -> IO ()
readerLoop h diagsMVar pendingRef fileDoneMVar diagVerRef = do
    _ <- try loop :: IO (Either SomeException ())
    pure ()
  where
    loop = forever $ do
        msg <- readLspMessage h
        if isNotification msg
            then dispatchNotification msg
            else dispatchResponse msg

    dispatchNotification msg =
        case notificationMethod msg of
            Just m
                | m == mPublishDiagnostics -> handleDiagnostics msg
                | m == mFileProgress -> handleFileProgress msg
            _ -> pure ()

    handleDiagnostics msg = case extractDiagnostics msg of
        Just ds -> do
            _ <- takeMVar diagsMVar
            putMVar diagsMVar ds
            atomicModifyIORef' diagVerRef (\v -> (v + 1, ()))
        Nothing -> pure ()

    handleFileProgress msg = case extractProcessing msg of
        Just arr | null arr -> void $ tryPutMVar fileDoneMVar ()
        _ -> pure ()

    dispatchResponse (Object o) = case KM.lookup "id" o of
        Just (Number n) -> fulfillPendingRequest (round n) o
        _ -> pure ()
    dispatchResponse _ = pure ()

    fulfillPendingRequest rid msg = do
        pending <- readIORef pendingRef
        case M.lookup rid pending of
            Just mvar -> do
                putMVar mvar (Object msg)
                modifyIORef' pendingRef (M.delete rid)
            Nothing -> pure ()

extractDiagnostics :: Value -> Maybe [Diagnostic]
extractDiagnostics msg = do
    Object o <- notificationParams msg
    diagsVal <- KM.lookup "diagnostics" o
    case Aeson.fromJSON diagsVal of
        Aeson.Success ds -> Just ds
        _ -> Nothing

extractProcessing :: Value -> Maybe [Value]
extractProcessing msg = do
    Object o <- notificationParams msg
    Array arr <- KM.lookup "processing" o
    pure (foldr (:) [] arr)

sendRequest :: LeanSession -> Text -> Value -> IO Value
sendRequest sess method params = do
    rid <- atomicModifyIORef' (lsRequestId sess) (\i -> (i + 1, i))
    mvar <- newEmptyMVar
    modifyIORef' (lsPendingReqs sess) (M.insert rid mvar)
    sendLspMessage (lsStdin sess) (makeRequest rid method params)
    readMVar mvar

sendNotify :: LeanSession -> Text -> Value -> IO ()
sendNotify sess method params =
    sendLspMessage (lsStdin sess) (makeNotification method params)

initializeParams :: Text -> Value
initializeParams rootUri =
    object
        [ "processId" .= Null
        , "rootUri" .= rootUri
        , "capabilities" .= object []
        ]

didOpenParams :: Text -> Text -> Value
didOpenParams uri content =
    object
        [ "textDocument"
            .= object
                [ "uri" .= uri
                , "languageId" .= ("lean4" :: Text)
                , "version" .= (0 :: Int)
                , "text" .= content
                ]
        ]

didChangeParams :: Text -> Int -> Text -> Value
didChangeParams uri ver content =
    object
        [ "textDocument"
            .= object
                [ "uri" .= uri
                , "version" .= ver
                ]
        , "contentChanges"
            .= [object ["text" .= content]]
        ]
