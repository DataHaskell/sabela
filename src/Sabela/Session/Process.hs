{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Spawn, initialise, and tear down the long-lived GHCi subprocess
that backs a notebook session. The flow is: 'createGhciProcess' starts
@cabal repl@ with the right RTS knobs, 'buildSessionState' wires the
output drains, and 'initializeGhci' silences the prompt and
synchronises on a first marker so the session is ready to run a cell.
-}
module Sabela.Session.Process (
    -- * Lifecycle
    newSession,
    newSessionStreaming,
    resetSession,
    closeSession,

    -- * Backend adapter
    ghciBackend,

    -- * Building blocks (exposed for tests and the streaming hook)
    createGhciProcess,
    ghciArgs,
    rtsGhcOptions,
    buildSessionState,
    initializeGhci,
    clearGhciPrompt,

    -- * Teardown helpers
    sendQuit,
    forceKill,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.STM (newTBQueueIO)
import Control.Exception (SomeException, try)
import Data.IORef (newIORef)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Sabela.Session (
    Session (..),
    SessionConfig (..),
    drainUntilMarkerStreaming,
    errLoop,
    getMarker,
    placeMarker,
    queryBrowse,
    queryComplete,
    queryDoc,
    queryInfo,
    queryKind,
    queryType,
    readLoop,
    runBlock,
    runBlockStreaming,
    sendRaw,
 )
import qualified Sabela.SessionTypes as ST
import System.Environment (lookupEnv)
import System.Exit (ExitCode)
import System.IO (
    BufferMode (LineBuffering),
    Handle,
    hClose,
    hFlush,
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

newSession :: SessionConfig -> IO Session
newSession cfg = newSessionStreaming cfg (\_ -> pure ())

newSessionStreaming :: SessionConfig -> (Text -> IO ()) -> IO Session
newSessionStreaming cfg onStartupLine = do
    (hIn, hOut, hErr, ph) <- createGhciProcess cfg
    sess <- buildSessionState cfg hIn hOut hErr ph onStartupLine
    initializeGhci sess onStartupLine
    pure sess

createGhciProcess :: SessionConfig -> IO (Handle, Handle, Handle, ProcessHandle)
createGhciProcess cfg = do
    mGhc <- lookupEnv "GHC"
    mCaps <- lookupEnv "SABELA_GHCI_CAPS"
    mHeap <- lookupEnv "SABELA_GHCI_MAXHEAP"
    let compilerArgs = ["--with-compiler=" ++ ghc | ghc <- maybeToList mGhc]
        args = ghciArgs cfg (rtsGhcOptions mCaps mHeap) ++ compilerArgs
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

ghciArgs :: SessionConfig -> String -> [String]
ghciArgs cfg rtsOpts =
    [ "repl"
    , "exe:main"
    , "--project-dir=" ++ scProjectDir cfg
    , "-v1"
    , "--repl-options=-fobject-code -O2"
    , "--ghc-options=" ++ rtsOpts
    , "-O2"
    ]

{- | GHCi RTS options. @-N@ and the heap limit are pinned from the environment
so a containerized session honors its CPU/memory caps: a bare @-N@ spins up one
capability per host core regardless of @--cpus@, and without @-M@ a runaway
build can OOM the whole box. Unset (local dev) keeps the prior @-N@ behavior.
-}
rtsGhcOptions :: Maybe String -> Maybe String -> String
rtsGhcOptions mCaps mHeap =
    "+RTS " ++ caps ++ " -A512m -n4m -H1G" ++ heap ++ " -RTS"
  where
    caps = maybe "-N" ("-N" ++) mCaps
    heap = maybe "" (" -M" ++) mHeap

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

initializeGhci :: Session -> (Text -> IO ()) -> IO ()
initializeGhci sess onLine = do
    clearGhciPrompt sess
    sendRaw sess (":cd " ++ scWorkDir (sessConfig sess))
    mk <- getMarker sess
    placeMarker sess mk
    _ <- drainUntilMarkerStreaming (sessLines sess) mk onLine
    pure ()

clearGhciPrompt :: Session -> IO ()
clearGhciPrompt sess = mapM_ (sendRaw sess) [":set prompt \"\"", ":set prompt-cont \"\""]

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

{- | Adapter exposing the live GHCi 'Session' through the generic
'ST.SessionBackend' record-of-functions interface.
-}
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
        , ST.sbQueryKind = queryKind sess
        , ST.sbQueryBrowse = queryBrowse sess
        , ST.sbQueryDoc = queryDoc sess
        }
