{-# LANGUAGE OverloadedStrings #-}

{- | Spawn, initialise, and tear down the long-lived GHCi subprocess
backing a notebook session: 'newSessionStreaming' starts @cabal repl@
in its own process group via 'Sabela.Session.Proc', wires the output
drains, and synchronises on a first marker before any cell runs.
-}
module Sabela.Session.Process (
    -- * Lifecycle
    newSession,
    newSessionStreaming,
    resetSession,
    closeSession,

    -- * Backend adapter
    ghciBackend,

    -- * Building blocks (exposed for tests)
    ghciArgs,
    rtsGhcOptions,
    ghciProcessSpec,
    buildSessionState,
    initializeGhci,
    startupErrorMessage,
    clearGhciPrompt,
    sendQuit,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newMVar)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.IORef (newIORef)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Session (
    Session (..),
    SessionConfig (..),
    getMarker,
    interruptIfBusy,
    markerText,
    placeMarker,
    readErrorBuffer,
    runBlock,
    runBlockStreaming,
    sendRaw,
    sessLines,
    sessProc,
    sessStdin,
 )
import Sabela.Session.Drain (DrainResult (..), drainUntilMarker)
import Sabela.Session.Proc (
    ProcSession (..),
    destroySession,
    sessionProcessSpec,
    withSpawnedSession,
 )
import Sabela.Session.Query (
    queryBrowse,
    queryComplete,
    queryDoc,
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

newSession :: SessionConfig -> IO Session
newSession cfg = newSessionStreaming cfg (\_ -> pure ())

newSessionStreaming :: SessionConfig -> (Text -> IO ()) -> IO Session
newSessionStreaming cfg onStartupLine = do
    spec <- ghciProcessSpec cfg
    withSpawnedSession spec $ \ps -> do
        sess <- buildSessionState cfg ps onStartupLine
        initializeGhci sess onStartupLine
        pure sess

-- | The @cabal repl@ spec, in its own process group with piped handles.
ghciProcessSpec :: SessionConfig -> IO CreateProcess
ghciProcessSpec cfg = do
    mGhc <- lookupEnv "GHC"
    mCaps <- lookupEnv "SABELA_GHCI_CAPS"
    mHeap <- lookupEnv "SABELA_GHCI_MAXHEAP"
    let compilerArgs = ["--with-compiler=" ++ ghc | ghc <- maybeToList mGhc]
        args = ghciArgs cfg (rtsGhcOptions mCaps mHeap) ++ compilerArgs
    pure (sessionProcessSpec (Just (scWorkDir cfg)) (proc "cabal" args))

{- | Repl args. Loaded modules (the compile-mode generated ones) build as
@-O2@ object code; @-odir@\/@-hidir@ are pinned absolute so the @:cd@ at init
cannot orphan the cache, and @-fexpose-all-unfoldings@ lets hot functions
inline across generated module boundaries.
-}
ghciArgs :: SessionConfig -> String -> [String]
ghciArgs cfg rtsOpts =
    [ "repl"
    , "exe:main"
    , "--project-dir=" ++ scProjectDir cfg
    , "-v1"
    , "--repl-options=-fobject-code -O2 -fexpose-all-unfoldings"
        ++ " -odir "
        ++ objDir
        ++ " -hidir "
        ++ objDir
    , "--ghc-options=" ++ rtsOpts
    , "-O2"
    ]
  where
    objDir = scProjectDir cfg </> "ghci-objs"

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
    ProcSession ->
    (Text -> IO ()) ->
    IO Session
buildSessionState cfg ps onStderrLine = do
    lock <- newMVar ()
    errBuf <- newIORef []
    counter <- newIORef 0
    cbRef <- newIORef onStderrLine
    busy <- newIORef False
    _ <- forkIO $ errLoop (psStderr ps) errBuf cbRef
    pure
        Session
            { sessProcSess = ps
            , sessLock = lock
            , sessErrBuf = errBuf
            , sessCounter = counter
            , sessConfig = cfg
            , sessErrCallback = cbRef
            , sessBusy = busy
            }

initializeGhci :: Session -> (Text -> IO ()) -> IO ()
initializeGhci sess onLine = do
    clearGhciPrompt sess
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

{- | GHCi never reached the first marker: the @cabal repl@ build/configure
step died. Append the captured stderr (the real cause, e.g. a missing C
library) so the failure is diagnosable, not an opaque "exited during startup".
-}
startupErrorMessage :: Text -> String
startupErrorMessage detail
    | T.null (T.strip detail) = base
    | otherwise = base ++ ":\n" ++ T.unpack (T.strip detail)
  where
    base = "GHCi exited during startup"

-- | Brief grace for the stderr drain to flush the build log before we read it.
startupErrSettleUs :: Int
startupErrSettleUs = 200000

clearGhciPrompt :: Session -> IO ()
clearGhciPrompt sess = mapM_ (sendRaw sess) [":set prompt \"\"", ":set prompt-cont \"\""]

resetSession :: Session -> IO Session
resetSession sess = do
    closeSession sess
    newSession (sessConfig sess)

{- | Polite close: @:quit@, a short grace, then the 'destroySession'
chokepoint — which also reclaims handles, queue, and registry entry
when the quit succeeded.
-}
closeSession :: Session -> IO ()
closeSession sess = do
    _ <- timeout quitWriteGraceUs (sendQuit (sessStdin sess))
    _ <- timeout quitGraceUs (waitForProcess (sessProc sess))
    destroySession (sessProcSess sess)

quitGraceUs, quitWriteGraceUs :: Int
quitGraceUs = 2000000
quitWriteGraceUs = 1000000

{- | One try around the whole sequence: the enclosing 'timeout' throws
exactly once, so per-op handlers would swallow it and re-block on flush.
An aborted close is reclaimed by 'destroySession'.
-}
sendQuit :: Handle -> IO ()
sendQuit h =
    void
        ( try (hPutStrLn h ":quit" >> hFlush h >> hClose h) ::
            IO (Either SomeException ())
        )

{- | Adapter exposing the live GHCi 'Session' through the generic
'ST.SessionBackend' record-of-functions interface.
-}
ghciBackend :: Session -> ST.SessionBackend
ghciBackend sess =
    ST.SessionBackend
        { ST.sbSessionId = psId (sessProcSess sess)
        , ST.sbRunBlock = runBlock sess
        , ST.sbRunBlockStreaming = runBlockStreaming sess
        , ST.sbClose = closeSession sess
        , ST.sbReset = ghciBackend <$> resetSession sess
        , ST.sbInterrupt = interruptIfBusy sess
        , ST.sbQueryComplete = queryComplete sess
        , ST.sbQueryType = queryType sess
        , ST.sbQueryInfo = queryInfo sess
        , ST.sbQueryKind = queryKind sess
        , ST.sbQueryBrowse = queryBrowse sess
        , ST.sbQueryDoc = queryDoc sess
        }
