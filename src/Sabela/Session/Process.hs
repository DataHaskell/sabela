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

    -- * Generation
    firstSessionGen,
    bumpSessionGen,

    -- * Building blocks (exposed for tests)
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
newSessionStreaming = newSessionGen firstSessionGen

{- | Spawn a session tagged with an explicit generation. 'resetSession'
threads the next generation through here so a restart's backend reports a
strictly-higher tag than the one it replaced (stress case 36).
-}
newSessionGen :: Int -> SessionConfig -> (Text -> IO ()) -> IO Session
newSessionGen gen cfg onStartupLine = do
    spec <- ghciProcessSpec cfg
    withSpawnedSession spec $ \ps -> do
        sess <- buildSessionStateGen gen cfg ps onStartupLine
        initializeGhci sess onStartupLine
        mapM_ spawnParentPoller (psPgid ps)
        pure sess

{- | The generation every freshly-spawned (un-restarted) session is born
with; each 'resetSession' seeds the next one strictly above it.
-}
firstSessionGen :: Int
firstSessionGen = 1

-- | Advance and return the session's generation tag.
bumpSessionGen :: Session -> IO Int
bumpSessionGen sess =
    atomicModifyIORef' (sessionGen sess) (\g -> (g + 1, g + 1))

-- | The @cabal repl@ spec, in its own process group with piped handles.
ghciProcessSpec :: SessionConfig -> IO CreateProcess
ghciProcessSpec cfg = do
    mGhc <- lookupEnv "GHC"
    mCaps <- lookupEnv "SABELA_GHCI_CAPS"
    mHeap <- lookupEnv "SABELA_GHCI_MAXHEAP"
    let compilerArgs = ["--with-compiler=" ++ ghc | ghc <- maybeToList mGhc]
        args = ghciArgs cfg (rtsGhcOptions mCaps mHeap) ++ compilerArgs
    pure (sessionProcessSpec (Just (scWorkDir cfg)) (proc "cabal" args))

{- | Repl args. The prompt runs **interpreted** (byte-code) so a plain
notebook — and every restart — starts fast (incident K / stress case 26): the
unconditional @-fobject-code -O2 -fexpose-all-unfoldings@ that forced the whole
session into object code is gone. Each @-- compile@ module opts itself back
into @-O2@ object code via a per-module @OPTIONS_GHC@ pragma
('Sabela.Compiled.objectCodeOptions'). @--builddir@, @-odir@, and @-hidir@ are
pinned absolute under the project dir so the @:cd@ to the work dir at init can
neither orphan the @.o@\/@.hi@ cache nor scatter a @dist-newstyle@ into the
notebook's directory.
-}
ghciArgs :: SessionConfig -> String -> [String]
ghciArgs cfg rtsOpts =
    [ "repl"
    , "exe:main"
    , "--project-dir=" ++ scProjectDir cfg
    , "--builddir=" ++ scProjectDir cfg </> "dist-newstyle"
    , "-v1"
    , "--repl-options=-odir " ++ objDir ++ " -hidir " ++ objDir
    , "--ghc-options=" ++ rtsOpts
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
buildSessionState = buildSessionStateGen firstSessionGen

-- | 'buildSessionState' tagged with an explicit generation.
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
            }

{- | A 12-digit per-session nonce derived from the wall-clock picoseconds, kept
in a fixed digit band so every marker number is the same width. It seeds the
high digits of each marker so notebook output cannot forge the live boundary.
-}
mkSessionNonce :: IO Int
mkSessionNonce = do
    t <- getPOSIXTime
    let ps = round (t * 1e12) :: Integer
    pure (fromIntegral (ps `mod` 900000000000 + 100000000000))

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

{- | Kill and respawn the kernel, seeding the replacement with a strictly
higher generation than the session it replaces so a client can detect the
restart and discard any result tagged with the older generation.
-}
resetSession :: Session -> IO Session
resetSession sess = do
    prevGen <- readSessionGen sess
    closeSession sess
    newSessionGen (prevGen + 1) (sessConfig sess) (\_ -> pure ())

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
        , ST.sbBusy = isBusy sess
        , ST.sbSessionGen = readSessionGen sess
        , ST.sbRequestStale = isRequestStale sess
        , ST.sbQueryComplete = queryComplete sess
        , ST.sbQueryType = queryType sess
        , ST.sbQueryInfo = queryInfo sess
        , ST.sbQueryKind = queryKind sess
        , ST.sbQueryBrowse = queryBrowse sess
        , ST.sbQueryDoc = queryDoc sess
        }
