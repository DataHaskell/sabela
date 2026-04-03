{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.Handlers.Python (
    ensurePythonSessionAlive,
    killPythonSession,
    execPythonCell,
    executePythonCell,
    executePythonCells,
    collectPythonDeps,
) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Sabela.Api (RunResult (..))
import Sabela.Bridge (pythonBridgePreamble)
import Sabela.Model (
    Cell (..),
    CellError (..),
    CellType (..),
    Notebook (..),
    NotebookEvent (..),
    OutputItem (..),
    SessionStatus (..),
 )
import Sabela.Output (parseMimeOutputs)
import Sabela.PythonSession (PythonSession, newPythonSession, pythonBackend)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.BridgeStore (getBridgeValues, setBridgeValue)
import Sabela.State.DependencyTracker (getPythonDeps, setPythonDeps)
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getPythonSession, modifyPythonSession)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import qualified System.IO
import qualified System.Process

import Sabela.Handlers.Shared

collectPythonDeps :: Notebook -> S.Set Text
collectPythonDeps nb =
    let pyCells = filter isPythonCodeCell (nbCells nb)
     in S.fromList (concatMap (parsePipDeps . cellSource) pyCells)

isPythonCodeCell :: Cell -> Bool
isPythonCodeCell c = cellType c == CodeCell && cellLang c == ST.Python

parsePipDeps :: T.Text -> [T.Text]
parsePipDeps src =
    [ T.strip rest
    | line <- T.lines src
    , Just rest <- [T.stripPrefix "# pip:" (T.strip line)]
    , not (T.null (T.strip rest))
    ]

ensurePythonSessionAlive :: App -> IO Bool
ensurePythonSessionAlive app =
    modifyPythonSession (appSessions app) $ \mSess -> do
        nb <- readNotebook (appNotebook app)
        let required = collectPythonDeps nb
        installed <- getPythonDeps (appDeps app)
        case (mSess, required == installed) of
            (Just _, True) -> pure (mSess, True)
            _ -> startPythonSession app required mSess

startPythonSession ::
    App ->
    S.Set Text ->
    Maybe ST.SessionBackend ->
    IO (Maybe ST.SessionBackend, Bool)
startPythonSession app deps mOldSess = do
    forM_ mOldSess $ \s ->
        void (try (ST.sbClose s) :: IO (Either SomeException ()))
    let venvDir = envTmpDir (appEnv app) </> "python-venv"
    depsOk <- installIfNeeded app venvDir deps
    if not depsOk
        then pure (Nothing, False)
        else createPythonWithVenv app venvDir deps

installIfNeeded :: App -> FilePath -> S.Set Text -> IO Bool
installIfNeeded app venvDir deps
    | S.null deps = pure True
    | otherwise = do
        broadcast app (EvSessionStatus (SUpdateDeps (S.toList deps)))
        ensureVenv app venvDir
        pipInstall app venvDir deps

ensureVenv :: App -> FilePath -> IO ()
ensureVenv app venvDir = do
    exists <- doesDirectoryExist venvDir
    unless exists $ do
        broadcast app (EvInstallLog "Creating Python virtual environment...")
        void $
            System.Process.readCreateProcess
                (System.Process.proc "python3" ["-m", "venv", venvDir])
                ""

pipInstall :: App -> FilePath -> S.Set Text -> IO Bool
pipInstall app venvDir deps = do
    broadcast app (EvInstallLog "Installing Python packages...")
    let pip = venvDir </> "bin" </> "pip"
        args = "install" : "--quiet" : map T.unpack (S.toList deps)
    result <- try (runAndLog app pip args) :: IO (Either SomeException ())
    case result of
        Right () -> pure True
        Left e -> do
            broadcast app (EvInstallLog ("pip install failed: " <> T.pack (show e)))
            broadcast app (EvSessionStatus SReset)
            pure False

runAndLog :: App -> FilePath -> [String] -> IO ()
runAndLog app cmd args = do
    let cp =
            (System.Process.proc cmd args)
                { System.Process.std_out = System.Process.CreatePipe
                , System.Process.std_err = System.Process.CreatePipe
                }
    (_, mOut, mErr, ph) <- System.Process.createProcess cp
    forM_ mOut $ \h -> void $ forkIO $ broadcastLines app h
    forM_ mErr $ \h -> void $ forkIO $ broadcastLines app h
    _ <- System.Process.waitForProcess ph
    pure ()

broadcastLines :: App -> System.IO.Handle -> IO ()
broadcastLines app h = do
    result <- try (TIO.hGetContents h) :: IO (Either SomeException T.Text)
    case result of
        Left _ -> pure ()
        Right content ->
            forM_ (filter (not . T.null) (T.lines content)) $ \line ->
                broadcast app (EvInstallLog line)

createPythonWithVenv ::
    App -> FilePath -> S.Set Text -> IO (Maybe ST.SessionBackend, Bool)
createPythonWithVenv app venvDir deps = do
    broadcast app (EvSessionStatus SStarting)
    let mVenv = if S.null deps then Nothing else Just venvDir
    result <-
        try (newPythonSession mVenv (envWorkDir (appEnv app))) ::
            IO (Either SomeException PythonSession)
    case result of
        Left e -> handleSessionFailure app e
        Right pySess -> do
            setPythonDeps (appDeps app) deps
            broadcast app (EvSessionStatus SReady)
            pure (Just (pythonBackend pySess), True)

handleSessionFailure ::
    App -> SomeException -> IO (Maybe ST.SessionBackend, Bool)
handleSessionFailure app e = do
    let msg = T.pack (show e)
    debugLog app $ "[handler] Python session failed: " <> msg
    broadcast app (EvInstallLog ("Python session failed: " <> msg))
    broadcast app (EvSessionStatus SReset)
    pure (Nothing, False)

killPythonSession :: App -> IO ()
killPythonSession app =
    void $
        modifyPythonSession (appSessions app) $ \mSess -> do
            forM_ mSess $ \s ->
                void (try (ST.sbClose s) :: IO (Either SomeException ()))
            pure (Nothing, ())

execPythonCell :: App -> Cell -> IO (RunResult, [CellError])
execPythonCell app cell = do
    mSess <- getPythonSession (appSessions app)
    case mSess of
        Nothing ->
            pure (RunResult (cellId cell) [] (Just "No Python session"), [])
        Just backend -> runPythonBlock app backend cell

runPythonBlock ::
    App -> ST.SessionBackend -> Cell -> IO (RunResult, [CellError])
runPythonBlock app backend cell = do
    bridgeVals <- getBridgeValues (appBridge app)
    let code = pythonBridgePreamble bridgeVals <> cellSource cell
    onLine <- mkStreamingCallback app (cellId cell)
    (rawOut, rawErr) <- ST.sbRunBlockStreaming backend code onLine
    parsePythonOutput app (cellId cell) rawOut rawErr

parsePythonOutput ::
    App -> Int -> T.Text -> T.Text -> IO (RunResult, [CellError])
parsePythonOutput app cid rawOut rawErr = do
    let items = parseMimeOutputs rawOut
        (exports, normalItems) = partitionExports items
        outputs = [OutputItem m b | (m, b) <- normalItems, not (T.null (T.strip b))]
    forM_ exports $ \(name, val) ->
        setBridgeValue (appBridge app) name (T.strip val)
    pure
        (RunResult cid outputs (if T.null rawErr then Nothing else Just rawErr), [])

executePythonCell :: App -> Int -> Int -> IO ()
executePythonCell app gen cid = do
    nb <- readNotebook (appNotebook app)
    forM_ (findPythonCell cid nb) (runSinglePythonCell app gen cid)

findPythonCell :: Int -> Notebook -> Maybe Cell
findPythonCell cid nb =
    find
        (\c -> cellId c == cid && cellType c == CodeCell && cellLang c == ST.Python)
        (nbCells nb)

runSinglePythonCell :: App -> Int -> Int -> Cell -> IO ()
runSinglePythonCell app gen cid cell = do
    broadcast app (EvCellUpdating cid)
    ok <- ensurePythonSessionAlive app
    if not ok
        then
            broadcastCellError app cid "Python session not available. Is 'python3' on PATH?"
        else tryExecAndBroadcast app gen cid cell

tryExecAndBroadcast :: App -> Int -> Int -> Cell -> IO ()
tryExecAndBroadcast app gen cid cell = do
    pyResult <-
        try (execPythonCell app cell) ::
            IO (Either SomeException (RunResult, [CellError]))
    let (result, errs) = unwrapExecResult cid pyResult
    whenCurrentGen app gen $
        updateAndBroadcast
            app
            (\nb' -> nb'{nbCells = map (applyResult result) (nbCells nb')})
            (EvCellResult (rrCellId result) (rrOutputs result) (rrError result) errs)

unwrapExecResult ::
    Int -> Either SomeException (RunResult, [CellError]) -> (RunResult, [CellError])
unwrapExecResult _ (Right r) = r
unwrapExecResult cid (Left e) = (RunResult cid [] (Just (T.pack (show e))), [])

executePythonCells :: App -> Int -> IO ()
executePythonCells app gen = do
    nb <- readNotebook (appNotebook app)
    let pyCells = filter isPythonCodeCell (nbCells nb)
    unless (null pyCells) $ do
        ok <- ensurePythonSessionAlive app
        when ok $ forM_ pyCells (execCellInSequence app gen)

execCellInSequence :: App -> Int -> Cell -> IO ()
execCellInSequence app gen cell =
    whenCurrentGen app gen $ do
        broadcast app (EvCellUpdating (cellId cell))
        tryExecAndBroadcast app gen (cellId cell) cell
