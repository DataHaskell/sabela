{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.Handlers.Python
    ( ensurePythonSessionAlive
    , killPythonSession
    , execPythonCell
    , executePythonCell
    , executePythonCells
    ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.Text as T
import Sabela.Api (RunResult (..))
import Sabela.Bridge (pythonBridgePreamble)
import Sabela.Model
    ( Cell (..)
    , CellError (..)
    , CellType (..)
    , Notebook (..)
    , NotebookEvent (..)
    , OutputItem (..)
    , SessionStatus (..)
    )
import Sabela.Output (parseMimeOutputs)
import Sabela.PythonSession (PythonSession, newPythonSession, pythonBackend)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.BridgeStore (getBridgeValues, setBridgeValue)
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getPythonSession, modifyPythonSession)

import Sabela.Handlers.Shared

ensurePythonSessionAlive :: App -> IO Bool
ensurePythonSessionAlive app =
    modifyPythonSession (appSessions app) $ \mSess ->
        case mSess of
            Just _ -> pure (mSess, True)
            Nothing -> createPythonSessionWithBroadcast app

createPythonSessionWithBroadcast :: App -> IO (Maybe ST.SessionBackend, Bool)
createPythonSessionWithBroadcast app = do
    broadcast app (EvSessionStatus SStarting)
    result <- try (newPythonSession (envWorkDir (appEnv app))) :: IO (Either SomeException PythonSession)
    case result of
        Left e -> handleSessionFailure app e
        Right pySess -> do
            broadcast app (EvSessionStatus SReady)
            pure (Just (pythonBackend pySess), True)

handleSessionFailure :: App -> SomeException -> IO (Maybe ST.SessionBackend, Bool)
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

runPythonBlock :: App -> ST.SessionBackend -> Cell -> IO (RunResult, [CellError])
runPythonBlock app backend cell = do
    bridgeVals <- getBridgeValues (appBridge app)
    let code = pythonBridgePreamble bridgeVals <> cellSource cell
    onLine <- mkStreamingCallback app (cellId cell)
    (rawOut, rawErr) <- ST.sbRunBlockStreaming backend code onLine
    parsePythonOutput app (cellId cell) rawOut rawErr

parsePythonOutput :: App -> Int -> T.Text -> T.Text -> IO (RunResult, [CellError])
parsePythonOutput app cid rawOut rawErr = do
    let items = parseMimeOutputs rawOut
        (exports, normalItems) = partitionExports items
        outputs = [OutputItem m b | (m, b) <- normalItems, not (T.null (T.strip b))]
    forM_ exports $ \(name, val) ->
        setBridgeValue (appBridge app) name (T.strip val)
    pure (RunResult cid outputs (if T.null rawErr then Nothing else Just rawErr), [])

executePythonCell :: App -> Int -> Int -> IO ()
executePythonCell app gen cid = do
    nb <- readNotebook (appNotebook app)
    forM_ (findPythonCell cid nb) (runSinglePythonCell app gen cid)

findPythonCell :: Int -> Notebook -> Maybe Cell
findPythonCell cid nb =
    find (\c -> cellId c == cid && cellType c == CodeCell && cellLang c == ST.Python) (nbCells nb)

runSinglePythonCell :: App -> Int -> Int -> Cell -> IO ()
runSinglePythonCell app gen cid cell = do
    broadcast app (EvCellUpdating cid)
    ok <- ensurePythonSessionAlive app
    if not ok
        then broadcastCellError app cid "Python session not available. Is 'python3' on PATH?"
        else tryExecAndBroadcast app gen cid cell

tryExecAndBroadcast :: App -> Int -> Int -> Cell -> IO ()
tryExecAndBroadcast app gen cid cell = do
    pyResult <- try (execPythonCell app cell) :: IO (Either SomeException (RunResult, [CellError]))
    let (result, errs) = unwrapExecResult cid pyResult
    whenCurrentGen app gen $
        updateAndBroadcast
            app
            (\nb' -> nb'{nbCells = map (applyResult result) (nbCells nb')})
            (EvCellResult (rrCellId result) (rrOutputs result) (rrError result) errs)

unwrapExecResult :: Int -> Either SomeException (RunResult, [CellError]) -> (RunResult, [CellError])
unwrapExecResult _ (Right r) = r
unwrapExecResult cid (Left e) = (RunResult cid [] (Just (T.pack (show e))), [])

executePythonCells :: App -> Int -> IO ()
executePythonCells app gen = do
    nb <- readNotebook (appNotebook app)
    let pyCells = filter (\c -> cellType c == CodeCell && cellLang c == ST.Python) (nbCells nb)
    unless (null pyCells) $ do
        ok <- ensurePythonSessionAlive app
        when ok $ forM_ pyCells (execCellInSequence app gen)

execCellInSequence :: App -> Int -> Cell -> IO ()
execCellInSequence app gen cell =
    whenCurrentGen app gen $ do
        broadcast app (EvCellUpdating (cellId cell))
        tryExecAndBroadcast app gen (cellId cell) cell
