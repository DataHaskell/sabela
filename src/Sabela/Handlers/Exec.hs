{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Cell execution: build the GHCi script for a 'Cell', run it through the
'SessionBackend', store bridge exports, and turn the raw stdout/stderr into
a structured 'RunResult' + parsed 'CellError's that 'Sabela.Handlers.Plan'
broadcasts to the frontend.
-}
module Sabela.Handlers.Exec (
    runAndBroadcast,
    execCell,
    execCellWith,
    isReplCrash,
    buildGhciScript,
    storeBridgeExports,
    parseCellResult,
    classifyError,
) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Api (RunResult (..))
import Sabela.Bridge (bridgePreamble, isTemplateHaskellOutput, widgetPreamble)
import Sabela.Errors (parseErrors)
import Sabela.Handlers.Lifecycle (handleKernelCrash, loadSabelaPrelude)
import Sabela.Handlers.Shared
import Sabela.Model (
    Cell (..),
    CellError (..),
    Notebook (..),
    NotebookEvent (..),
    OutputItem (..),
    textToMime,
 )
import Sabela.Output (parseMimeOutputs)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.BridgeStore (getBridgeValues, setBridgeValue)
import Sabela.State.NotebookStore ()
import Sabela.State.SessionManager (getHaskellSession)
import Sabela.State.WidgetStore (getWidgetValues)
import ScriptHs.Parser (ScriptFile (..), parseScript)
import ScriptHs.Render (toGhciScript)

runAndBroadcast :: App -> Int -> Cell -> IO ()
runAndBroadcast app gen cell = do
    broadcast app (EvCellUpdating (cellId cell))
    loadSabelaPrelude app
    (result, errs) <- execCell app cell
    whenCurrentGen app gen $
        updateAndBroadcast
            app
            (\nb -> nb{nbCells = map (applyResult result) (nbCells nb)})
            (EvCellResult (rrCellId result) (rrOutputs result) (rrError result) errs)

execCell :: App -> Cell -> IO (RunResult, [CellError])
execCell app cell = do
    mSess <- getHaskellSession (appSessions app)
    case mSess of
        Nothing -> pure (RunResult (cellId cell) [] (Just "No GHCi session"), [])
        Just backend -> execCellWith app cell backend

execCellWith :: App -> Cell -> ST.SessionBackend -> IO (RunResult, [CellError])
execCellWith app cell backend = do
    ghci <- buildGhciScript app cell
    debugLog app $
        T.pack $
            "[handler] Cell " ++ show (cellId cell) ++ ":\n" ++ T.unpack ghci
    onLine <- mkStreamingCallback app (cellId cell)
    result <- try (ST.sbRunBlockStreaming backend ghci onLine)
    case result of
        Left (e :: SomeException) -> do
            handleKernelCrash app ("Kernel crashed: " <> T.pack (show e))
            pure
                (RunResult (cellId cell) [] (Just ("Kernel crashed: " <> T.pack (show e))), [])
        Right (rawOut, rawErr) -> do
            storeBridgeExports app rawOut
            (rr, errs) <- parseCellResult (cellId cell) rawOut rawErr
            when (isReplCrash rawErr) $
                handleKernelCrash app rawErr
            pure (rr, errs)

isReplCrash :: Text -> Bool
isReplCrash err = "repl failed" `T.isInfixOf` err

buildGhciScript :: App -> Cell -> IO Text
buildGhciScript app cell = do
    cellWidgets <- getWidgetValues (appWidgets app) (cellId cell)
    bridgeVals <- getBridgeValues (appBridge app)
    let preamble = widgetPreamble (cellId cell) cellWidgets <> bridgePreamble bridgeVals
        sf = scriptLines (parseScript (cellSource cell))
    pure (preamble <> toGhciScript sf)

storeBridgeExports :: App -> Text -> IO ()
storeBridgeExports app rawOut = do
    let (exports, _) = partitionExports (parseMimeOutputs rawOut)
    forM_ exports $ \(name, val) ->
        setBridgeValue (appBridge app) name (T.strip val)

parseCellResult :: Int -> Text -> Text -> IO (RunResult, [CellError])
parseCellResult cid rawOut rawErr = do
    let (_, normalItems) = partitionExports (parseMimeOutputs rawOut)
        outputs =
            [ OutputItem (textToMime m) b
            | (m, b) <- normalItems
            , not (T.null (T.strip b))
            ]
        errs = parseErrors rawErr
        actualErr = classifyError errs rawErr
    pure (RunResult cid outputs actualErr, errs)

classifyError :: [CellError] -> Text -> Maybe Text
classifyError errs rawErr
    | null errs && isTemplateHaskellOutput rawErr = Nothing
    | T.null rawErr = Nothing
    | otherwise = Just rawErr
