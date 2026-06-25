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
    locateError,
) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Api (RunResult (..))
import Sabela.Bridge (bridgePreamble, isTemplateHaskellOutput, widgetPreamble)
import Sabela.Errors (parseErrors)
import Sabela.Errors.Json (annotateDefSites, parseJsonInteractive)
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
import Sabela.Parse (cellNames)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.BridgeStore (getBridgeValues, setBridgeValue)
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession)
import Sabela.State.WidgetStore (getWidgetValues)
import ScriptHs.Compiled (linePragmaTag)
import ScriptHs.Parser (ScriptFile (..), parseScriptNumbered)
import ScriptHs.Render (toGhciScript, toGhciScriptTagged)

runAndBroadcast :: App -> Int -> Cell -> IO ()
runAndBroadcast app gen cell = do
    broadcast app (EvCellUpdating (cellId cell))
    loadSabelaPrelude app
    (result, errs) <- execCell app cell
    whenCurrentGen app gen $
        updateAndBroadcast
            app
            (\nb -> nb{nbCells = map (applyResult result) (nbCells nb)})
            ( EvCellResult
                (rrCellId result)
                (rrOutputs result)
                (rrError result)
                errs
                (rrWarnings result)
            )

execCell :: App -> Cell -> IO (RunResult, [CellError])
execCell app cell = do
    mSess <- getHaskellSession (appSessions app)
    case mSess of
        Nothing -> pure (RunResult (cellId cell) [] (Just "No GHCi session") [], [])
        Just backend -> execCellWith app cell backend

execCellWith :: App -> Cell -> ST.SessionBackend -> IO (RunResult, [CellError])
execCellWith app cell backend = do
    let jsonOn = ST.sbJsonDiagnostics backend
    ghci <- buildGhciScript jsonOn app cell
    debugLog app $
        T.pack $
            "[handler] Cell " ++ show (cellId cell) ++ ":\n" ++ T.unpack ghci
    onLine <- mkStreamingCallback app (cellId cell)
    result <- try (ST.sbRunBlockStreaming backend ghci onLine)
    case result of
        Left (e :: SomeException) -> do
            handleKernelCrash app backend ("Kernel crashed: " <> T.pack (show e))
            pure
                ( RunResult
                    (cellId cell)
                    []
                    (Just ("Kernel crashed: " <> T.pack (show e)))
                    []
                , []
                )
        Right (rawOut, rawErr) -> do
            storeBridgeExports app rawOut
            parsed <- parseCellResult jsonOn (cellId cell) rawOut rawErr
            annotated <- annotateSuggestions app parsed
            when (isReplCrash rawErr) $
                handleKernelCrash app backend rawErr
            pure annotated

isReplCrash :: Text -> Bool
isReplCrash err = "repl failed" `T.isInfixOf` err

{- | Render a cell to a GHCi script, prepending the widget/bridge preamble.
With JSON diagnostics on (GHC ≥ 9.8), each block is tagged with a
@{\-# LINE … "sabela-cell-N" #-\}@ pragma so GHC reports cell-relative lines and
the cell file regardless of the preamble; otherwise the untagged renderer keeps
the pre-9.8 @<interactive>@ output the textual 'parseErrors' expects.
-}
buildGhciScript :: Bool -> App -> Cell -> IO Text
buildGhciScript jsonOn app cell = do
    cellWidgets <- getWidgetValues (appWidgets app) (cellId cell)
    bridgeVals <- getBridgeValues (appBridge app)
    let preamble = widgetPreamble (cellId cell) cellWidgets <> bridgePreamble bridgeVals
        (sf, numbered) = parseScriptNumbered (cellSource cell)
        body
            | jsonOn = toGhciScriptTagged (linePragmaTag (cellId cell)) numbered
            | otherwise = toGhciScript (scriptLines sf)
    pure (preamble <> body)

storeBridgeExports :: App -> Text -> IO ()
storeBridgeExports app rawOut = do
    let (exports, _) = partitionExports (parseMimeOutputs rawOut)
    forM_ exports $ \(name, val) ->
        setBridgeValue (appBridge app) name (T.strip val)

{- | Turn raw stdout\/stderr into a 'RunResult' + structured errors. With JSON
diagnostics on, stderr is NDJSON: errors and warnings are split by severity, and
the holistic 'rrError' is rebuilt from the error messages plus any non-JSON
residual (so warnings never read as a failure). Off, the textual 'parseErrors'
path is used unchanged and there are no warnings.
-}
parseCellResult :: Bool -> Int -> Text -> Text -> IO (RunResult, [CellError])
parseCellResult jsonOn cid rawOut rawErr = do
    let (_, normalItems) = partitionExports (parseMimeOutputs rawOut)
        outputs =
            [ OutputItem (textToMime m) b
            | (m, b) <- normalItems
            , not (T.null (T.strip b))
            ]
    pure $
        if jsonOn
            then
                let (errs, warns, residual) = parseJsonInteractive rawErr
                    pseudoRaw = T.unlines (map (locateError cid) errs) <> residual
                 in (RunResult cid outputs (classifyError errs pseudoRaw) warns, errs)
            else
                let errs = parseErrors rawErr
                 in (RunResult cid outputs (classifyError errs rawErr) [], errs)

{- | Resolve each @Perhaps use `name'@ suggestion in the result's messages to
the cell that defines @name@. Skips the notebook-wide def scan unless a message
actually carries a suggestion, so a clean or ordinary error pays nothing.
-}
annotateSuggestions ::
    App -> (RunResult, [CellError]) -> IO (RunResult, [CellError])
annotateSuggestions app res@(rr, errs)
    | not (any (T.isInfixOf "Perhaps") msgs) = pure res
    | otherwise = do
        resolve <- defSiteResolver app
        let ann = annotateDefSites resolve
        pure
            ( rr{rrError = ann <$> rrError rr}
            , [e{ceMessage = ann (ceMessage e)} | e <- errs]
            )
  where
    msgs = maybe [] pure (rrError rr) ++ map ceMessage errs

-- | A @name -> defining cell@ lookup over the current notebook's cells.
defSiteResolver :: App -> IO (Text -> Maybe Int)
defSiteResolver app = do
    nb <- readNotebook (appNotebook app)
    let defMap =
            M.fromList
                [ (name, cellId c)
                | c <- nbCells nb
                , name <- S.toList (fst (cellNames (cellSource c)))
                ]
    pure (`M.lookup` defMap)

{- | Prefix a diagnostic's message with its cell-relative location for the
holistic @error@ display, e.g. @cell 12, line 1: Variable not in scope …@.
The error's own line is otherwise invisible in the joined text (GHC's hints
abbreviate other definition sites to a bare @(line N)@), and the cell number
matches the badge the editor shows, so a notebook of single-line cells no longer
reads as "line 1" everywhere.
-}
locateError :: Int -> CellError -> Text
locateError cid e = "cell " <> tShow cid <> linePart <> ": " <> ceMessage e
  where
    linePart = maybe "" (\l -> ", line " <> tShow l) (ceLine e)
    tShow = T.pack . show

{- | Decide whether raw stderr counts as a cell failure. Template Haskell
chatter and linker noise (macOS @ld: warning:@ lines emitted when GHCi
links native code) are harmless and never flag the cell.
-}
classifyError :: [CellError] -> Text -> Maybe Text
classifyError errs rawErr
    | null errs && isTemplateHaskellOutput rawErr = Nothing
    | T.null cleaned = Nothing
    | otherwise = Just cleaned
  where
    cleaned =
        T.strip . T.unlines . filter (not . isLinkerNoise) . T.lines $ rawErr
    isLinkerNoise l = "ld: warning:" `T.isPrefixOf` T.strip l
