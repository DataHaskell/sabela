{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.AI.Capabilities (
    -- * Tool definitions
    chatTools,

    -- * Tool execution
    executeTool,

    -- * Edit lifecycle
    acceptEdit,
    revertEdit,
) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, newTVarIO, readTChan, writeTVar)
import Control.Exception (SomeException, try)
import Data.Aeson (
    ToJSON (..),
    Value (..),
    object,
    (.=),
 )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (atomicModifyIORef')
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Temp (createTempDirectory)
import System.Timeout (timeout)

import Sabela.AI.Doc (cellHash)
import Sabela.AI.Handles (
    HandleId (..),
    LargeResult (..),
    grepLines,
    headLines,
    lookupHandle,
    sliceLines,
    storeLargeResult,
    summarizeForLLM,
    tailLines,
 )
import Sabela.AI.ReferenceCard (sliceApiReference)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Anthropic.Types (
    CacheControl (..),
    CancelToken,
    ToolDef (..),
    isCancelled,
    newCancelToken,
 )
import Sabela.Handlers (ReactiveNotebook (..))
import Sabela.Model
import Sabela.Output (displayPrelude)
import Sabela.PythonSession (newPythonSession, pythonBackend)
import Sabela.Session (
    SessionConfig (..),
    closeSession,
    ghciBackend,
    newSession,
    readErrorBuffer,
    runBlock,
 )
import Sabela.SessionTypes (CellLang (..), SessionBackend (..))
import Sabela.State
import qualified ScriptHs.Parser as Scripths
import qualified ScriptHs.Render as Scripths

------------------------------------------------------------------------
-- Tool definitions
------------------------------------------------------------------------

chatTools :: [ToolDef]
chatTools = withLastCached rawChatTools

{- | Set cache_control on the last tool so all tool schemas join the cached
prefix. Tool definitions are extremely stable → 1-hour TTL.
-}
withLastCached :: [ToolDef] -> [ToolDef]
withLastCached xs = case reverse xs of
    [] -> []
    (t : rest) -> reverse (t{tdCacheControl = Just EphemeralHour} : rest)

-- | Construct a ToolDef with no cache_control; withLastCached sets it on the last.
mkTool :: Text -> Text -> Value -> ToolDef
mkTool name desc schema = ToolDef name desc schema Nothing

rawChatTools :: [ToolDef]
rawChatTools =
    [ mkTool
        "list_cells"
        "List all cells in the notebook with their IDs, types, languages, first line of source, and whether they have errors. Use this for a quick overview."
        (object ["type" .= ("object" :: Text), "properties" .= object []])
    , mkTool
        "read_cell"
        "Read the full source code and outputs of a specific cell."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "cell_id"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("The cell ID to read" :: Text)
                            ]
                    ]
            , "required" .= (["cell_id"] :: [Text])
            ]
        )
    , mkTool
        "read_cell_output"
        "Read only the outputs and error of a cell (not the source). Useful for checking execution results."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "cell_id"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("The cell ID" :: Text)
                            ]
                    ]
            , "required" .= (["cell_id"] :: [Text])
            ]
        )
    , mkTool
        "find_cells_by_content"
        "Search cell sources for a pattern (substring match). Returns matching cell IDs with context."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "pattern"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("Substring to search for in cell sources" :: Text)
                            ]
                    ]
            , "required" .= (["pattern"] :: [Text])
            ]
        )
    , mkTool
        "replace_cell_source"
        "Replace the source of a cell you own and AUTO-RUN it. Use this for iterating on cells you inserted yourself — no user approval round-trip. For Haskell code cells the tool response includes an `execution` field {ran, ok, outputs, error, errors}; read it before moving on. Pass expected_hash to detect drift. For user-authored changes that deserve review, use propose_edit instead."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "cell_id"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("The cell ID to replace" :: Text)
                            ]
                    , "new_source"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("The new source code — full replacement, not a diff" :: Text)
                            ]
                    , "expected_hash"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description"
                                .= ( "Optional: the cell hash you last saw. If it does not match, the edit is rejected and you should re-read the cell." ::
                                        Text
                                   )
                            ]
                    ]
            , "required" .= (["cell_id", "new_source"] :: [Text])
            ]
        )
    , mkTool
        "propose_edit"
        "Propose a source code change for a cell. The change is NOT applied immediately — the user must accept it. Use for edits to cells the user authored where review matters; for your own scaffolding cells prefer replace_cell_source which applies + runs in one step. Returns an editId for tracking. Pass expected_hash to detect drift."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "cell_id"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("The cell ID to edit" :: Text)
                            ]
                    , "new_source"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("The new source code" :: Text)
                            ]
                    , "expected_hash"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description"
                                .= ( "Optional: the cell hash you last saw. If it does not match the current hash, the edit is rejected and you should re-read the cell." ::
                                        Text
                                   )
                            ]
                    ]
            , "required" .= (["cell_id", "new_source"] :: [Text])
            ]
        )
    , mkTool
        "insert_cell"
        "Insert a new cell into the notebook. Applied immediately."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "after_cell_id"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("Insert after this cell ID. Use -1 for beginning." :: Text)
                            ]
                    , "cell_type"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "enum" .= (["CodeCell", "ProseCell"] :: [Text])
                            ]
                    , "language"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "enum" .= (["Haskell", "Python"] :: [Text])
                            ]
                    , "source"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("Source code or markdown" :: Text)
                            ]
                    ]
            , "required" .= (["after_cell_id", "source"] :: [Text])
            ]
        )
    , mkTool
        "delete_cell"
        "Delete a cell from the notebook."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "cell_id"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("The cell ID to delete" :: Text)
                            ]
                    ]
            , "required" .= (["cell_id"] :: [Text])
            ]
        )
    , mkTool
        "execute_cell"
        "Execute a cell and wait for its result. Returns the outputs and any errors."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "cell_id"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("The cell ID to execute" :: Text)
                            ]
                    ]
            , "required" .= (["cell_id"] :: [Text])
            ]
        )
    , mkTool
        "scratchpad"
        "Run code in an isolated scratchpad session. Does NOT modify the notebook. The scratchpad has the same packages available but separate state. Use this for standalone experiments."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "code"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("Code to execute" :: Text)
                            ]
                    , "language"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "enum" .= (["Haskell", "Python"] :: [Text])
                            , "description" .= ("Language. Default: Haskell." :: Text)
                            ]
                    ]
            , "required" .= (["code"] :: [Text])
            ]
        )
    , mkTool
        "ghci_query"
        "Lightweight GHCi introspection against the live Haskell session: type, info, kind, doc, or browse a module. Much cheaper than execute_cell for syntax discovery. Use `browse` with a module name (e.g. \"DataFrame\") to list exports."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "op"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "enum" .= (["type", "info", "kind", "browse", "doc"] :: [Text])
                            , "description"
                                .= ("Which GHCi command to run (:type, :info, :kind, :browse, :doc)." :: Text)
                            ]
                    , "arg"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description"
                                .= ( "For type/info/kind/doc: an expression, name, or type. For browse: a module name like \"DataFrame\"." ::
                                        Text
                                   )
                            ]
                    ]
            , "required" .= (["op", "arg"] :: [Text])
            ]
        )
    , mkTool
        "api_reference"
        "Fetch signatures for DataFrame, DataFrame.Functions, DataFrame.Display.Web.Plot, or Granite.Svg. Pass a module name (substring match on the section header) to get that module's section, or omit/empty to get all. Output is cleaned :browse output. Use this before writing dataframe or granite code if you're uncertain of a signature."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "module"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description"
                                .= ( "Substring of the module name, e.g. \"DataFrame\", \"Functions\", \"Plot\", \"Granite\". Empty for all sections." ::
                                        Text
                                   )
                            ]
                    ]
            ]
        )
    , mkTool
        "explore_result"
        "Drill into a large result returned by a previous tool call. Use this when a tool returned a handleId instead of inlining the full payload."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "handle_id"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("The handleId from a prior tool result." :: Text)
                            ]
                    , "op"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "enum" .= (["head", "tail", "slice", "grep"] :: [Text])
                            ]
                    , "n"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("For head/tail: number of lines." :: Text)
                            ]
                    , "from"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("For slice: 1-based start line (inclusive)." :: Text)
                            ]
                    , "to"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("For slice: 1-based end line (inclusive)." :: Text)
                            ]
                    , "pattern"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("For grep: substring to search for." :: Text)
                            ]
                    ]
            , "required" .= (["handle_id", "op"] :: [Text])
            ]
        )
    ]

------------------------------------------------------------------------
-- Tool execution dispatch
------------------------------------------------------------------------

executeTool ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    CancelToken ->
    Text ->
    Value ->
    -- | (result, isError)
    IO (Value, Bool)
executeTool app store rn cancelTok toolName input
    | Just parseErr <- lookupParseError input =
        pure
            ( object
                [ "error" .= ("Could not parse tool arguments." :: Text)
                , "hint" .= parseErr
                ]
            , True
            )
    | otherwise = do
        eResult <- try $ case toolName of
            "list_cells" -> execListCells app
            "read_cell" -> execReadCell app input
            "read_cell_output" -> execReadCellOutput app input
            "find_cells_by_content" -> execFindCells app input
            "propose_edit" -> execProposeEdit app store input
            "replace_cell_source" -> execReplaceCellSource app store rn cancelTok input
            "insert_cell" -> execInsertCell app store rn cancelTok input
            "delete_cell" -> execDeleteCell app input
            "execute_cell" -> execExecuteCell app store rn cancelTok input
            "scratchpad" -> execScratchpadGuarded app store input
            "ghci_query" -> execGhciQuery app input
            "api_reference" -> execApiReference input
            "explore_result" -> execExploreResult store input
            _ -> pure (object ["error" .= ("Unknown tool: " <> toolName)], True)
        case eResult of
            Left (e :: SomeException) ->
                pure (object ["error" .= T.pack (show e)], True)
            Right r -> pure r

{- | Return the parse-error hint planted by the orchestrator when a streamed
tool_use JSON couldn't be decoded. Dispatch fails fast in that case rather
than letting downstream tools report misleading "cell_id required" errors.
-}
lookupParseError :: Value -> Maybe Text
lookupParseError v = case field "_parseError" v of
    Just (String s) -> Just s
    _ -> Nothing

------------------------------------------------------------------------
-- Tool implementations
------------------------------------------------------------------------

-- Helper to extract fields from JSON Value
field :: Text -> Value -> Maybe Value
field key (Object o) = KM.lookup (Key.fromText key) o
field _ _ = Nothing

fieldText :: Text -> Value -> Text
fieldText key v = case field key v of
    Just (String s) -> s
    _ -> ""

fieldInt :: Text -> Value -> Maybe Int
fieldInt key v = case field key v of
    Just (Number n) -> Just (round n)
    _ -> Nothing

execListCells :: App -> IO (Value, Bool)
execListCells app = do
    nb <- readNotebook (appNotebook app)
    let summaries = zipWith summarize [1 :: Int ..] (nbCells nb)
    pure (object ["title" .= nbTitle nb, "cells" .= summaries], False)
  where
    summarize pos c =
        object
            [ "id" .= cellId c
            , "hash" .= cellHash c
            , "position" .= pos
            , "type" .= cellType c
            , "lang" .= cellLang c
            , "firstLine" .= T.take 80 (head' (T.lines (cellSource c)))
            , "hasError" .= isJust (cellError c)
            , "dirty" .= cellDirty c
            ]
    head' [] = ""
    head' (x : _) = x

execReadCell :: App -> Value -> IO (Value, Bool)
execReadCell app input = do
    let mcid = fieldInt "cell_id" input
    case mcid of
        Nothing -> pure (object ["error" .= ("cell_id required" :: Text)], True)
        Just cid -> do
            nb <- readNotebook (appNotebook app)
            case lookupCell cid nb of
                Nothing ->
                    pure (object ["error" .= ("Cell not found: " <> T.pack (show cid))], True)
                Just c ->
                    pure
                        ( object
                            [ "id" .= cellId c
                            , "hash" .= cellHash c
                            , "type" .= cellType c
                            , "lang" .= cellLang c
                            , "source" .= cellSource c
                            , "outputs" .= cellOutputs c
                            , "error" .= cellError c
                            ]
                        , False
                        )

execReadCellOutput :: App -> Value -> IO (Value, Bool)
execReadCellOutput app input = do
    let mcid = fieldInt "cell_id" input
    case mcid of
        Nothing -> pure (object ["error" .= ("cell_id required" :: Text)], True)
        Just cid -> do
            nb <- readNotebook (appNotebook app)
            case lookupCell cid nb of
                Nothing ->
                    pure (object ["error" .= ("Cell not found: " <> T.pack (show cid))], True)
                Just c ->
                    pure
                        ( object
                            [ "id" .= cellId c
                            , "outputs" .= cellOutputs c
                            , "error" .= cellError c
                            ]
                        , False
                        )

execFindCells :: App -> Value -> IO (Value, Bool)
execFindCells app input = do
    let pat = fieldText "pattern" input
    if T.null pat
        then pure (object ["error" .= ("pattern required" :: Text)], True)
        else do
            nb <- readNotebook (appNotebook app)
            let matches = mapMaybe (matchCell pat) (nbCells nb)
            pure (object ["matches" .= matches], False)
  where
    matchCell pat c
        | pat `T.isInfixOf` cellSource c =
            let ls = zip [1 :: Int ..] (T.lines (cellSource c))
                matchingLines =
                    [ object ["line" .= n, "text" .= T.take 120 l]
                    | (n, l) <- ls
                    , pat `T.isInfixOf` l
                    ]
             in Just $
                    object
                        [ "id" .= cellId c
                        , "lang" .= cellLang c
                        , "matchingLines" .= take 5 matchingLines
                        ]
        | otherwise = Nothing

{- | Direct cell-source replacement: apply immediately (no user approval),
broadcast the change, auto-run the cell for Haskell code cells, and return
a compact execution summary. Analogous to 'execInsertCell' but for an
existing cell. Intended for AI-internal iteration.
-}
execReplaceCellSource ::
    App -> AIStore -> ReactiveNotebook -> CancelToken -> Value -> IO (Value, Bool)
execReplaceCellSource app store rn cancelTok input = do
    let mcid = fieldInt "cell_id" input
        newSrc = fieldText "new_source" input
        mExpected = case field "expected_hash" input of
            Just (String s) | not (T.null s) -> Just s
            _ -> Nothing
    case mcid of
        Nothing ->
            pure (object ["error" .= ("cell_id required" :: Text)], True)
        Just cid -> do
            nb <- readNotebook (appNotebook app)
            case lookupCell cid nb of
                Nothing ->
                    pure
                        ( object ["error" .= ("Cell not found: " <> T.pack (show cid))]
                        , True
                        )
                Just c -> case mExpected of
                    Just expected
                        | cellHash c /= expected ->
                            pure
                                ( object
                                    [ "error" .= ("Hash mismatch — re-read the cell and retry." :: Text)
                                    , "cellId" .= cid
                                    , "currentHash" .= cellHash c
                                    , "expectedHash" .= expected
                                    ]
                                , True
                                )
                    _ -> applyReplaceCellSource app store rn cancelTok c newSrc

applyReplaceCellSource ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    CancelToken ->
    Cell ->
    Text ->
    IO (Value, Bool)
applyReplaceCellSource app store rn cancelTok oldCell newSrc = do
    let cid = cellId oldCell
        newCell =
            oldCell
                { cellSource = newSrc
                , cellDirty = True
                , cellOutputs = []
                , cellError = Nothing
                }
    modifyNotebook (appNotebook app) $ \nb ->
        nb
            { nbCells =
                map
                    (\c -> if cellId c == cid then newCell else c)
                    (nbCells nb)
            }
    broadcastNotebook app
    execSummary <-
        if cellType newCell == CodeCell
            && cellLang newCell == Haskell
            && not (T.null (T.strip newSrc))
            then autoExecuteAfterMutation app store rn cancelTok cid
            else pure Null
    pure
        ( object
            [ "cellId" .= cid
            , "hash" .= cellHash newCell
            , "execution" .= execSummary
            ]
        , False
        )

execProposeEdit :: App -> AIStore -> Value -> IO (Value, Bool)
execProposeEdit app store input = do
    let mcid = fieldInt "cell_id" input
        newSrc = fieldText "new_source" input
        mExpected = case field "expected_hash" input of
            Just (String s) | not (T.null s) -> Just s
            _ -> Nothing
    case mcid of
        Nothing -> pure (object ["error" .= ("cell_id required" :: Text)], True)
        Just cid -> do
            nb <- readNotebook (appNotebook app)
            case lookupCell cid nb of
                Nothing ->
                    pure (object ["error" .= ("Cell not found: " <> T.pack (show cid))], True)
                Just c -> case mExpected of
                    Just expected
                        | cellHash c /= expected ->
                            pure
                                ( object
                                    [ "error"
                                        .= ( "Hash mismatch — cell has changed since you last read it. Re-read and retry." ::
                                                Text
                                           )
                                    , "cellId" .= cid
                                    , "currentHash" .= cellHash c
                                    , "expectedHash" .= expected
                                    ]
                                , True
                                )
                    _ -> proceedProposeEdit app store c newSrc

proceedProposeEdit :: App -> AIStore -> Cell -> Text -> IO (Value, Bool)
proceedProposeEdit app store c newSrc = do
    let cid = cellId c
    eid <- atomicModifyIORef' (aiNextEditId store) (\n -> (n + 1, n))
    let editId = EditId eid
    statusVar <- newTVarIO Pending
    mTurn <- getCurrentTurn store
    let tid = maybe (TurnId 0) turnId mTurn
        edit =
            AiEdit
                { aeEditId = editId
                , aeCellId = cid
                , aeOldSource = cellSource c
                , aeNewSource = newSrc
                , aeStatus = statusVar
                , aeTurnId = tid
                }
    addPendingEdit store edit
    let TurnId tidInt = tid
    broadcast (appEvents app) $
        EvChatEditProposed tidInt eid cid (cellSource c) newSrc
    pure
        ( object
            [ "editId" .= eid
            , "cellId" .= cid
            , "status" .= ("pending" :: Text)
            ]
        , False
        )

execInsertCell ::
    App -> AIStore -> ReactiveNotebook -> CancelToken -> Value -> IO (Value, Bool)
execInsertCell app store rn cancelTok input = do
    let afterId = fromMaybe (-1) (fieldInt "after_cell_id" input)
        src = fieldText "source" input
        cellTp = case fieldText "cell_type" input of
            "ProseCell" -> ProseCell
            _ -> CodeCell
        lang = case fieldText "language" input of
            "Python" -> Python
            _ -> Haskell
    nid <- freshCellId (appNotebook app)
    let cell = Cell nid cellTp lang src [] Nothing True
    modifyNotebook (appNotebook app) $ \nb ->
        nb{nbCells = ins afterId cell (nbCells nb)}
    broadcastNotebook app
    execSummary <-
        if cellTp == CodeCell && lang == Haskell && not (T.null (T.strip src))
            then autoExecuteAfterMutation app store rn cancelTok nid
            else pure Null
    pure
        ( object
            [ "cellId" .= nid
            , "hash" .= cellHash cell
            , "execution" .= execSummary
            ]
        , False
        )
  where
    ins (-1) c cs = c : cs
    ins _ c [] = [c]
    ins aid c (x : xs)
        | cellId x == aid = x : c : xs
        | otherwise = x : ins aid c xs

execDeleteCell :: App -> Value -> IO (Value, Bool)
execDeleteCell app input = do
    let mcid = fieldInt "cell_id" input
    case mcid of
        Nothing -> pure (object ["error" .= ("cell_id required" :: Text)], True)
        Just cid -> do
            modifyNotebook (appNotebook app) $ \nb ->
                nb{nbCells = filter (\c -> cellId c /= cid) (nbCells nb)}
            broadcastNotebook app
            pure (object ["deleted" .= True, "cellId" .= cid], False)

{- | Run a single cell via the reactive notebook, compact the result, and
return a JSON summary suitable for embedding in a mutation-tool response.
Errors are surfaced; large outputs/errors are routed through the handle store.
-}
autoExecuteAfterMutation ::
    App -> AIStore -> ReactiveNotebook -> CancelToken -> Int -> IO Value
autoExecuteAfterMutation app store rn cancelTok cid = do
    res <- executeCell app rn cid cancelTok
    case res of
        Left err ->
            pure $
                object
                    [ "ran" .= True
                    , "ok" .= False
                    , "error" .= err
                    ]
        Right er -> do
            outputsField <- compactOutputs store (erOutputs er)
            errorField <- compactMaybeText store (erError er)
            let ok = null (erErrors er) && isNothing (erError er)
            pure $
                object
                    [ "ran" .= True
                    , "ok" .= ok
                    , "outputs" .= outputsField
                    , "error" .= errorField
                    , "errors" .= erErrors er
                    ]

execExecuteCell ::
    App -> AIStore -> ReactiveNotebook -> CancelToken -> Value -> IO (Value, Bool)
execExecuteCell app store rn cancelTok input = do
    let mcid = fieldInt "cell_id" input
    case mcid of
        Nothing -> pure (object ["error" .= ("cell_id required" :: Text)], True)
        Just cid -> do
            result <- executeCell app rn cid cancelTok
            case result of
                Left err -> pure (object ["error" .= err], True)
                Right er -> do
                    outputsField <- compactOutputs store (erOutputs er)
                    errorField <- compactMaybeText store (erError er)
                    pure
                        ( object
                            [ "outputs" .= outputsField
                            , "error" .= errorField
                            , "errors" .= erErrors er
                            , "cellId" .= cid
                            ]
                        , False
                        )

-- | Render outputs compactly: large individual outputs are swapped for a handle.
compactOutputs :: AIStore -> [OutputItem] -> IO Value
compactOutputs store items = do
    compacted <- mapM compactOne items
    pure (toJSON compacted)
  where
    compactOne oi = do
        r <- storeLargeResult (aiHandles store) (oiOutput oi)
        case r of
            Left cleaned ->
                pure $
                    object
                        [ "mime" .= oiMime oi
                        , "output" .= cleaned
                        ]
            Right (hid, summary, nLines, nBytes) ->
                pure $
                    object
                        [ "mime" .= oiMime oi
                        , "large" .= summarizeForLLM hid summary nLines nBytes
                        ]

compactMaybeText :: AIStore -> Maybe Text -> IO Value
compactMaybeText _ Nothing = pure Null
compactMaybeText store (Just t) = do
    r <- storeLargeResult (aiHandles store) t
    pure $ case r of
        Left cleaned -> String cleaned
        Right (hid, summary, nLines, nBytes) -> summarizeForLLM hid summary nLines nBytes

executeCell ::
    App ->
    ReactiveNotebook ->
    Int ->
    CancelToken ->
    IO (Either Text ExecutionResult)
executeCell app rn cid cancelTok = do
    resultVar <- newEmptyMVar
    listenerThread <- forkIO $ do
        chan <- subscribeBroadcast (appEvents app)
        let loop = do
                ev <- atomically $ readTChan chan
                case ev of
                    EvCellResult rid outputs err errs
                        | rid == cid ->
                            putMVar resultVar (ExecutionResult outputs err errs)
                    _ -> loop
        loop
    rnRunCell rn cid
    mResult <- timeout 130000000 (takeMVar resultVar)
    killThread listenerThread
    cancelled <- isCancelled cancelTok
    if cancelled
        then pure (Left "Cancelled")
        else case mResult of
            Nothing -> pure (Left "Cell execution timed out (>120s)")
            Just r -> pure (Right r)

{- | Wrap 'execScratchpad' with a circuit breaker. If the model has had
several consecutive failing scratchpad attempts in the same turn, append a
nudge to the tool response telling it to change approach instead of
silently letting the loop churn through a rate-limit window.

The breaker is per-turn and stored on the 'AIStore' via the current turn's
'turnScratchpadFails' counter. It doesn't actually block the tool — it just
annotates the response, which is enough for the model to notice.
-}
execScratchpadGuarded :: App -> AIStore -> Value -> IO (Value, Bool)
execScratchpadGuarded app store input = do
    (result, isErr) <- execScratchpad app store input
    mTurn <- getCurrentTurn store
    case mTurn of
        Nothing -> pure (result, isErr)
        Just turn -> do
            fails <-
                if isErr
                    then
                        atomicModifyIORef'
                            (turnScratchpadFails turn)
                            (\n -> (n + 1, n + 1))
                    else do
                        _ <- atomicModifyIORef' (turnScratchpadFails turn) (const (0, ()))
                        pure 0
            if fails >= 3
                then pure (annotateChurn fails result, isErr)
                else pure (result, isErr)
  where
    annotateChurn n (Object o) =
        Object $
            KM.insert
                (Key.fromText "_sabelaHint")
                ( String $
                    "You have had "
                        <> T.pack (show n)
                        <> " consecutive failing scratchpad calls this turn. Common causes:"
                        <> " (a) top-level `let` (forbidden by scripths — write `x = 1` without `let`);"
                        <> " (b) ambiguous type defaults (pin with `:: Int` or `:: Double`);"
                        <> " (c) missing import. Before retrying, either ghci_query :type"
                        <> " the function to confirm its signature, or step back and explain to"
                        <> " the user what you're blocked on."
                )
                o
    annotateChurn n other =
        object
            [ "scratchpadResult" .= other
            , "_sabelaHint"
                .= ( "Churning: "
                        <> T.pack (show n)
                        <> " consecutive scratchpad errors. Change approach or ask the user." ::
                        Text
                   )
            ]

execScratchpad :: App -> AIStore -> Value -> IO (Value, Bool)
execScratchpad app store input = do
    let rawCode = fieldText "code" input
        lang = case fieldText "language" input of
            "Python" -> Python
            _ -> Haskell
    if T.null rawCode
        then pure (object ["error" .= ("code required" :: Text)], True)
        else do
            -- For Haskell, run the snippet through the same scripths
            -- preprocessing that notebook cells get — this strips top-level
            -- `let`, auto-wraps multi-line defs in `:{ :}`, rewrites
            -- top-level TH splices, etc. Keeps scratchpad semantics parity
            -- with cell semantics so the model doesn't learn two rule-sets.
            let code = case lang of
                    Haskell -> renderHaskellForGhci rawCode
                    Python -> rawCode
            res <-
                try
                    ( do
                        backend <- ensureScratchpad app store lang
                        sbRunBlock backend code
                    ) ::
                    IO (Either SomeException (Text, Text))
            case res of
                Left e -> do
                    -- Drop the cached scratchpad so the next call rebuilds a
                    -- fresh GHCi instead of repeatedly hitting a dead
                    -- backend.
                    evictScratchpad store
                    pure (object ["error" .= T.pack (show e)], True)
                Right (stdout, stderr) -> do
                    let augErr = augmentGhciError stderr
                    stdoutV <- compactMaybeText store (Just stdout)
                    stderrV <- compactMaybeText store (Just augErr)
                    pure
                        ( object
                            [ "stdout" .= stdoutV
                            , "stderr" .= stderrV
                            ]
                        , not (T.null stderr)
                        )

{- | Tear down the cached scratchpad backend (if any) and clear the slot.
Used after a failure so the next scratchpad call starts a fresh session.
-}
evictScratchpad :: AIStore -> IO ()
evictScratchpad store = do
    mSp <- getScratchpad store
    case mSp of
        Just sp -> do
            _ <-
                try (sbClose (spBackend sp)) ::
                    IO (Either SomeException ())
            setScratchpad store Nothing
        Nothing -> pure ()

{- | Apply scripths's GHCi rendering to an ad-hoc Haskell snippet so scratchpad
is parity with notebook cells.
-}
renderHaskellForGhci :: Text -> Text
renderHaskellForGhci src =
    let sf = Scripths.scriptLines (Scripths.parseScript src)
     in Scripths.toGhciScript sf

{- | Tack a short hint onto GHCi stderr when it contains the classic top-level
`let` parse error, so models don't hallucinate that "session state is
corrupted". Zero-cost on benign stderr.
-}
augmentGhciError :: Text -> Text
augmentGhciError err
    | T.null err = err
    | mentionsLetParseError err =
        err
            <> "\n[sabela hint] GHCi rejected a top-level `let` binding. Scripths strips the"
            <> " `let` automatically in notebook cells; in scratchpad, write `x = 1`"
            <> " directly (no `let`). `let ... in ...` expressions and `let` inside do/where"
            <> " blocks remain fine."
    | otherwise = err
  where
    mentionsLetParseError t =
        ("parse error" `T.isInfixOf` t)
            && ("let" `T.isInfixOf` t || "=" `T.isInfixOf` t && "on input" `T.isInfixOf` t)

------------------------------------------------------------------------
-- ghci_query and explore_result
------------------------------------------------------------------------

execGhciQuery :: App -> Value -> IO (Value, Bool)
execGhciQuery app input = do
    let op = fieldText "op" input
        arg = fieldText "arg" input
    if T.null arg
        then pure (object ["error" .= ("arg required" :: Text)], True)
        else do
            mBackend <- getHaskellSession (appSessions app)
            case mBackend of
                Nothing ->
                    pure
                        ( object
                            [ "error" .= ("No live Haskell session — run a cell first to start GHCi." :: Text)
                            ]
                        , True
                        )
                Just backend -> do
                    result <- case op of
                        "type" -> sbQueryType backend arg
                        "info" -> sbQueryInfo backend arg
                        "kind" -> sbQueryKind backend arg
                        "browse" -> sbQueryBrowse backend arg
                        "doc" -> sbQueryDoc backend arg
                        other ->
                            pure ("Unknown op: " <> other <> " (use type|info|kind|browse|doc)")
                    pure (object ["op" .= op, "arg" .= arg, "result" .= result], False)

execApiReference :: Value -> IO (Value, Bool)
execApiReference input = do
    let mName = case field "module" input of
            Just (String s) -> s
            _ -> ""
        body = sliceApiReference mName
    pure (object ["module" .= mName, "reference" .= body], False)

execExploreResult :: AIStore -> Value -> IO (Value, Bool)
execExploreResult store input = do
    let hidText = fieldText "handle_id" input
        op = fieldText "op" input
    if T.null hidText
        then pure (object ["error" .= ("handle_id required" :: Text)], True)
        else do
            mLr <- lookupHandle (aiHandles store) (HandleId hidText)
            case mLr of
                Nothing ->
                    pure
                        ( object ["error" .= ("Handle not found (may have expired): " <> hidText)]
                        , True
                        )
                Just lr -> pure (runExplore op input lr, False)

runExplore :: Text -> Value -> LargeResult -> Value
runExplore op input lr = case op of
    "head" ->
        let n = fromMaybe 20 (fieldInt "n" input)
         in object ["lines" .= headLines n lr, "totalLines" .= lrTotalLines lr]
    "tail" ->
        let n = fromMaybe 20 (fieldInt "n" input)
         in object ["lines" .= tailLines n lr, "totalLines" .= lrTotalLines lr]
    "slice" ->
        let from = fromMaybe 1 (fieldInt "from" input)
            to = fromMaybe (from + 20) (fieldInt "to" input)
         in object
                [ "lines" .= sliceLines from to lr
                , "from" .= from
                , "to" .= to
                , "totalLines" .= lrTotalLines lr
                ]
    "grep" ->
        let pat = fieldText "pattern" input
            hits = [object ["line" .= i, "text" .= t] | (i, t) <- grepLines pat lr]
         in object ["hits" .= hits, "totalLines" .= lrTotalLines lr]
    other ->
        object ["error" .= ("Unknown op: " <> other <> " (use head|tail|slice|grep)")]

ensureScratchpad :: App -> AIStore -> CellLang -> IO SessionBackend
ensureScratchpad app store lang = do
    mSp <- getScratchpad store
    case mSp of
        Just sp | spLang sp == lang -> pure (spBackend sp)
        mOld -> do
            -- Kill existing if language changed
            case mOld of
                Just sp -> sbClose (spBackend sp)
                Nothing -> pure ()
            spDir <-
                createTempDirectory (envTmpDir (appEnv app)) "sabela-scratch"
            backend <- case lang of
                Haskell -> do
                    let projDir = envTmpDir (appEnv app) ++ "/repl-project"
                    sess <- newSession (SessionConfig projDir spDir)
                    -- If GHCi/cabal dies during prelude injection, surface
                    -- the captured cabal/ghci stderr instead of the opaque
                    -- "GHCi process exited with ExitFailure 1" so callers can
                    -- see the real cause.
                    initRes <-
                        try (runBlock sess displayPrelude) ::
                            IO (Either SomeException (Text, Text))
                    case initRes of
                        Left e -> do
                            errBuf <- readErrorBuffer sess
                            _ <-
                                try (closeSession sess) ::
                                    IO (Either SomeException ())
                            ioError $
                                userError $
                                    "Scratchpad startup failed: "
                                        ++ show e
                                        ++ ( if T.null errBuf
                                                then " (no stderr captured)"
                                                else
                                                    "\n--- captured stderr ---\n"
                                                        ++ T.unpack errBuf
                                           )
                        Right _ -> pure (ghciBackend sess)
                Python -> do
                    let venvDir = envTmpDir (appEnv app) ++ "/python-venv"
                    sess <- newPythonSession (Just venvDir) spDir
                    pure (pythonBackend sess)
            let sp = ScratchpadSession backend spDir lang
            setScratchpad store (Just sp)
            pure backend

------------------------------------------------------------------------
-- Edit lifecycle (called by server endpoints)
------------------------------------------------------------------------

{- | Accept a pending AI edit: write the new source, run the cell reactively,
and wait for its execution result before returning. The returned 'Cell' is
re-read from the notebook store AFTER execution, so its @cellOutputs@ /
@cellError@ fields reflect the fresh result — no client-side polling needed.

On timeout (cell takes >130s) we return the cell with whatever state the
notebook store currently has; SSE events will catch the eventual result.
-}
acceptEdit :: App -> AIStore -> ReactiveNotebook -> EditId -> IO (Maybe Cell)
acceptEdit app store rn eid = do
    mEdit <- lookupEdit store eid
    case mEdit of
        Nothing -> pure Nothing
        Just edit -> do
            modifyNotebook (appNotebook app) $ \nb ->
                nb
                    { nbCells =
                        map
                            ( \c ->
                                if cellId c == aeCellId edit
                                    then c{cellSource = aeNewSource edit, cellDirty = True}
                                    else c
                            )
                            (nbCells nb)
                    }
            atomically $ writeTVar (aeStatus edit) Accepted
            broadcastNotebook app
            -- Block on execution so the HTTP response / tool_result carries
            -- the actual outputs instead of forcing the caller to poll.
            ct <- newCancelToken
            _ <- executeCell app rn (aeCellId edit) ct
            nb <- readNotebook (appNotebook app)
            pure (lookupCell (aeCellId edit) nb)

revertEdit :: App -> AIStore -> EditId -> IO ()
revertEdit app store eid = do
    updateEditStatus store eid Reverted
    broadcastNotebook app
