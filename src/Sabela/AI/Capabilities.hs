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
    encode,
    object,
    (.=),
 )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (atomicModifyIORef')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Temp (createTempDirectory)
import System.Timeout (timeout)

import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Anthropic.Types (CancelToken, ToolDef (..), isCancelled)
import Sabela.Handlers (ReactiveNotebook (..))
import Sabela.Model
import Sabela.Output (displayPrelude)
import Sabela.PythonSession (newPythonSession, pythonBackend)
import Sabela.Session (SessionConfig (..), ghciBackend, newSession, runBlock)
import Sabela.SessionTypes (CellLang (..), SessionBackend (..))
import Sabela.State

------------------------------------------------------------------------
-- Tool definitions
------------------------------------------------------------------------

chatTools :: [ToolDef]
chatTools =
    [ ToolDef
        "list_cells"
        "List all cells in the notebook with their IDs, types, languages, first line of source, and whether they have errors. Use this for a quick overview."
        (object ["type" .= ("object" :: Text), "properties" .= object []])
    , ToolDef
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
    , ToolDef
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
    , ToolDef
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
    , ToolDef
        "propose_edit"
        "Propose a source code change for a cell. The change is NOT applied immediately — the user must accept it. Returns an editId for tracking."
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
                    ]
            , "required" .= (["cell_id", "new_source"] :: [Text])
            ]
        )
    , ToolDef
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
                            , "enum" .= (["Haskell", "Lean4", "Python"] :: [Text])
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
    , ToolDef
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
    , ToolDef
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
    , ToolDef
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
executeTool app store rn cancelTok toolName input = do
    eResult <- try $ case toolName of
        "list_cells" -> execListCells app
        "read_cell" -> execReadCell app input
        "read_cell_output" -> execReadCellOutput app input
        "find_cells_by_content" -> execFindCells app input
        "propose_edit" -> execProposeEdit app store input
        "insert_cell" -> execInsertCell app input
        "delete_cell" -> execDeleteCell app input
        "execute_cell" -> execExecuteCell app rn cancelTok input
        "scratchpad" -> execScratchpad app store input
        _ -> pure (object ["error" .= ("Unknown tool: " <> toolName)], True)
    case eResult of
        Left (e :: SomeException) ->
            pure (object ["error" .= T.pack (show e)], True)
        Right r -> pure r

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
    let summaries = zipWith cellSummary [1 :: Int ..] (nbCells nb)
    pure (object ["title" .= nbTitle nb, "cells" .= summaries], False)
  where
    cellSummary pos c =
        object
            [ "id" .= cellId c
            , "position" .= pos
            , "type" .= cellType c
            , "lang" .= cellLang c
            , "firstLine" .= T.take 80 (head' (T.lines (cellSource c)))
            , "hasError" .= (Data.Maybe.isJust (cellError c))
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

execProposeEdit :: App -> AIStore -> Value -> IO (Value, Bool)
execProposeEdit app store input = do
    let mcid = fieldInt "cell_id" input
        newSrc = fieldText "new_source" input
    case mcid of
        Nothing -> pure (object ["error" .= ("cell_id required" :: Text)], True)
        Just cid -> do
            nb <- readNotebook (appNotebook app)
            case lookupCell cid nb of
                Nothing ->
                    pure (object ["error" .= ("Cell not found: " <> T.pack (show cid))], True)
                Just c -> do
                    eid <- atomicModifyIORef' (aiNextEditId store) (\n -> (n + 1, n))
                    let editId = EditId eid
                    statusVar <- newTVarIO Pending
                    -- Get current turn ID
                    mTurn <- getCurrentTurn store
                    let tid = maybe (TurnId 0) turnId mTurn
                    let edit =
                            AiEdit
                                { aeEditId = editId
                                , aeCellId = cid
                                , aeOldSource = cellSource c
                                , aeNewSource = newSrc
                                , aeStatus = statusVar
                                , aeTurnId = tid
                                }
                    addPendingEdit store edit
                    -- Broadcast the proposal via SSE
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

execInsertCell :: App -> Value -> IO (Value, Bool)
execInsertCell app input = do
    let afterId = fromMaybe (-1) (fieldInt "after_cell_id" input)
        src = fieldText "source" input
        cellTp = case fieldText "cell_type" input of
            "ProseCell" -> ProseCell
            _ -> CodeCell
        lang = case fieldText "language" input of
            "Python" -> Python
            "Lean4" -> Lean4
            _ -> Haskell
    nid <- freshCellId (appNotebook app)
    let cell = Cell nid cellTp lang src [] Nothing True
    modifyNotebook (appNotebook app) $ \nb ->
        nb{nbCells = ins afterId cell (nbCells nb)}
    pure (object ["cellId" .= nid], False)
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
            pure (object ["deleted" .= True, "cellId" .= cid], False)

execExecuteCell ::
    App -> ReactiveNotebook -> CancelToken -> Value -> IO (Value, Bool)
execExecuteCell app rn cancelTok input = do
    let mcid = fieldInt "cell_id" input
    case mcid of
        Nothing -> pure (object ["error" .= ("cell_id required" :: Text)], True)
        Just cid -> do
            result <- executeCell app rn cid cancelTok
            case result of
                Left err -> pure (object ["error" .= err], True)
                Right er -> pure (toJSON er, False)

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

execScratchpad :: App -> AIStore -> Value -> IO (Value, Bool)
execScratchpad app store input = do
    let code = fieldText "code" input
        lang = case fieldText "language" input of
            "Python" -> Python
            _ -> Haskell
    if T.null code
        then pure (object ["error" .= ("code required" :: Text)], True)
        else do
            backend <- ensureScratchpad app store lang
            (stdout, stderr) <- sbRunBlock backend code
            pure
                ( object
                    [ "stdout" .= stdout
                    , "stderr" .= stderr
                    ]
                , not (T.null stderr)
                )

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
                    _ <- runBlock sess displayPrelude
                    pure (ghciBackend sess)
                Python -> do
                    let venvDir = envTmpDir (appEnv app) ++ "/python-venv"
                    sess <- newPythonSession (Just venvDir) spDir
                    pure (pythonBackend sess)
                Lean4 ->
                    error "Scratchpad not supported for Lean4"
            let sp = ScratchpadSession backend spDir lang
            setScratchpad store (Just sp)
            pure backend

------------------------------------------------------------------------
-- Edit lifecycle (called by server endpoints)
------------------------------------------------------------------------

acceptEdit :: App -> AIStore -> EditId -> IO (Maybe Cell)
acceptEdit app store eid = do
    mEdit <- lookupEdit store eid
    case mEdit of
        Nothing -> pure Nothing
        Just edit -> do
            -- Write the new source to the notebook
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
            -- Update status
            atomically $ writeTVar (aeStatus edit) Accepted
            -- Return the updated cell
            nb <- readNotebook (appNotebook app)
            pure (lookupCell (aeCellId edit) nb)

revertEdit :: AIStore -> EditId -> IO ()
revertEdit store eid = updateEditStatus store eid Reverted
