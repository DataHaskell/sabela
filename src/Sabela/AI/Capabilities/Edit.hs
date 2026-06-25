{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Mutating notebook tools (replace, propose-edit, insert, delete) plus
the synchronous 'executeCell' / 'autoExecuteAfterMutation' helpers
mutation tools call so their response carries fresh execution results.
-}
module Sabela.AI.Capabilities.Edit (
    execReplaceCellSource,
    execProposeEdit,
    execInsertCell,
    execDeleteCell,
    applyReplaceCellSource,
    proceedProposeEdit,

    -- * Re-exports from the split "Sabela.AI.Capabilities.Edit.Run"
    autoExecuteAfterMutation,
    execExecuteCell,
    executeCell,
) where

import Control.Concurrent.STM (newTVarIO)
import Data.Aeson (Value (..), object, (.=))
import Data.IORef (atomicModifyIORef')
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.Run (
    autoExecuteAfterMutation,
    execExecuteCell,
    executeCell,
 )
import Sabela.AI.Capabilities.Util (
    field,
    fieldInt,
    fieldText,
    parseCellLang,
    parseCellType,
 )
import Sabela.AI.Doc (cellHash)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Anthropic.Types (CancelToken)
import Sabela.Api (InsertAt (..), errorJson, errorJsonWith)
import Sabela.Handlers (ReactiveNotebook (..), insertCellAt)
import Sabela.Model
import Sabela.Parse (validateCellShape)
import Sabela.SessionTypes (CellLang (..))
import Sabela.State

{- | Replace a cell's source, broadcast, and (for Haskell code cells)
auto-run via 'autoExecuteAfterMutation' so the response carries the
fresh execution summary. AI-internal iteration loop.
-}
execReplaceCellSource ::
    App -> AIStore -> ReactiveNotebook -> CancelToken -> Value -> IO ToolOutcome
execReplaceCellSource app store rn cancelTok input = do
    let mcid = fieldInt "cell_id" input
        newSrc = fieldText "new_source" input
        mExpected = case field "expected_hash" input of
            Just (String s) | not (T.null s) -> Just s
            _ -> Nothing
    case mcid of
        Nothing ->
            pure (errOutcome (errorJson "cell_id required"))
        Just cid -> do
            nb <- readNotebook (appNotebook app)
            case lookupCell cid nb of
                Nothing ->
                    pure (errOutcome (errorJson ("Cell not found: " <> T.pack (show cid))))
                Just c -> case mExpected of
                    Just expected
                        | cellHash c /= expected ->
                            pure
                                ( errOutcome
                                    ( errorJsonWith
                                        "Hash mismatch — re-read the cell and retry."
                                        [ "cellId" .= cid
                                        , "currentHash" .= cellHash c
                                        , "expectedHash" .= expected
                                        ]
                                    )
                                )
                    _ -> applyReplaceCellSource app store rn cancelTok c newSrc

applyReplaceCellSource ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    CancelToken ->
    Cell ->
    Text ->
    IO ToolOutcome
applyReplaceCellSource app store rn cancelTok oldCell newSrc =
    case structuralReject oldCell newSrc of
        Just msg -> pure (errOutcome (errorJson msg))
        Nothing -> doReplace app store rn cancelTok oldCell newSrc

{- | The pre-GHC structural rejection for a Haskell cell, if any. Non-Haskell
cells are not shape-checked (the validator uses the Haskell parser).
-}
structuralReject :: Cell -> Text -> Maybe Text
structuralReject c newSrc
    | cellLang c == Haskell = validateCellShape (cellType c) newSrc
    | otherwise = Nothing

doReplace ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    CancelToken ->
    Cell ->
    Text ->
    IO ToolOutcome
doReplace app store rn cancelTok oldCell newSrc = do
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
    pure $
        okOutcome $
            object
                [ "cellId" .= cid
                , "hash" .= cellHash newCell
                , "execution" .= execSummary
                ]

execProposeEdit :: App -> AIStore -> Value -> IO ToolOutcome
execProposeEdit app store input = do
    let mcid = fieldInt "cell_id" input
        newSrc = fieldText "new_source" input
        mExpected = case field "expected_hash" input of
            Just (String s) | not (T.null s) -> Just s
            _ -> Nothing
    case mcid of
        Nothing -> pure (errOutcome (errorJson "cell_id required"))
        Just cid -> do
            nb <- readNotebook (appNotebook app)
            case lookupCell cid nb of
                Nothing ->
                    pure (errOutcome (errorJson ("Cell not found: " <> T.pack (show cid))))
                Just c -> case mExpected of
                    Just expected
                        | cellHash c /= expected ->
                            pure
                                ( errOutcome
                                    ( errorJsonWith
                                        "Hash mismatch — cell has changed since you last read it. Re-read and retry."
                                        [ "cellId" .= cid
                                        , "currentHash" .= cellHash c
                                        , "expectedHash" .= expected
                                        ]
                                    )
                                )
                    _ -> proceedProposeEdit app store c newSrc

proceedProposeEdit :: App -> AIStore -> Cell -> Text -> IO ToolOutcome
proceedProposeEdit app store c newSrc = do
    let cid = cellId c
    eid <- atomicModifyIORef' (aiNextEditId store) (\n -> (n + 1, n))
    let editId = EditId eid
    statusVar <- newTVarIO Pending
    mTurn <- getCurrentTurn store
    let mTid = fmap turnId mTurn
        edit =
            AiEdit
                { aeEditId = editId
                , aeCellId = cid
                , aeOldSource = cellSource c
                , aeNewSource = newSrc
                , aeStatus = statusVar
                , aeTurnId = mTid
                }
    addPendingEdit store edit
    broadcast (appEvents app) $
        EvChatEditProposed mTid editId cid (cellSource c) newSrc
    pure $
        okOutcome $
            object
                [ "editId" .= eid
                , "cellId" .= cid
                , "status" .= ("pending" :: Text)
                ]

execInsertCell ::
    App -> AIStore -> ReactiveNotebook -> CancelToken -> Value -> IO ToolOutcome
execInsertCell app store rn cancelTok input = do
    let mAfterId = fieldInt "after_cell_id" input
        src = fieldText "source" input
        rawType = fieldText "cell_type" input
        rawLang = fieldText "language" input
        mAt = parseInsertAt mAfterId
    case (mAt, parseCellType rawType, parseCellLang rawLang) of
        (Nothing, _, _) ->
            pure
                ( errOutcome
                    ( errorJson
                        "after_cell_id must be -1 (at beginning) or a non-negative cell id"
                    )
                )
        (_, Nothing, _) ->
            pure
                ( errOutcome
                    ( errorJson
                        ( "Unknown cell_type: "
                            <> rawType
                            <> ". Expected CodeCell or ProseCell."
                        )
                    )
                )
        (_, _, Nothing) ->
            pure
                ( errOutcome
                    ( errorJson
                        ( "Unknown language: "
                            <> rawLang
                            <> ". Expected Haskell or Python."
                        )
                    )
                )
        (Just _, Just cellTp, Just Haskell)
            | Just msg <- validateCellShape cellTp src ->
                pure (errOutcome (errorJson msg))
        (Just at, Just cellTp, Just lang) -> do
            nid <- freshCellId (appNotebook app)
            let cell = Cell nid cellTp lang src [] Nothing True
            modifyNotebook (appNotebook app) $ \nb ->
                nb{nbCells = insertCellAt at cell (nbCells nb)}
            broadcastNotebook app
            execSummary <-
                if cellTp == CodeCell && lang == Haskell && not (T.null (T.strip src))
                    then autoExecuteAfterMutation app store rn cancelTok nid
                    else pure Null
            pure $
                okOutcome $
                    object
                        [ "cellId" .= nid
                        , "hash" .= cellHash cell
                        , "execution" .= execSummary
                        ]

{- | Parse the tool's @after_cell_id@ field into the 'InsertAt' ADT.
@Nothing@ (field absent) defaults to 'AtBeginning' to match the previous
@fromMaybe (-1)@ behaviour; @Just (-1)@ is 'AtBeginning'; non-negative
values are 'After'; any other negative number returns 'Nothing' (a
tool-call bug surfaced to the model rather than silent appending).
-}
parseInsertAt :: Maybe Int -> Maybe InsertAt
parseInsertAt Nothing = Just AtBeginning
parseInsertAt (Just (-1)) = Just AtBeginning
parseInsertAt (Just n) | n >= 0 = Just (After n)
parseInsertAt _ = Nothing

execDeleteCell :: App -> Value -> IO ToolOutcome
execDeleteCell app input = do
    let mcid = fieldInt "cell_id" input
    case mcid of
        Nothing -> pure (errOutcome (errorJson "cell_id required"))
        Just cid -> do
            modifyNotebook (appNotebook app) $ \nb ->
                nb{nbCells = filter (\c -> cellId c /= cid) (nbCells nb)}
            broadcastNotebook app
            pure (okOutcome (object ["deleted" .= True, "cellId" .= cid]))

-- 'autoExecuteAfterMutation', 'execExecuteCell', and 'executeCell' moved
-- to "Sabela.AI.Capabilities.Edit.Run" so this module stays under the
-- 300-LOC cap. Re-exported above for back-compat.
