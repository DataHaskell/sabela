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
    conflictJson,

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
import Sabela.Api (errorJson, errorJsonWith)
import Sabela.Handlers (
    DefConflict (..),
    NotebookViolation (..),
    ReactiveNotebook (..),
    checkedAppend,
    setCellSourceChecked,
 )
import Sabela.Model
import Sabela.Parse (staleBindings, validateCellShape)
import Sabela.Parse.Normalize (normalizeInsert, rewriteTopLevelLet, unwrapMain)
import Sabela.SessionTypes (CellLang (..))
import Sabela.State

{- | Turn a rejected mutation into actionable guidance: name the binding, point
at the cell that already owns it, and tell the model to use it or edit the owner
instead of redefining it in a new cell.
-}
conflictJson :: DefConflict -> Value
conflictJson (DefConflict name owner) =
    errorJsonWith
        ( "`"
            <> name
            <> "` is already defined in cell "
            <> tShow owner
            <> " and is in scope. Use it directly, or call replace_cell_source"
            <> " on cell "
            <> tShow owner
            <> " to change that definition. Do not redefine it in a new cell."
        )
        ["binding" .= name, "ownerCell" .= owner]
  where
    tShow = T.pack . show

{- | Turn a rejected insert into actionable guidance. A duplicate reuses
'conflictJson'; a pending error names the failing cell and the two remedies (fix
it in place, or remove it) — the contract carries the fix, not the prompt.
-}
violationJson :: NotebookViolation -> Value
violationJson (VDuplicateDef dc) = conflictJson dc
violationJson (VPendingError cid msg) =
    errorJsonWith
        ( "Cell "
            <> tShow cid
            <> " has an unresolved error, so a new cell cannot be added. Fix it with"
            <> " replace_cell_source, or remove it with delete_cell, then insert again."
            <> " Its error: "
            <> T.takeWhile (/= '\n') msg
        )
        ["pendingErrorCell" .= cid]
  where
    tShow = T.pack . show

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
applyReplaceCellSource app store rn cancelTok oldCell newSrc0 =
    case structuralReject oldCell newSrc of
        Just msg -> pure (errOutcome (errorJson msg))
        Nothing -> doReplace app store rn cancelTok oldCell newSrc
  where
    -- Keep replace consistent with insert: unwrap a top-level `main` to bare
    -- top-level code so an edited-in `main` runs instead of silently not.
    newSrc =
        if cellLang oldCell == Haskell
            then unwrapMain (rewriteTopLevelLet newSrc0)
            else newSrc0

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
    res <- atomicEditNotebook (appNotebook app) $ \nb ->
        case setCellSourceChecked oldCell newSrc nb of
            Left conflict -> (nb, Left conflict)
            Right (nb', newCell) -> (nb', Right newCell)
    case res of
        Left conflict -> pure (errOutcome (conflictJson conflict))
        Right newCell -> do
            broadcastNotebook app
            let stale =
                    if cellLang newCell == Haskell
                        then staleBindings (cellSource oldCell) newSrc
                        else []
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
                        , "staleBindings" .= stale
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
    let src = fieldText "source" input
        rawType = fieldText "cell_type" input
        rawLang = fieldText "language" input
        mLang = if T.null rawLang then Just Haskell else parseCellLang rawLang
        mType = if T.null rawType then Just CodeCell else parseCellType rawType
    case (mType, mLang) of
        (Nothing, _) ->
            pure
                ( errOutcome
                    ( errorJson
                        ("Unknown cell_type: " <> rawType <> ". Expected CodeCell or ProseCell.")
                    )
                )
        (_, Nothing) ->
            pure
                ( errOutcome
                    ( errorJson
                        ("Unknown language: " <> rawLang <> ". Expected Haskell or Python.")
                    )
                )
        (Just rawTp, Just rawLg) -> do
            let (cellTp, src', notes) = normalizeInsert rawTp src
                lang = if cellTp /= rawTp then Haskell else rawLg
            case validateCellShape cellTp src' of
                Just msg | lang == Haskell -> pure (errOutcome (errorJson msg))
                _ -> do
                    nid <- freshCellId (appNotebook app)
                    let cell = Cell nid cellTp lang src' [] Nothing True
                    res <- atomicEditNotebook (appNotebook app) $ \nb ->
                        case checkedAppend cell nb of
                            Left v -> (nb, Left v)
                            Right nb' -> (nb', Right ())
                    case res of
                        Left v -> pure (errOutcome (violationJson v))
                        Right () -> do
                            broadcastNotebook app
                            execSummary <-
                                if cellTp == CodeCell && lang == Haskell && not (T.null (T.strip src'))
                                    then autoExecuteAfterMutation app store rn cancelTok nid
                                    else pure Null
                            pure $
                                okOutcome $
                                    object $
                                        [ "cellId" .= nid
                                        , "hash" .= cellHash cell
                                        , "execution" .= execSummary
                                        ]
                                            <> [ "note" .= T.unwords notes | not (null notes)
                                               ]

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
