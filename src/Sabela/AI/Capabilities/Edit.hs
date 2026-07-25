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

    -- and "Sabela.AI.Capabilities.Edit.Propose"
    autoExecuteAfterMutation,
    execExecuteCell,
    executeCell,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.Ack (ackWriteAndRun, withNote)
import Sabela.AI.Capabilities.Edit.Admit (
    conflictJson,
    pendingErrorFor,
    sigBodyProposalFor,
    supersedeNote,
    supersedesRedCell,
    violationJson,
 )
import Sabela.AI.Capabilities.Edit.CompileGate (compileGateCheck)
import Sabela.AI.Capabilities.Edit.Propose (
    execProposeEdit,
    proceedProposeEdit,
 )
import Sabela.AI.Capabilities.Edit.Replace (
    applyReplaceCellSource,
    execReplaceCellSource,
 )
import Sabela.AI.Capabilities.Edit.Run (
    autoExecuteAfterMutation,
    execExecuteCell,
    executeCell,
 )
import Sabela.AI.Capabilities.Edit.ValueGate (prewriteValueVeto)
import Sabela.AI.Capabilities.Util (
    fieldInt,
    fieldText,
    parseCellLang,
    parseCellType,
 )
import Sabela.AI.NormalizeGate (gatedNormalizeInsert)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Anthropic.Types (CancelToken)
import Sabela.Api (errorJson)
import Sabela.Handlers (
    NotebookViolation (..),
    ReactiveNotebook (..),
    checkedAppend,
    pendingError,
 )
import Sabela.Model
import Sabela.Parse (validateCellShape)
import Sabela.SessionTypes (CellLang (..))
import Sabela.State

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
            let (cellTp, src', notes) = gatedNormalizeInsert rawTp src
                lang = if cellTp /= rawTp then Haskell else rawLg
            mVeto <- prewriteValueVeto app lang cellTp src'
            case (mVeto, validateCellShape cellTp src') of
                (Just veto, _) -> pure veto
                (_, Just msg) | lang == Haskell -> pure (errOutcome (errorJson msg))
                -- A bodiless top-level signature dams later inserts (topMonth):
                -- answer as a `name = _` proposal, never commit the red cell.
                _
                    | Just prop <- sigBodyProposalFor lang cellTp src' ->
                        pure (errOutcome prop)
                _ -> do
                    nid <- freshCellId (appNotebook app)
                    let cell = Cell nid cellTp lang src' [] Nothing True
                    -- A notebook already dammed by a pending error skips the
                    -- gate, straight to the existing redirect-to-replace UX
                    -- below, which re-detects it atomically and race-safely.
                    peek <- readNotebook (appNotebook app)
                    case pendingError peek of
                        Just _ ->
                            commitInsert app store rn cancelTok input cell src' notes
                        Nothing -> do
                            gate <- compileGateCheck app Nothing lang cellTp src'
                            case gate of
                                Left rejection -> pure (errOutcome rejection)
                                Right () ->
                                    commitInsert
                                        app
                                        store
                                        rn
                                        cancelTok
                                        input
                                        cell
                                        src'
                                        notes

{- | Commit a gate-passed (or pending-error-dammed) insert candidate: the
existing checked-append + pending-error-redirect + auto-run behaviour,
unchanged from before G1's compile gate was added in front of it.
-}
commitInsert ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    CancelToken ->
    Value ->
    Cell ->
    Text ->
    [Text] ->
    IO ToolOutcome
commitInsert app store rn cancelTok input cell src' notes = do
    res <- atomicEditNotebook (appNotebook app) $ \nb ->
        case checkedAppend cell nb of
            Left v -> (nb, Left v)
            Right nb' -> (nb', Right ())
    case res of
        -- An insert rewriting the red cell's definitions is a
        -- fix attempt: apply it as a replace of that cell (the
        -- cascade runs on it) instead of re-dumping the error.
        Left (VPendingError cid msg) -> do
            mRed <- lookupCell cid <$> readNotebook (appNotebook app)
            case mRed of
                Just red
                    | supersedesRedCell src' (cellSource red) -> do
                        out <-
                            execReplaceCellSource
                                app
                                store
                                rn
                                cancelTok
                                ( object
                                    [ "cell_id" .= cid
                                    , "new_source" .= src'
                                    ]
                                )
                        pure (withNote (supersedeNote cid) out)
                Just red ->
                    pure
                        (errOutcome (pendingErrorFor cid (cellSource red)))
                Nothing ->
                    pure
                        (errOutcome (violationJson (VPendingError cid msg)))
        Left v -> pure (errOutcome (violationJson v))
        Right () -> do
            broadcastNotebook app
            let runnable =
                    cellType cell == CodeCell
                        && cellLang cell == Haskell
                        && not (T.null (T.strip src'))
            ackWriteAndRun
                app
                store
                rn
                cancelTok
                input
                cell
                runnable
                notes

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
