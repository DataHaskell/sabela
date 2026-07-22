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
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.Ack (ackWriteAndRun)
import Sabela.AI.Capabilities.Edit.Admit (
    conflictJson,
    pendingErrorFor,
    restickCabal,
    sigBodyProposalFor,
    supersedeNote,
    supersedesRedCell,
    violationJson,
 )
import Sabela.AI.Capabilities.Edit.Propose (
    execProposeEdit,
    proceedProposeEdit,
 )
import Sabela.AI.Capabilities.Edit.Run (
    autoExecuteAfterMutation,
    execExecuteCell,
    executeCell,
 )
import Sabela.AI.Capabilities.Edit.ValueGate (prewriteValueVeto)
import Sabela.AI.Capabilities.Util (
    field,
    fieldInt,
    fieldText,
    parseCellLang,
    parseCellType,
 )
import Sabela.AI.Doc (cellHash)
import Sabela.AI.NormalizeGate (
    currentSourceNote,
    gatedNormalizeInsert,
    gatedRewrite,
 )
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Anthropic.Types (CancelToken)
import Sabela.Api (errorJson, errorJsonWith)
import Sabela.Handlers (
    NotebookViolation (..),
    ReactiveNotebook (..),
    checkedAppend,
    setCellSourceChecked,
 )
import Sabela.Model
import Sabela.Parse (staleBindings, validateCellShape)
import Sabela.SessionTypes (CellLang (..))
import Sabela.State

-- | Attach a note to a successful outcome; errors pass through untouched.
withNote :: T.Text -> ToolOutcome -> ToolOutcome
withNote n (ToolOk (Object o)) = ToolOk (Object (KM.insert "note" (String n) o))
withNote _ out = out

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
        Nothing -> do
            out <- doReplace app store rn cancelTok oldCell newSrc
            pure $ case disclosures of
                [] -> out
                ns -> withNote (T.unwords ns) out
  where
    -- Keep replace consistent with insert: the same gated rewrite (one
    -- acceptance law, section 9.3) — kept iff it parses no worse.
    base =
        if cellLang oldCell == Haskell
            then gatedRewrite newSrc0
            else newSrc0
    newSrc =
        if cellLang oldCell == Haskell
            then restickCabal (cellSource oldCell) base
            else base
    -- R7.1: every kept machine rewrite is disclosed, carrying the source.
    disclosures =
        [ "Kept the -- cabal: line your replace omitted (its imports still need it)."
        | newSrc /= base
        ]
            <> [currentSourceNote newSrc | base /= newSrc0]

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
                                    cellTp == CodeCell
                                        && lang == Haskell
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
