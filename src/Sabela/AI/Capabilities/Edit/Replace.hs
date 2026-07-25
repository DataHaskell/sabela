{-# LANGUAGE OverloadedStrings #-}

{- | The @replace_cell_source@ tool: gated rewrite, structural pre-check, G1's
compile gate, and the checked commit. Split from "Sabela.AI.Capabilities.Edit"
for the module-size cap; 'execInsertCell' there still reaches
'execReplaceCellSource' for its red-cell-supersession redirect.
-}
module Sabela.AI.Capabilities.Edit.Replace (
    execReplaceCellSource,
    applyReplaceCellSource,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.Admit (conflictJson, restickCabal)
import Sabela.AI.Capabilities.Edit.Ack (withNote)
import Sabela.AI.Capabilities.Edit.CompileGate (compileGateCheck)
import Sabela.AI.Capabilities.Edit.Run (autoExecuteAfterMutation)
import Sabela.AI.Capabilities.Util (field, fieldInt, fieldText)
import Sabela.AI.Doc (cellHash)
import Sabela.AI.NormalizeGate (gatedRewrite)
import Sabela.AI.Store (AIStore)
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.Anthropic.Types (CancelToken)
import Sabela.Api (errorJson, errorJsonWith)
import Sabela.Handlers (ReactiveNotebook, setCellSourceChecked)
import Sabela.Model
import Sabela.Parse (staleBindings, validateCellShape)
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
applyReplaceCellSource app store rn cancelTok oldCell newSrc0 =
    case structuralReject oldCell newSrc of
        Just msg -> pure (errOutcome (errorJson msg))
        Nothing -> do
            out <- doReplace app store rn cancelTok oldCell newSrc
            pure $ case disclosures of
                [] -> out
                ns -> withNote (T.unwords ns) out
  where
    -- Restick before gating: 'restickCabal' only fires when the submission
    -- carries zero cabal lines, a fact none of 'gatedRewrite'\'s generators
    -- can change, so the two compose in either order — restick first lets
    -- 'gatedRewrite' (G7, the one normalizer every path shares) see, and
    -- echo, the truly final source.
    sticked =
        if cellLang oldCell == Haskell
            then restickCabal (cellSource oldCell) newSrc0
            else newSrc0
    (newSrc, gateNotes) =
        if cellLang oldCell == Haskell
            then gatedRewrite sticked
            else (sticked, [])
    -- R7.1: every kept machine rewrite is disclosed, carrying the source.
    disclosures =
        [ "Kept the -- cabal: line your replace omitted (its imports still need it)."
        | sticked /= newSrc0
        ]
            <> gateNotes

{- | The pre-GHC structural rejection for a Haskell cell, if any. Non-Haskell
cells are not shape-checked (the validator uses the Haskell parser).
-}
structuralReject :: Cell -> Text -> Maybe Text
structuralReject c newSrc
    | cellLang c == Haskell = validateCellShape (cellType c) newSrc
    | otherwise = Nothing

{- | G1's compile gate, then the checked commit: a candidate that does not
compile against the current notebook prefix is rejected outright — no cell,
no generation bump, no session mutation — carrying the full diagnostic.
-}
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
    gate <- compileGateCheck app (Just cid) (cellLang oldCell) (cellType oldCell) newSrc
    case gate of
        Left rejection -> pure (errOutcome rejection)
        Right () -> commitReplace app store rn cancelTok oldCell newSrc

commitReplace ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    CancelToken ->
    Cell ->
    Text ->
    IO ToolOutcome
commitReplace app store rn cancelTok oldCell newSrc = do
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
