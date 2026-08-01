{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.Replace (
    execReplaceCellSource,
    execSupersedeCell,
    applyReplaceCellSource,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.Ack (withNote)
import Sabela.AI.Capabilities.Edit.Admit (conflictJson)
import Sabela.AI.Capabilities.Edit.GateRepair (gatedCandidate)
import Sabela.AI.Capabilities.Edit.Run (autoExecuteAfterMutation)
import Sabela.AI.Capabilities.Edit.Submission (
    Submission,
    compiledText,
    replaceSubmission,
    reroutedSubmission,
    submissionNotes,
 )
import Sabela.AI.Capabilities.Util (field, fieldInt, fieldText)
import Sabela.AI.Doc (cellHash)
import Sabela.AI.Store (AIStore)
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.Anthropic.Types (CancelToken)
import Sabela.Api (errorJson, errorJsonWith)
import Sabela.Handlers (ReactiveNotebook, setCellSourceChecked)
import Sabela.Model
import Sabela.Parse (staleBindings, validateCellShape)
import Sabela.SessionTypes (CellLang (..))
import Sabela.State

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

{- | An insert the router turned into an overwrite of @cid@. The caller's own
bytes remain the submission, so a rejection still shows it its own text.
-}
execSupersedeCell ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    CancelToken ->
    Int ->
    Submission ->
    IO ToolOutcome
execSupersedeCell app store rn cancelTok cid sub = do
    nb <- readNotebook (appNotebook app)
    case lookupCell cid nb of
        Nothing ->
            pure (errOutcome (errorJson ("Cell not found: " <> T.pack (show cid))))
        Just c ->
            applyReplaceSubmission
                app
                store
                rn
                cancelTok
                c
                (reroutedSubmission (cellLang c) (cellSource c) sub)

applyReplaceCellSource ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    CancelToken ->
    Cell ->
    Text ->
    IO ToolOutcome
applyReplaceCellSource app store rn cancelTok oldCell newSrc0 =
    applyReplaceSubmission
        app
        store
        rn
        cancelTok
        oldCell
        (replaceSubmission (cellLang oldCell) (cellSource oldCell) newSrc0)

applyReplaceSubmission ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    CancelToken ->
    Cell ->
    Submission ->
    IO ToolOutcome
applyReplaceSubmission app store rn cancelTok oldCell sub =
    case structuralReject oldCell (compiledText sub) of
        Just msg -> pure (errOutcome (errorJson msg))
        Nothing -> do
            out <- doReplace app store rn cancelTok oldCell sub
            pure $ case submissionNotes sub of
                [] -> out
                ns -> withNote (T.unwords ns) out

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
    Submission ->
    IO ToolOutcome
doReplace app store rn cancelTok oldCell sub = do
    let cid = cellId oldCell
    gate <-
        gatedCandidate app (Just cid) (cellLang oldCell) (cellType oldCell) sub
    case gate of
        Left rejection -> pure (errOutcome rejection)
        Right (newSrc', repairNotes) ->
            commitReplace app store rn cancelTok oldCell newSrc' repairNotes

commitReplace ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    CancelToken ->
    Cell ->
    Text ->
    [Text] ->
    IO ToolOutcome
commitReplace app store rn cancelTok oldCell newSrc repairNotes = do
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
                        ( [ "cellId" .= cid
                          , "hash" .= cellHash newCell
                          , "execution" .= execSummary
                          , "staleBindings" .= stale
                          ]
                            <> ["repairs" .= repairNotes | not (null repairNotes)]
                        )
