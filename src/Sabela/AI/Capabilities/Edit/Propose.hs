{-# LANGUAGE OverloadedStrings #-}

{- | The propose-edit tool: stage a pending cell edit for the user to accept
or revert, broadcast on the chat channel. Split from
"Sabela.AI.Capabilities.Edit" for the module-size cap.
-}
module Sabela.AI.Capabilities.Edit.Propose (
    execProposeEdit,
    proceedProposeEdit,
) where

import Control.Concurrent.STM (newTVarIO)
import Data.Aeson (Value (..), object, (.=))
import Data.IORef (atomicModifyIORef')
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Util (field, fieldInt, fieldText)
import Sabela.AI.Doc (cellHash)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Api (errorJson, errorJsonWith)
import Sabela.Model
import Sabela.State

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
