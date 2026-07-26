{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Repair.Blocking (repairBlockingCell) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.CellResult (CellId)
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Repair (Dispatch, compiled, repairOne)

repairBlockingCell ::
    Dispatch -> CellId -> IO (Maybe (ToolCall, Either Text ToolOutcome))
repairBlockingCell disp cid = do
    merr <- readCellError disp cid
    case merr of
        Nothing -> pure Nothing
        Just err -> do
            m <- repairOne disp cid err
            pure $ case m of
                Just fix@(_, out) | compiled out -> Just fix
                _ -> Nothing

readCellError :: Dispatch -> CellId -> IO (Maybe Text)
readCellError disp cid = do
    out <- disp (ToolCall "read_cell" (object ["cell_id" .= cid]))
    pure $ case out of
        Right (ToolOk (Object o))
            | Just (String e) <- KM.lookup (K.fromText "error") o
            , not (T.null e) ->
                Just e
        _ -> Nothing
