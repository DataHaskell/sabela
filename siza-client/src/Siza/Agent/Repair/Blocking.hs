{-# LANGUAGE OverloadedStrings #-}

{- | The pending-error-block repair seam (search-api.md 5.4/9.5): heal the cell
a blocked insert names, using the fits GHC already reported, before the red is
handed back to the model to thrash on — the same class-keyed cascade the
red-cell path runs, dispatched by diagnostic class, never a library.
-}
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

{- | Heal the cell a pending-error block names before the red is handed back to
the model: read its diagnostic, run the class-keyed cascade, keep the write ONLY
when a candidate recompiled the cell. 'Nothing' (cell clean or nothing survived)
leaves the caller its routed-unblock.
-}
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

-- | The blocking cell's diagnostic text, or 'Nothing' when it reads clean.
readCellError :: Dispatch -> CellId -> IO (Maybe Text)
readCellError disp cid = do
    out <- disp (ToolCall "read_cell" (object ["cell_id" .= cid]))
    pure $ case out of
        Right (ToolOk (Object o))
            | Just (String e) <- KM.lookup (K.fromText "error") o
            , not (T.null e) ->
                Just e
        _ -> Nothing
