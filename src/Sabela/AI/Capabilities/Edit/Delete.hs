{-# LANGUAGE OverloadedStrings #-}

{- | The delete tool, behind the orphan gate: a delete that would strip the
last declaration of a dependency some surviving cell imports is refused with
the orphan named, because the survivor would break on any rebuild.
-}
module Sabela.AI.Capabilities.Edit.Delete (execDeleteCell) where

import Data.Aeson (Value, object, (.=))

import Sabela.AI.Capabilities.Edit.OrphanGate (deleteOrphans, orphanRefusal)
import Sabela.AI.Capabilities.Util (fieldInt)
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.Api (errorJson)
import Sabela.Model (Cell (..), Notebook (..))
import Sabela.State

execDeleteCell :: App -> Value -> IO ToolOutcome
execDeleteCell app input = do
    let mcid = fieldInt "cell_id" input
    case mcid of
        Nothing -> pure (errOutcome (errorJson "cell_id required"))
        Just cid -> do
            nb <- readNotebook (appNotebook app)
            orphans <- deleteOrphans (envGlobalDeps (appEnv app)) nb cid
            if not (null orphans)
                then pure (errOutcome (orphanRefusal cid orphans))
                else do
                    modifyNotebook (appNotebook app) $ \current ->
                        current
                            { nbCells =
                                filter
                                    (\c -> cellId c /= cid)
                                    (nbCells current)
                            }
                    broadcastNotebook app
                    pure
                        ( okOutcome
                            (object ["deleted" .= True, "cellId" .= cid])
                        )
