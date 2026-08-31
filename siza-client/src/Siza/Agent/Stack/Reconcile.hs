{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Stack.Reconcile (reconcileAwait) where

import Control.Monad (forM)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.CellResult (CellId)
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Owned (OwnedCell (..))
import Siza.Agent.Stack (Dispatch, StackSession, ownedCells, recordCall)

reconcileAwait ::
    StackSession ->
    Dispatch ->
    ToolCall ->
    Either Text ToolOutcome ->
    IO ([(ToolCall, Either Text ToolOutcome)], [Text])
reconcileAwait ss disp call outcome
    | tcName call == "await_idle"
    , settledAwait outcome = do
        before <- ownedCells ss
        let deferred =
                Map.keys
                    (Map.filter (\oc -> ocArtifactEligible oc && not (ocExecuted oc)) before)
        if null deferred
            then pure ([], [])
            else reconcile deferred
    | otherwise = pure ([], [])
  where
    reconcile deferred = do
        let listed = ToolCall "list_cells" (object [])
        listOut <- disp listed
        recordCall ss (listed, listOut)
        afterList <- ownedCells ss
        readSteps <- forM (failing deferred afterList) $ \cid -> do
            let readCall = ToolCall "read_cell" (object ["cell_id" .= cid])
            readOut <- disp readCall
            recordCall ss (readCall, readOut)
            pure (readCall, readOut)
        final <- ownedCells ss
        pure ((listed, listOut) : readSteps, failureNotes deferred final)

settledAwait :: Either Text ToolOutcome -> Bool
settledAwait (Right (ToolOk (Object o))) = case KM.lookup "waited" o of
    Just (String tag) -> tag == "idle" || tag == "settled"
    _ -> False
settledAwait _ = False

failing :: [CellId] -> Map.Map CellId OwnedCell -> [CellId]
failing targets owned =
    [cid | cid <- targets, Just oc <- [Map.lookup cid owned], not (ocHealthy oc)]

failureNotes :: [CellId] -> Map.Map CellId OwnedCell -> [Text]
failureNotes targets owned =
    [ "Settled owned cell "
        <> T.pack (show cid)
        <> " failed: "
        <> T.take 500 (ocDiagnostic oc)
    | cid <- targets
    , Just oc <- [Map.lookup cid owned]
    , not (ocHealthy oc)
    ]
