{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Owned.Snapshot (
    OwnedCell (..),
    recordSnapshot,
    recordRead,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.CellResult (CellId)
import Sabela.AI.Types (ToolOutcome (..))

data OwnedCell = OwnedCell
    { ocHealthy :: Bool
    , ocExecuted :: Bool
    , ocDiagnostic :: Text
    , ocSource :: Text
    , ocInvariantAlarm :: Bool
    , ocArtifactEligible :: Bool
    , ocHash :: Maybe Text
    }

recordSnapshot ::
    Either Text ToolOutcome -> Map CellId OwnedCell -> Map CellId OwnedCell
recordSnapshot (Right (ToolOk (Object o))) m = case cellStates o of
    Nothing -> m
    Just states -> Map.mapMaybeWithKey (refresh (Map.fromList states)) m
  where
    refresh states cid oc = do
        (dirty, hasError, hash, eligible) <- Map.lookup cid states
        if maybe False (/= hash) (ocHash oc)
            then Nothing
            else
                Just
                    oc
                        { ocHealthy = not hasError
                        , ocExecuted = not dirty
                        , ocArtifactEligible = eligible
                        , ocDiagnostic = snapshotDiagnostic cid hasError
                        , ocHash = Just hash
                        }
recordSnapshot _ m = m

snapshotDiagnostic :: CellId -> Bool -> Text
snapshotDiagnostic cid True =
    "list_cells observed hasError=true for cell " <> T.pack (show cid)
snapshotDiagnostic _ False = ""

cellStates :: KM.KeyMap Value -> Maybe [(CellId, (Bool, Bool, Text, Bool))]
cellStates o = case KM.lookup "cells" o of
    Just (Array xs) -> traverse stateOf (foldr (:) [] xs)
    _ -> Nothing
  where
    stateOf (Object cell) = do
        cid <- intField "id" cell
        dirty <- boolField "dirty" cell
        hasError <- boolField "hasError" cell
        hash <- textField "hash" cell
        cellType <- textField "type" cell
        pure (cid, (dirty, hasError, hash, cellType == "CodeCell"))
    stateOf _ = Nothing

recordRead ::
    Maybe CellId ->
    Either Text ToolOutcome ->
    Map CellId OwnedCell ->
    Map CellId OwnedCell
recordRead _ (Right (ToolOk (Object o))) m
    | Just cid <- intField "id" o
    , Just hash <- textField "hash" o
    , Just source <- textField "source" o
    , Just err <- errorField o =
        Map.update (refresh hash source err) cid m
  where
    refresh hash source err oc
        | maybe False (/= hash) (ocHash oc) = Nothing
        | otherwise =
            Just
                oc
                    { ocHealthy = maybe (ocHealthy oc) (const False) err
                    , ocDiagnostic = fromMaybe (ocDiagnostic oc) err
                    , ocSource = source
                    , ocHash = Just hash
                    }
recordRead (Just cid) out m
    | Just diagnostic <- readFailure out =
        Map.adjust
            ( \oc ->
                oc
                    { ocHealthy = False
                    , ocDiagnostic =
                        "read_cell failed while retrieving the diagnostic: " <> diagnostic
                    }
            )
            cid
            m
recordRead _ _ m = m

readFailure :: Either Text ToolOutcome -> Maybe Text
readFailure (Left e) = Just e
readFailure (Right (ToolErr (Object o))) = case KM.lookup "error" o of
    Just (String e) -> Just e
    _ -> Just "tool error"
readFailure (Right (ToolErr _)) = Just "tool error"
readFailure _ = Nothing

errorField :: KM.KeyMap Value -> Maybe (Maybe Text)
errorField o = case KM.lookup "error" o of
    Just Null -> Just Nothing
    Just (String e) -> Just (Just e)
    _ -> Nothing

intField :: Text -> KM.KeyMap Value -> Maybe CellId
intField k o = case KM.lookup (K.fromText k) o of
    Just (Number s) -> Just (round s)
    _ -> Nothing

boolField :: Text -> KM.KeyMap Value -> Maybe Bool
boolField k o = case KM.lookup (K.fromText k) o of
    Just (Bool b) -> Just b
    _ -> Nothing

textField :: Text -> KM.KeyMap Value -> Maybe Text
textField k o = case KM.lookup (K.fromText k) o of
    Just (String s) -> Just s
    _ -> Nothing
