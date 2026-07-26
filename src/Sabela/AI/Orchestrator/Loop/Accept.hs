{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Orchestrator.Loop.Accept (
    liveOwned,
    verifyDownstream,
    downstreamDependents,
    looksLikeToolCallText,
    maxReenter,
    reenterNudge,
    toolCallNudge,
) where

import Control.Monad (forM)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Sabela.AI.Capabilities.Edit.Run (executeCell)
import Sabela.AI.Health (healthOfCellError, healthOfResult)
import Sabela.AI.Owned (OwnedCell (..))
import Sabela.AI.Types
import Sabela.Handlers (ReactiveNotebook)
import Sabela.Model (Cell, cellError, cellId, cellSource, lookupCell)
import Sabela.Reactivity (haskellCodeCells)
import Sabela.State (App (..), readNotebook)
import Sabela.Topo (TopoResult (..), selectAffectedTopoFrom)

liveOwned :: App -> Map Int OwnedCell -> IO (Map Int OwnedCell)
liveOwned app owned = do
    nb <- readNotebook (appNotebook app)
    pure (Map.mapMaybeWithKey (reverify nb) owned)
  where
    reverify nb cid oc = case lookupCell cid nb of
        Just cell
            | cellSource cell == ocSource oc ->
                Just oc{ocHealth = healthOfCellError (cellError cell)}
        _ -> Nothing

verifyDownstream ::
    App -> ReactiveNotebook -> Turn -> Map Int OwnedCell -> IO (Map Int OwnedCell)
verifyDownstream app rn turn owned
    | Map.null owned = pure Map.empty
    | otherwise = do
        nb <- readNotebook (appNotebook app)
        let dependents = downstreamDependents owned (haskellCodeCells nb)
        fmap Map.fromList $ forM dependents $ \cid -> do
            res <- executeCell app rn cid (turnCancel turn)
            pure (cid, OwnedCell (healthOfResult res) "")

downstreamDependents :: Map Int a -> [Cell] -> [Int]
downstreamDependents owned cells =
    [ cellId c
    | c <- trOrdered (fst (selectAffectedTopoFrom (Map.keysSet owned) cells))
    , not (Map.member (cellId c) owned)
    ]

maxReenter :: Int
maxReenter = 2

reenterNudge :: [Int] -> Text
reenterNudge reds =
    "These cells you wrote still have errors: "
        <> T.intercalate ", " (map (T.pack . show) reds)
        <> ". Fix them before finishing — read the error and repair or rewrite the cell."

looksLikeToolCallText :: Text -> Bool
looksLikeToolCallText t = case Aeson.decodeStrict (TE.encodeUtf8 (T.strip t)) of
    Just (Object o) -> any (\k -> KM.member (AK.fromText k) o) toolArgKeys
    _ -> False
  where
    toolArgKeys =
        [ "query"
        , "module"
        , "name"
        , "source"
        , "code"
        , "cell_id"
        , "new_source"
        , "expr"
        , "goal"
        ]

toolCallNudge :: Text
toolCallNudge =
    "That looked like a tool call written as text. Invoke it through the tool\
    \ interface instead of writing the JSON in a message."
