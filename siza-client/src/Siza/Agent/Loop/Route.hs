{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Loop.Route (
    blockingCell,
    discloseRoute,
    routedRetryNote,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Types (ToolOutcome (..), toolOutcomeValue)
import Sabela.AI.WriteAck (AckEnvelope (..), RefusalAck (..), parseAckEnvelope)

blockingCell :: Either Text ToolOutcome -> Maybe Int
blockingCell (Right out)
    | Just (EnvRefusal ra) <- parseAckEnvelope (toolOutcomeValue out)
    , raKind ra == "pending-error" =
        raCell ra
blockingCell _ = Nothing

routedRetryNote :: Int -> Text
routedRetryNote n =
    "insert was blocked by red cell "
        <> tShow n
        <> "; re-applied your source as replace_cell_source on cell "
        <> tShow n
        <> " to clear the dam (routed retry)."
  where
    tShow = T.pack . show

discloseRoute :: Int -> ToolOutcome -> ToolOutcome
discloseRoute n out = case out of
    ToolOk v -> ToolOk (annotate v)
    ToolErr v -> ToolErr (annotate v)
  where
    annotate (Object o) =
        Object (KM.insertWith keepExisting (K.fromText "routedRetry") note o)
    annotate v = v
    note = String (routedRetryNote n)
    keepExisting _ old = old
