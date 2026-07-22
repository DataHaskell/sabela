{-# LANGUAGE OverloadedStrings #-}

{- | Routed unblock (R6.8/R7.1): a pending-error refusal to an insert names the
blocking cell. Rather than let the model insert-retry against the dam (the
topMonth turn-30 trap, where the same blocked insert consumed turn after turn),
the loop re-dispatches the model's own source through replace_cell_source on
that cell — one bounded, disclosed hop that clears the dam.
-}
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

{- | The cell a pending-error refusal names as blocking, if this outcome is one
— decoded through the one write-ack envelope validator, never a string scrape.
-}
blockingCell :: Either Text ToolOutcome -> Maybe Int
blockingCell (Right out)
    | Just (EnvRefusal ra) <- parseAckEnvelope (toolOutcomeValue out)
    , raKind ra == "pending-error" =
        raCell ra
blockingCell _ = Nothing

-- | The routed-retry disclosure attached to the replace outcome (R7.1).
routedRetryNote :: Int -> Text
routedRetryNote n =
    "insert was blocked by red cell "
        <> tShow n
        <> "; re-applied your source as replace_cell_source on cell "
        <> tShow n
        <> " to clear the dam (routed retry)."
  where
    tShow = T.pack . show

-- | Record the routed-retry disclosure on the replace outcome's envelope.
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
