module Siza.Agent.Discover.Dedup (ledgerShortcutStep) where

import Data.Aeson (Value)
import Data.Text (Text)

import Siza.Agent.Discover.Goal (goalProduced)
import Siza.Agent.Discover.History (ledgerShortcut)
import Siza.Agent.Discover.Ledger (
    SearchLedger (..),
    discoverRepeat,
    ledgerArm,
    ledgerUnspentGoal,
 )
import Siza.Agent.Discover.MissLadder (withCandidate)
import Siza.Agent.Discover.Types (StandingGoal (..))

ledgerShortcutStep :: SearchLedger -> Text -> (SearchLedger, Maybe Value)
ledgerShortcutStep led q
    | slHardClosed led = escalating (discoverRepeat q led)
    | Just out <- ledgerShortcut led q =
        let (led', hard) = discoverRepeat q led
         in escalating
                ( led'
                , case fmap (withCandidate (slRefuted led') (slFacts led')) hard of
                    Just stop -> Just stop
                    Nothing -> Just out
                )
    | otherwise = (led, Nothing)

{- | A duplicate is the miss rung's evidence in another shape — the same
question again with the goal still standing — so it spends the goal's one type
query on the same terms, and not when the hits it hands back produce the goal.
-}
escalating :: (SearchLedger, Maybe Value) -> (SearchLedger, Maybe Value)
escalating (led, Nothing) = (led, Nothing)
escalating (led, Just out) = (armed, Just out)
  where
    armed = case ledgerUnspentGoal led of
        Just sg | not (goalProduced (sgType sg) out) -> ledgerArm sg led
        _ -> led
