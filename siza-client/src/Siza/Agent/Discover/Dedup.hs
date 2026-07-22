module Siza.Agent.Discover.Dedup (ledgerShortcutStep) where

import Data.Aeson (Value)
import Data.Text (Text)

import Siza.Agent.Discover.History (ledgerShortcut)
import Siza.Agent.Discover.Ledger (SearchLedger (..), discoverRepeat)
import Siza.Agent.Discover.MissLadder (withCandidate)

-- | Stateful dispatch shortcut: exact repeats count toward the hard gate.
ledgerShortcutStep :: SearchLedger -> Text -> (SearchLedger, Maybe Value)
ledgerShortcutStep led q
    | slHardClosed led = discoverRepeat q led
    | Just out <- ledgerShortcut led q =
        let (led', hard) = discoverRepeat q led
         in ( led'
            , case fmap (withCandidate (slFacts led')) hard of
                Just stop -> Just stop
                Nothing -> Just out
            )
    | otherwise = (led, Nothing)
