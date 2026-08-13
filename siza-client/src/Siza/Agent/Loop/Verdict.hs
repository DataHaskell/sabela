{- |
Technique: verdict channel routing [Episode].
Guarantee: only a check that ran and failed may carry a counterexample.
Entry: 'verdictMsg'. Next: Siza.Agent.Messages. Trap: "health_gate" is a synthetic tool name.
-}
module Siza.Agent.Loop.Verdict (
    verdictMsg,
    unconfirmedDiagMsg,
) where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Sabela.AI.CellResult (CellId)
import Siza.Agent.Check (CheckResult (..))
import Siza.Agent.Deliverable (missingDeliverables)
import Siza.Agent.Messages (unconfirmedMsgWith, verifyMsgWith)
import Siza.Agent.Owned (OwnedCell (..))

{- | Only a check that ran and failed is a diagnostic; its detail is a
counterexample. Every other outcome reached no verdict, so its detail is the
reason there is none and is reported on the could-not-run channel.
-}
verdictMsg ::
    Text -> CheckResult -> Maybe Text -> Map CellId OwnedCell -> Value
verdictMsg prompt CheckFailed mDetail owned =
    verifyMsgWith (Map.size owned) (missing prompt owned) mDetail
verdictMsg prompt _ mDetail owned = unconfirmedDiagMsg prompt mDetail owned

unconfirmedDiagMsg ::
    Text -> Maybe Text -> Map CellId OwnedCell -> Value
unconfirmedDiagMsg prompt mEv owned =
    unconfirmedMsgWith (Map.size owned) (missing prompt owned) mEv

missing :: Text -> Map CellId OwnedCell -> [Text]
missing prompt owned =
    missingDeliverables prompt (map ocSource (Map.elems owned))
