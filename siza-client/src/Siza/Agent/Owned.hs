module Siza.Agent.Owned (
    OwnedCell (..),
    StopDecision (..),
    recordOwned,
    stopDecision,
    ownedCellOutcome,
    bestFailing,
    latestDraft,
    redSignature,
    noProgressStep,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Sabela.AI.CellResult (CellId)
import Sabela.AI.Types (ToolOutcome (..))

import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover (isOwningTool, toolCallSource)
import Siza.Agent.Tools (renderOutcome)

-- | Latest health, rendered diagnostic, and source for a cell this episode wrote.
data OwnedCell = OwnedCell
    { ocHealthy :: Bool
    , ocDiagnostic :: Text
    , ocSource :: Text
    }

-- | The harness's verdict when the model stops calling tools.
data StopDecision = Stop | Reenter [CellId]
    deriving (Eq, Show)

-- | Fold one (call, outcome) into the owned-cell health map.
recordOwned ::
    (ToolCall, Either Text ToolOutcome) ->
    Map CellId OwnedCell ->
    Map CellId OwnedCell
recordOwned (tc, out) m = case ownedCellOutcome tc out of
    Just (cid, healthy) ->
        Map.insert cid (OwnedCell healthy (renderOutcome out) (toolCallSource tc)) m
    Nothing -> m

stopDecision :: Map CellId Bool -> StopDecision
stopDecision owned = case [cid | (cid, ok) <- Map.toList owned, not ok] of
    [] -> Stop
    reds -> Reenter reds

ownedCellOutcome :: ToolCall -> Either Text ToolOutcome -> Maybe (CellId, Bool)
ownedCellOutcome tc out
    | not (isOwningTool (tcName tc)) = Nothing
    | Right (ToolOk (Object o)) <- out
    , Just cid <- intField "cellId" o =
        Just (cid, cellOk o)
    | otherwise = Nothing

bestFailing :: Map CellId OwnedCell -> Text
bestFailing owned =
    case [oc | oc <- Map.elems owned, not (ocHealthy oc)] of
        (oc : _) -> "Gave up with a failing cell. Last diagnostic: " <> ocDiagnostic oc
        [] -> ""

{- | The model's own most recent draft (R3.8): the source of the highest-id
owned cell — the candidate seed nearest the proposer, mined as generator input
only ('candidateCellFrom' vets it), never trusted as a fact. 'Nothing' when the
episode has written nothing to seed from.
-}
latestDraft :: Map CellId OwnedCell -> Maybe Text
latestDraft owned = ocSource . snd <$> Map.lookupMax owned

{- | A stable no-progress signature for the still-red owned cells: each red cell
paired with its rendered diagnostic, sorted. An identical signature across two
reenter rounds means nothing changed — the gate is spinning and should stop.
-}
redSignature :: [CellId] -> Map CellId OwnedCell -> [(CellId, Text)]
redSignature reds owned =
    sort [(cid, ocDiagnostic oc) | cid <- reds, Just oc <- [Map.lookup cid owned]]

{- | One reenter-guard step: the updated seen-set plus whether this signature has
been seen before (no progress). Tracking a SET catches an A/B/A/B oscillation,
which a compare-to-previous check resets on every round and never trips.
-}
noProgressStep ::
    Set [(CellId, Text)] -> [(CellId, Text)] -> (Set [(CellId, Text)], Bool)
noProgressStep seen sig = (Set.insert sig seen, sig `Set.member` seen)

cellOk :: KM.KeyMap Value -> Bool
cellOk o = case KM.lookup "execution" o of
    Just (Object e) -> boolField "ok" e
    _ -> boolField "ok" o

intField :: Text -> KM.KeyMap Value -> Maybe CellId
intField k o = case KM.lookup (K.fromText k) o of
    Just (Number s) -> Just (round s)
    _ -> Nothing

boolField :: Text -> KM.KeyMap Value -> Bool
boolField k o = KM.lookup (K.fromText k) o == Just (Bool True)
