module Eval.Owned (
    OwnedCell (..),
    StopDecision (..),
    recordOwned,
    stopDecision,
    ownedCellOutcome,
    bestFailing,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Sabela.AI.CellResult (CellId)
import Sabela.AI.Types (ToolOutcome (..))

import Eval.Discover (isOwningTool, toolCallSource)
import Eval.Ollama (ToolCall (..))
import Eval.Tools (renderOutcome)

-- | Latest health, rendered diagnostic, and source for a cell this episode wrote.
data OwnedCell = OwnedCell
    { ocHealthy :: Bool
    , ocDiagnostic :: Text
    , ocSource :: Text
    }

-- | The harness's verdict when the model stops calling tools.
data StopDecision = Stop | Reenter [CellId]
    deriving (Show, Eq)

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
