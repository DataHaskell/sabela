module Sabela.AI.Owned (
    MutationEvent (..),
    OwnedCell (..),
    StopDecision (..),
    recordMutation,
    stopDecision,
    ownershipStale,
    bestFailing,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Sabela.AI.Capabilities.ToolName (ToolName, actsOnNotebook)
import Sabela.AI.CellResult (CellId)
import Sabela.AI.Health (Health, isClean)

data MutationEvent = MutationEvent
    { meTool :: ToolName
    , meCellId :: CellId
    , meSource :: Text
    , meHealth :: Health
    }
    deriving (Eq, Show)

data OwnedCell = OwnedCell
    { ocHealth :: Health
    , ocSource :: Text
    }
    deriving (Eq, Show)

data StopDecision = Stop | Reenter [CellId]
    deriving (Eq, Show)

recordMutation :: MutationEvent -> Map CellId OwnedCell -> Map CellId OwnedCell
recordMutation me
    | actsOnNotebook (meTool me) =
        Map.insert (meCellId me) (OwnedCell (meHealth me) (meSource me))
    | otherwise = id

stopDecision :: Map CellId OwnedCell -> StopDecision
stopDecision owned =
    case [cid | (cid, oc) <- Map.toList owned, not (isClean (ocHealth oc))] of
        [] -> Stop
        reds -> Reenter reds

ownershipStale :: CellId -> Text -> Map CellId OwnedCell -> Bool
ownershipStale cid currentSrc owned = case Map.lookup cid owned of
    Just oc -> ocSource oc /= currentSrc
    Nothing -> False

bestFailing :: Map CellId OwnedCell -> Maybe OwnedCell
bestFailing owned =
    case [oc | oc <- Map.elems owned, not (isClean (ocHealth oc))] of
        (oc : _) -> Just oc
        [] -> Nothing
