{- | Ownership of the cells an agent turn wrote, for the closure-verified accept.
The turn completes only when every owned cell is clean; a red one drives a repair
re-entry. Keyed by the cell's source at mutation time.
-}
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

{- | An agent mutation of one cell: which tool, which cell, the source committed
(the revision stamp), and the resulting health.
-}
data MutationEvent = MutationEvent
    { meTool :: ToolName
    , meCellId :: CellId
    , meSource :: Text
    , meHealth :: Health
    }
    deriving (Eq, Show)

-- | The latest health and source (revision) of a cell the turn owns.
data OwnedCell = OwnedCell
    { ocHealth :: Health
    , ocSource :: Text
    }
    deriving (Eq, Show)

-- | The accept gate's verdict once the model stops calling tools.
data StopDecision = Stop | Reenter [CellId]
    deriving (Eq, Show)

{- | Fold a mutation into the owned map. Only cells written by a notebook-acting
tool ('actsOnNotebook') are owned; a read/query tool records nothing.
-}
recordMutation :: MutationEvent -> Map CellId OwnedCell -> Map CellId OwnedCell
recordMutation me
    | actsOnNotebook (meTool me) =
        Map.insert (meCellId me) (OwnedCell (meHealth me) (meSource me))
    | otherwise = id

-- | 'Stop' when every owned cell is clean; otherwise 'Reenter' the red ones.
stopDecision :: Map CellId OwnedCell -> StopDecision
stopDecision owned =
    case [cid | (cid, oc) <- Map.toList owned, not (isClean (ocHealth oc))] of
        [] -> Stop
        reds -> Reenter reds

{- | Is ownership of @cid@ stale — the cell's current source differs from what
the agent committed (a concurrent edit)? 'False' for a cell the turn does not own.
-}
ownershipStale :: CellId -> Text -> Map CellId OwnedCell -> Bool
ownershipStale cid currentSrc owned = case Map.lookup cid owned of
    Just oc -> ocSource oc /= currentSrc
    Nothing -> False

-- | The first red owned cell (for surfacing why a turn gave up), if any.
bestFailing :: Map CellId OwnedCell -> Maybe OwnedCell
bestFailing owned =
    case [oc | oc <- Map.elems owned, not (isClean (ocHealth oc))] of
        (oc : _) -> Just oc
        [] -> Nothing
