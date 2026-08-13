{- |
Technique: client echo of the server compile gate, plus the stop rule [Gating/Repair].
Guarantee: only a landed (compiling) artifact counts as delivered.
Entry: 'landedArtifact', 'stopDecision'. Siblings: Siza.Agent.Futility (dispatch) and Siza.Agent.Streak (cell) detect repetition too.
-}
module Siza.Agent.Owned (
    OwnedCell (..),
    StopDecision (..),
    hasArtifact,
    noWriteReason,
    landedArtifact,
    recordOwned,
    stopDecision,
    substantive,
    ownedCellOutcome,
    bestFailing,
    latestDraft,
    newestFailing,
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
import qualified Data.Text as T
import Sabela.AI.CellResult (CellId)
import Sabela.AI.Types (ToolOutcome (..))

import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Check (NoVerdict (..))
import Siza.Agent.GrammarCards (isOwningTool, toolCallSource)
import Siza.Agent.Render (renderOutcome)

data OwnedCell = OwnedCell
    { ocHealthy :: Bool
    , ocDiagnostic :: Text
    , ocSource :: Text
    , ocInvariantAlarm :: Bool
    }

data StopDecision = Stop | Reenter [CellId]
    deriving (Eq, Show)

recordOwned ::
    (ToolCall, Either Text ToolOutcome) ->
    Map CellId OwnedCell ->
    Map CellId OwnedCell
recordOwned (tc, out) m
    | tcName tc == "delete_cell" = maybe m (`Map.delete` m) (deletedCellId out)
    | otherwise = case ownedCellOutcome tc out of
        Just (cid, healthy) ->
            Map.insert
                cid
                (OwnedCell healthy (renderOutcome out) (toolCallSource tc) (rejectedClass out))
                m
        Nothing -> m

rejectedClass :: Either Text ToolOutcome -> Bool
rejectedClass (Right (ToolOk (Object o))) = outcomeTagIs "Rejected" o
rejectedClass _ = False

outcomeTagIs :: Text -> KM.KeyMap Value -> Bool
outcomeTagIs want o = tagField (execObject o)
  where
    execObject e = case KM.lookup "execution" e of
        Just (Object inner) -> inner
        _ -> e
    tagField e = case KM.lookup "outcome" e of
        Just (Object oc) -> KM.lookup "tag" oc == Just (String want)
        _ -> False

deletedCellId :: Either Text ToolOutcome -> Maybe CellId
deletedCellId (Right (ToolOk (Object o)))
    | boolField "deleted" o = intField "cellId" o
deletedCellId _ = Nothing

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
bestFailing owned = case newestFailing owned of
    Just oc -> "Gave up with a failing cell. Last diagnostic: " <> ocDiagnostic oc
    Nothing -> ""

newestFailing :: Map CellId OwnedCell -> Maybe OwnedCell
newestFailing owned =
    case Map.toDescList (Map.filter (not . ocHealthy) owned) of
        ((_, oc) : _) -> Just oc
        [] -> Nothing

hasArtifact :: Map CellId OwnedCell -> Bool
hasArtifact = any (substantive . ocSource) . Map.elems

{- | Why this turn has nothing to check, or 'Nothing' when it has something.
Read from the same map 'hasArtifact' reads, so the reason reported can never
contradict the cells recorded.
-}
noWriteReason :: Map CellId OwnedCell -> Maybe NoVerdict
noWriteReason owned
    | hasArtifact owned = Nothing
    | Map.null owned = Just NoCellCommitted
    | otherwise = Just (NoExecutableCell (Map.size owned))

{- | The one predicate for "a write that counts": committed, clean, and
carrying more than comments and pragmas. Every "did this turn deliver
anything" asks this, so no weaker notion can drift from 'hasArtifact'.
-}
landedArtifact :: (ToolCall, Either Text ToolOutcome) -> Bool
landedArtifact (tc, out) =
    maybe False snd (ownedCellOutcome tc out) && substantive (toolCallSource tc)

substantive :: Text -> Bool
substantive src =
    not (null [l | l <- map T.strip (T.lines src), not (isPreamble l)])
  where
    isPreamble l =
        T.null l
            || "--" `T.isPrefixOf` l
            || "{-#" `T.isPrefixOf` l

latestDraft :: Map CellId OwnedCell -> Maybe Text
latestDraft owned = ocSource . snd <$> Map.lookupMax owned

redSignature :: [CellId] -> Map CellId OwnedCell -> [(CellId, Text)]
redSignature reds owned =
    sort [(cid, ocDiagnostic oc) | cid <- reds, Just oc <- [Map.lookup cid owned]]

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
