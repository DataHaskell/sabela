{- |
Technique: turn wrap-up [Episode]: budget nudges, unconfirmed writes, final stop line.
Guarantee: 'WriteEcho' counts unconfirmed writes by parsing sabela's 'AckEnvelope'.
Entry: 'budgetView'. Next: Siza.Agent.Loop.Verdict.
-}
module Siza.Agent.Loop.WrapUp (
    BudgetView (..),
    budgetView,
    countUnconfirmed,
    escalationK,
    finalTurnMarker,
    isWrapUpNudge,
    missRungFloor,
    windDownMarker,
    wrapUpDue,
    wrapUpFinal,
    wrapUpFinalUnconfirmed,
    wrapUpMarker,
    wrapUpMsg,
    wrapUpOnce,
) where

import Data.Aeson (Value, object, (.=))
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.CellResult (CellId)
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.AI.WriteAck (AckEnvelope (..), parseAckEnvelope)
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.GrammarCards (isOwningTool)
import Siza.Agent.Loop.Support (factsBlock, nudgeFloor, nudgeK)
import Siza.Agent.Owned (
    OwnedCell (..),
    hasArtifact,
    newestFailing,
    ownedCellOutcome,
 )

data BudgetView = BudgetView
    { bvTurnsLeft :: Int
    , bvRepairsLeft :: Int
    , bvRepairsSpent :: Int
    , bvTimeLeftFrac :: Double
    }
    deriving (Eq, Show)

budgetView :: Int -> Int -> Int -> Int -> Double -> Double -> BudgetView
budgetView maxTurns turn maxRepairs repairs elapsed deadline =
    BudgetView
        { bvTurnsLeft = maxTurns - turn
        , bvRepairsLeft = maxRepairs - repairs
        , bvRepairsSpent = repairs
        , bvTimeLeftFrac = timeLeftFrac elapsed deadline
        }

timeLeftFrac :: Double -> Double -> Double
timeLeftFrac elapsed deadline
    | isInfinite deadline || isNaN deadline = 1
    | deadline <= 0 = 0
    | otherwise = max 0 ((deadline - elapsed) / deadline)

wrapUpDue :: BudgetView -> Bool
wrapUpDue bv =
    bvTurnsLeft bv <= 1
        || (bvRepairsSpent bv > 0 && bvRepairsLeft bv <= 1)
        || bvTimeLeftFrac bv <= 0.1

finalTurnMarker :: Text
finalTurnMarker = "Final turn:"

windDownMarker :: Text
windDownMarker = "Wind down:"

{- | The opener asserts only what the fired budget guarantees: a repair round
or a deadline leaves the loop free to read many more replies.
-}
wrapUpMarker :: BudgetView -> Text
wrapUpMarker bv
    | bvTurnsLeft bv <= 1 = finalTurnMarker
    | otherwise = windDownMarker

-- | Recognises a wrap-up nudge whichever budget opened it.
isWrapUpNudge :: Text -> Bool
isWrapUpNudge c = any (`T.isInfixOf` c) [finalTurnMarker, windDownMarker]

wrapUpMsg :: [Text] -> BudgetView -> Value
wrapUpMsg facts bv =
    object
        [ "role" .= ("user" :: Text)
        , "content"
            .= ( wrapUpMarker bv
                    <> " "
                    <> budgetLine bv
                    <> "."
                    <> finalityClause bv
                    <> " If one write completes the deliverable, make it now \
                       \(insert_cell / replace_cell_source); otherwise \
                       \summarise what was accomplished and state any \
                       \blocker plainly. Do not search further."
                    <> factsBlock facts
               )
        ]

{- | Only the turn budget makes the next reply provably the last one read: a
repair round or a deadline can leave several more replies to come.
-}
finalityClause :: BudgetView -> Text
finalityClause bv
    | bvTurnsLeft bv <= 1 = " This is the last reply that will be read."
    | otherwise = ""

budgetLine :: BudgetView -> Text
budgetLine bv
    | bvTurnsLeft bv <= 1 = "the turn budget ends after this reply"
    | bvRepairsSpent bv > 0 && bvRepairsLeft bv <= 1 =
        "the repair budget ends after this round"
    | otherwise = "the time budget is nearly spent"

wrapUpOnce :: IORef Bool -> IO [Text] -> BudgetView -> IO [Value]
wrapUpOnce ref getFacts bv
    | not (wrapUpDue bv) = pure []
    | otherwise = do
        fired <- readIORef ref
        if fired
            then pure []
            else do
                writeIORef ref True
                facts <- getFacts
                pure [wrapUpMsg facts bv]

-- | 'wrapUpFinalUnconfirmed' for an episode whose every write came back.
wrapUpFinal :: Text -> Map CellId OwnedCell -> Text -> Text
wrapUpFinal = wrapUpFinalUnconfirmed 0

wrapUpFinalUnconfirmed ::
    Int -> Text -> Map CellId OwnedCell -> Text -> Text
wrapUpFinalUnconfirmed unconfirmed stopped owned candidate
    | not (T.null (T.strip candidate)) = candidate
    | otherwise =
        "Stopped (" <> stopped <> "): " <> stateLine unconfirmed owned

{- | What the harness learned about a dispatched write. A refusal it read is
an answer about the notebook; only a reply it never received is not.
-}
data WriteEcho = Recorded | Refused | Unheard
    deriving (Eq, Show)

{- | The transport can drop a reply after the server has applied the write, so
a write with no answer leaves the notebook unknown. A refusal states what
happened to it, and an empty notebook is then observed rather than unknown.
-}
writeEcho :: ToolCall -> Either Text ToolOutcome -> WriteEcho
writeEcho tc out
    | Just _ <- ownedCellOutcome tc out = Recorded
    | Right o <- out, statesOutcome o = Refused
    | otherwise = Unheard

-- | An errored tool, or a payload naming why nothing was committed.
statesOutcome :: ToolOutcome -> Bool
statesOutcome (ToolErr _) = True
statesOutcome (ToolOk v) = case parseAckEnvelope v of
    Just (EnvRefusal _) -> True
    Just (EnvBusy _) -> True
    _ -> False

-- | The dispatched writes the harness never heard back about.
countUnconfirmed :: [(ToolCall, Either Text ToolOutcome)] -> Int
countUnconfirmed steps =
    length
        [ ()
        | (tc, out) <- steps
        , isOwningTool (tcName tc)
        , writeEcho tc out == Unheard
        ]

{- | The terminal state as the unresolved point, not as a headcount: a cell
count says nothing about whether the request was answered.
-}
stateLine :: Int -> Map CellId OwnedCell -> Text
stateLine unconfirmed owned
    | Map.null owned, unconfirmed > 0 = unconfirmedLine unconfirmed
    | Map.null owned = "no cell was written before the episode ended."
    | otherwise = recordedLine owned <> unconfirmedNote unconfirmed

unconfirmedLine :: Int -> Text
unconfirmedLine n =
    tShow n
        <> " write(s) were dispatched and no reply came back, so what \
           \reached the notebook is unknown."

unconfirmedNote :: Int -> Text
unconfirmedNote n
    | n <= 0 = ""
    | otherwise =
        " A further "
            <> tShow n
            <> " write(s) got no reply, so their effect is unknown."

recordedLine :: Map CellId OwnedCell -> Text
recordedLine owned = case newestFailing owned of
    Nothing
        | not (hasArtifact owned) ->
            tShow (Map.size owned)
                <> " cell(s) written, none of them substantive \8212 no \
                   \deliverable was committed."
    Nothing ->
        tShow (Map.size owned)
            <> " cell(s) written and healthy; the episode ended before a \
               \summary was written."
    Just red ->
        tShow (Map.size owned)
            <> " cell(s) written, "
            <> tShow redCount
            <> " still failing. Last diagnostic: "
            <> T.take 280 (ocDiagnostic red)
  where
    redCount = Map.size (Map.filter (not . ocHealthy) owned)

escalationK :: Int -> Int -> Int
escalationK total remaining
    | 2 * remaining > total = nudgeK
    | otherwise = 1

missRungFloor :: Int -> Int -> Int
missRungFloor total remaining
    | remaining <= nudgeFloor = 3
    | 2 * remaining <= total = 2
    | otherwise = 1

tShow :: Int -> Text
tShow = T.pack . show
