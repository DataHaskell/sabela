{-# LANGUAGE OverloadedStrings #-}

{- | Budget-exhaustion wrap-up (R8.3, R9.8) and budget-proportional escalation
(R5.6/R5.9): the last-turn message, the non-empty-final guarantee, and the
curves the loop wires into the nudge and miss ladder. Silent on happy paths.
-}
module Siza.Agent.Loop.WrapUp (
    BudgetView (..),
    budgetView,
    escalationK,
    missRungFloor,
    wrapUpDue,
    wrapUpFinal,
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
import Siza.Agent.Loop.Support (factsBlock, nudgeFloor, nudgeK)
import Siza.Agent.Owned (OwnedCell (..))

{- | What is left of every episode budget dimension, read at the top of a
turn: remaining turns, remaining and spent repair rounds, and the unspent
fraction of the wall-clock deadline.
-}
data BudgetView = BudgetView
    { bvTurnsLeft :: Int
    , bvRepairsLeft :: Int
    , bvRepairsSpent :: Int
    , bvTimeLeftFrac :: Double
    }
    deriving (Eq, Show)

-- | Assemble the view from the loop's counters and clocks.
budgetView :: Int -> Int -> Int -> Int -> Double -> Double -> BudgetView
budgetView maxTurns turn maxRepairs repairs elapsed deadline =
    BudgetView
        { bvTurnsLeft = maxTurns - turn
        , bvRepairsLeft = maxRepairs - repairs
        , bvRepairsSpent = repairs
        , bvTimeLeftFrac = timeLeftFrac elapsed deadline
        }

-- | Unspent deadline fraction; an unbounded or absurd deadline is never low.
timeLeftFrac :: Double -> Double -> Double
timeLeftFrac elapsed deadline
    | isInfinite deadline || isNaN deadline = 1
    | deadline <= 0 = 0
    | otherwise = max 0 ((deadline - elapsed) / deadline)

{- | Is the upcoming turn the episode's last (any dimension)? The repair
dimension counts only once repair spend exists, so a tight repair budget
never marks a happy path's first turn as final.
-}
wrapUpDue :: BudgetView -> Bool
wrapUpDue bv =
    bvTurnsLeft bv <= 1
        || (bvRepairsSpent bv > 0 && bvRepairsLeft bv <= 1)
        || bvTimeLeftFrac bv <= 0.1

-- | The stable head of the wrap-up message, for tests and transcript audits.
wrapUpMarker :: Text
wrapUpMarker = "Final turn:"

{- | The last-turn wrap-up (R8.3, R5.7): names the exhausted budget, asks for
a final write or a plain summary-plus-blocker, carries the held facts, and
never advises further searching.
-}
wrapUpMsg :: [Text] -> BudgetView -> Value
wrapUpMsg facts bv =
    object
        [ "role" .= ("user" :: Text)
        , "content"
            .= ( wrapUpMarker
                    <> " "
                    <> budgetLine bv
                    <> ". This is the last reply that will be read. If one \
                       \write completes the deliverable, make it now \
                       \(insert_cell / replace_cell_source); otherwise \
                       \summarise what was accomplished and state any \
                       \blocker plainly. Do not search further."
                    <> factsBlock facts
               )
        ]

-- | Which budget dimension ends the episode, for the wrap-up's first line.
budgetLine :: BudgetView -> Text
budgetLine bv
    | bvTurnsLeft bv <= 1 = "the turn budget ends after this reply"
    | bvRepairsSpent bv > 0 && bvRepairsLeft bv <= 1 =
        "the repair budget ends after this round"
    | otherwise = "the time budget is nearly spent"

{- | Inject the wrap-up exactly once per episode: fires on the first due
view, closing the search channel via @getFacts@ (R5.7) and carrying what it
held; every later call is silent.
-}
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

{- | The finish-time guarantee: a non-blank candidate passes through
byte-identically (R9.8); a blank one becomes a summary that names the stop
reason (R8.3) and the owned-cell state, so no stop reason can yield silence.
-}
wrapUpFinal :: Text -> Map CellId OwnedCell -> Text -> Text
wrapUpFinal stopped owned candidate
    | not (T.null (T.strip candidate)) = candidate
    | otherwise = "Stopped (" <> stopped <> "): " <> stateLine owned

-- | The owned-cell state line of a synthesised final, bounded (R3.9).
stateLine :: Map CellId OwnedCell -> Text
stateLine owned = case reds of
    _ | Map.null owned -> "no cell was written before the episode ended."
    [] ->
        tShow (Map.size owned)
            <> " cell(s) written and healthy; the episode ended before a \
               \summary was written."
    (red : _) ->
        tShow (Map.size owned)
            <> " cell(s) written, "
            <> tShow (length reds)
            <> " still failing. Last diagnostic: "
            <> T.take 280 (ocDiagnostic red)
  where
    reds = [oc | oc <- Map.elems owned, not (ocHealthy oc)]

{- | Budget-proportional act-nudge threshold (R5.6): consecutive read-only
calls tolerated before the nudge. 'nudgeK' while more than half the turn
budget remains; one — every read-only turn nudges — past mid-budget.
-}
escalationK :: Int -> Int -> Int
escalationK total remaining
    | 2 * remaining > total = nudgeK
    | otherwise = 1

{- | Budget-proportional miss-ladder floor (R5.6): the minimum escalation
rung of ANY miss, so a cluster-shifting spiral still hears the held facts by
mid-budget and the act-or-blocker line near the floor.
-}
missRungFloor :: Int -> Int -> Int
missRungFloor total remaining
    | remaining <= nudgeFloor = 3
    | 2 * remaining <= total = 2
    | otherwise = 1

tShow :: Int -> Text
tShow = T.pack . show
