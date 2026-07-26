{-# LANGUAGE OverloadedStrings #-}

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
import Siza.Agent.Owned (OwnedCell (..), newestFailing)

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

wrapUpMarker :: Text
wrapUpMarker = "Final turn:"

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

wrapUpFinal :: Text -> Map CellId OwnedCell -> Text -> Text
wrapUpFinal stopped owned candidate
    | not (T.null (T.strip candidate)) = candidate
    | otherwise = "Stopped (" <> stopped <> "): " <> stateLine owned

stateLine :: Map CellId OwnedCell -> Text
stateLine owned = case newestFailing owned of
    _ | Map.null owned -> "no cell was written before the episode ended."
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
