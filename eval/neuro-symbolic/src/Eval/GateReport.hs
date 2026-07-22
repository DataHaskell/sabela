{-# LANGUAGE OverloadedStrings #-}

{- | Gate report rendering, split from "Eval.Gate" (module size cap): the
A/B summary over 'SearchMode', per-task splits, and the calls-to-green cost
table. "Eval.Gate" re-exports everything here.
-}
module Eval.GateReport (
    summariseGate,
    gateByTask,
    renderGate,
) where

import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

import Eval.Bench (
    ArmCost (..),
    ArmResult (..),
    Comparison (..),
    RunStat (..),
    armCost,
    noiseCaveat,
    passRate,
    round1,
    round3,
    tshow,
    twoProportionZ,
 )
import Eval.GateResult (SearchMode (..))

-- | Aggregate (mode, passed) outcomes; SearchOff is arm A, SearchOn arm B.
summariseGate :: [(SearchMode, Bool)] -> Comparison
summariseGate outcomes =
    Comparison a b (passRate b - passRate a) (twoProportionZ a b)
  where
    a = tally SearchOff
    b = tally SearchOn
    tally mode =
        ArmResult
            (length [() | (m, ok) <- outcomes, m == mode, ok])
            (length [() | (m, _) <- outcomes, m == mode])

gateByTask :: [(Text, SearchMode, Bool)] -> [(Text, Comparison)]
gateByTask outcomes =
    [ (tid, summariseGate [(m, ok) | (t, m, ok) <- outcomes, t == tid])
    | tid <- nub [t | (t, _, _) <- outcomes]
    ]

-- | The full gate report: per-task pass split, overall z, and calls-to-green cost.
renderGate :: [(Text, SearchMode, RunStat)] -> Text
renderGate outcomes =
    T.unlines
        ("Per task (A=SearchOff, B=SearchOn):" : map taskRow (gateByTask passes))
        <> "\nOverall:\n"
        <> renderComparison (summariseGate [(m, ok) | (_, m, ok) <- passes])
        <> "\n"
        <> renderCost outcomes
  where
    passes = [(t, m, rsPass s) | (t, m, s) <- outcomes]
    taskRow (tid, Comparison a b _ _) =
        "  " <> tid <> ":  A " <> rate a <> "   B " <> rate b
    rate r = tshow (arPasses r) <> "/" <> tshow (arRuns r)

renderComparison :: Comparison -> Text
renderComparison (Comparison a b diff z) =
    T.unlines
        ( [ "Arm A (SearchOff): " <> rate a
          , "Arm B (SearchOn):  " <> rate b
          , "B - A: " <> tshow (round3 diff) <> "   z = " <> tshow (round3 z)
          , "significant at 5%: " <> tshow (abs z > 1.96)
          ]
            <> noiseCaveat z (arRuns a)
        )
  where
    rate r = tshow (arPasses r) <> "/" <> tshow (arRuns r)

renderCost :: [(Text, SearchMode, RunStat)] -> Text
renderCost outcomes =
    T.unlines
        ( "Cost to pass (mean tool calls / turns over passing runs, A=Off B=On):"
            : map row (costByTask outcomes)
        )
        <> "Overall:  A "
        <> cell (armCost (statsFor SearchOff))
        <> "   B "
        <> cell (armCost (statsFor SearchOn))
        <> "\n"
  where
    statsFor mode = [s | (_, m, s) <- outcomes, m == mode]
    row (tid, (a, b)) = "  " <> tid <> ":  A " <> cell a <> "   B " <> cell b
    cell (ArmCost n c t) =
        tshow (round1 c) <> "c/" <> tshow (round1 t) <> "t (" <> tshow n <> ")"

costByTask :: [(Text, SearchMode, RunStat)] -> [(Text, (ArmCost, ArmCost))]
costByTask outcomes =
    [ (tid, (statsFor tid SearchOff, statsFor tid SearchOn))
    | tid <- nub [t | (t, _, _) <- outcomes]
    ]
  where
    statsFor tid mode = armCost [s | (t, m, s) <- outcomes, t == tid, m == mode]
