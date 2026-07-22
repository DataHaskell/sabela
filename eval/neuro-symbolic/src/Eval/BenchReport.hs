{-# LANGUAGE OverloadedStrings #-}

{- | Bench statistics and report rendering, split from "Eval.Bench" (module
size cap): arm tallies, the two-proportion z, per-task splits, and the
cost-to-pass table. "Eval.Bench" re-exports everything here.
-}
module Eval.BenchReport (
    ArmResult (..),
    Comparison (..),
    RunStat (..),
    ArmCost (..),
    passRate,
    twoProportionZ,
    summariseRuns,
    renderComparison,
    noiseCaveat,
    byTask,
    renderReport,
    armCost,
    costByTask,
    renderReportFull,
    renderReportFlagged,
    round1,
    round3,
    tshow,
) where

import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

import Eval.Agent (GrammarMode (..))
import Eval.Applicability (excludeFlagged)

-- | Passes out of runs for one arm.
data ArmResult = ArmResult
    { arPasses :: Int
    , arRuns :: Int
    }
    deriving (Eq, Show)

data Comparison = Comparison
    { cmpArmA :: ArmResult
    , cmpArmB :: ArmResult
    , cmpDiff :: Double
    , cmpZ :: Double
    }
    deriving (Eq, Show)

passRate :: ArmResult -> Double
passRate (ArmResult p n)
    | n == 0 = 0
    | otherwise = fromIntegral p / fromIntegral n

{- | The two-proportion z statistic for arm B over arm A under the pooled-variance
null. Zero when either arm has no runs or the pooled rate is degenerate, so a
caller never divides by zero. @|z| > 1.96@ is the usual 5% two-sided threshold.
-}
twoProportionZ :: ArmResult -> ArmResult -> Double
twoProportionZ a@(ArmResult xa na) b@(ArmResult xb nb)
    | na == 0 || nb == 0 || se == 0 = 0
    | otherwise = (passRate b - passRate a) / se
  where
    p = fromIntegral (xa + xb) / fromIntegral (na + nb)
    se = sqrt (p * (1 - p) * (1 / fromIntegral na + 1 / fromIntegral nb))

-- | Aggregate (arm, passed) outcomes into the A-vs-B comparison.
summariseRuns :: [(GrammarMode, Bool)] -> Comparison
summariseRuns outcomes =
    Comparison a b (passRate b - passRate a) (twoProportionZ a b)
  where
    a = tally GrammarOff
    b = tally GrammarOn
    tally mode =
        ArmResult
            (length [() | (m, ok) <- outcomes, m == mode, ok])
            (length [() | (m, _) <- outcomes, m == mode])

-- | A one-block report of a comparison, with the 5% significance verdict.
renderComparison :: Comparison -> Text
renderComparison (Comparison a b diff z) =
    T.unlines
        ( [ "Arm A (GrammarOff): " <> rate a
          , "Arm B (GrammarOn):  " <> rate b
          , "B - A: " <> tshow (round3 diff) <> "   z = " <> tshow (round3 z)
          , "significant at 5%: " <> tshow (abs z > 1.96)
          ]
            <> noiseCaveat z (arRuns a)
        )
  where
    rate r = tshow (arPasses r) <> "/" <> tshow (arRuns r)

-- | The line that stops a small-n coin flip being read as a lever effect.
noiseCaveat :: Double -> Int -> [Text]
noiseCaveat z n
    | abs z > 1.96 = []
    | otherwise =
        [ "  (|z| <= 1.96 at n = "
            <> tshow n
            <> " per arm: treat the delta as NOISE — do not steer on it)"
        ]

data RunStat = RunStat
    { rsPass :: Bool
    , rsTurns :: Int
    , rsCalls :: Int
    }
    deriving (Eq, Show)

data ArmCost = ArmCost
    { acPassN :: Int
    , acMeanCalls :: Double
    , acMeanTurns :: Double
    }
    deriving (Eq, Show)

armCost :: [RunStat] -> ArmCost
armCost rs = ArmCost n (mean rsCalls) (mean rsTurns)
  where
    passed = filter rsPass rs
    n = length passed
    mean f
        | n == 0 = 0
        | otherwise = fromIntegral (sum (map f passed)) / fromIntegral n

costByTask :: [(Text, GrammarMode, RunStat)] -> [(Text, (ArmCost, ArmCost))]
costByTask outcomes =
    [ (tid, (statsFor tid GrammarOff, statsFor tid GrammarOn))
    | tid <- nub [t | (t, _, _) <- outcomes]
    ]
  where
    statsFor tid mode = armCost [s | (t, m, s) <- outcomes, t == tid, m == mode]

renderCost :: [(Text, GrammarMode, RunStat)] -> Text
renderCost outcomes =
    T.unlines
        ( "Cost to pass (mean tool calls / turns over passing runs, A=Off B=On):"
            : map row (costByTask outcomes)
        )
        <> "Overall:  A "
        <> cell (armCost (statsFor GrammarOff))
        <> "   B "
        <> cell (armCost (statsFor GrammarOn))
        <> "\n"
  where
    statsFor mode = [s | (_, m, s) <- outcomes, m == mode]
    row (tid, (a, b)) = "  " <> tid <> ":  A " <> cell a <> "   B " <> cell b
    cell (ArmCost n c t) =
        tshow (round1 c) <> "c/" <> tshow (round1 t) <> "t (" <> tshow n <> ")"

renderReportFull :: [(Text, GrammarMode, RunStat)] -> Text
renderReportFull outcomes =
    renderReport [(t, m, rsPass s) | (t, m, s) <- outcomes]
        <> "\n"
        <> renderCost outcomes

{- | The full report with flagged pairs handled per R8.2: a VOID pair appears
in NO row (it measured nothing); NA and lever-saturated pairs print BOTH
per-arm outcomes in the Per task table as category rows, excluded from every
lever number (overall comparison and cost) — a grade is not a measurement.
-}
renderReportFlagged ::
    [(Text, Int)] ->
    [(Text, Int)] ->
    [(Text, Int)] ->
    [(Text, Int, GrammarMode, RunStat)] ->
    Text
renderReportFlagged voids nas sats rows =
    T.unlines
        ( "Per task (A=GrammarOff, B=GrammarOn):"
            : map measuredRow (byTask measuredBool)
                <> map (categoryRow naLabel) nas
                <> map (categoryRow satLabel) sats
        )
        <> "\nOverall:\n"
        <> renderComparison (summariseRuns [(m, ok) | (_, m, ok) <- measuredBool])
        <> "\n"
        <> renderCost measured
  where
    measured = excludeFlagged (voids <> nas <> sats) rows
    measuredBool = [(t, m, rsPass s) | (t, m, s) <- measured]
    measuredRow (tid, Comparison a b _ _) =
        "  " <> tid <> ":  A " <> rate a <> "   B " <> rate b
    rate r = tshow (arPasses r) <> "/" <> tshow (arRuns r)
    naLabel = "[not applicable — search-free pair; excluded from lever deltas]"
    satLabel =
        "[lever-saturated — lever fired, arms identical; excluded from lever deltas]"
    categoryRow label (task, seed) =
        "  "
            <> task
            <> " s"
            <> tshow seed
            <> ":  A "
            <> passFail task seed GrammarOff
            <> "   B "
            <> passFail task seed GrammarOn
            <> "   "
            <> label
    passFail task seed mode =
        case [rsPass st | (t, s, m, st) <- rows, t == task, s == seed, m == mode] of
            (ok : _) -> if ok then "pass" else "fail"
            [] -> "unrecorded"

byTask :: [(Text, GrammarMode, Bool)] -> [(Text, Comparison)]
byTask outcomes =
    [ (tid, summariseRuns [(m, ok) | (t, m, ok) <- outcomes, t == tid])
    | tid <- nub [t | (t, _, _) <- outcomes]
    ]

renderReport :: [(Text, GrammarMode, Bool)] -> Text
renderReport outcomes =
    T.unlines ("Per task (A=GrammarOff, B=GrammarOn):" : map row (byTask outcomes))
        <> "\nOverall:\n"
        <> renderComparison (summariseRuns [(m, ok) | (_, m, ok) <- outcomes])
  where
    row (tid, Comparison a b _ _) =
        "  " <> tid <> ":  A " <> rate a <> "   B " <> rate b
    rate r = tshow (arPasses r) <> "/" <> tshow (arRuns r)

round3 :: Double -> Double
round3 x = fromIntegral (round (x * 1000) :: Int) / 1000

round1 :: Double -> Double
round1 x = fromIntegral (round (x * 10) :: Int) / 10

tshow :: (Show a) => a -> Text
tshow = T.pack . show
