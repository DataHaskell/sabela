{- | Reasoning corpus — programming-competition tasks.

Answer-QUALITY tasks: each has a single deterministic answer over inline input
and a pure 'ByValue' check pinned to a LIVE-VALIDATED value. These probe the
model's reasoning, not library discovery — whether it hand-rolls or reaches for a
package is irrelevant, so no task names a library and none renders. Deliverables
are top-level bindings (notebooks have no @main@). See the validated values in
the haddock above each task.
-}
module Eval.Corpus.Reasoning.Competition (
    competitionTasks,
) where

import Eval.Task (Grader (..), Task (..))

-- | The competition reasoning tasks, in catalogue order.
competitionTasks :: [Task]
competitionTasks =
    [ maxSubarrayTask
    , countPrimesTask
    , lisLengthTask
    , coinChangeTask
    , nQueensTask
    , gridPathsTask
    ]

{- | Kadane's maximum-subarray sum. Validated: the answer for
[-2,1,-3,4,-1,2,1,-5,4] is 6 (the run [4,-1,2,1]) and for the all-negative
[-3,-1,-2] is -1 (the largest single element).
-}
maxSubarrayTask :: Task
maxSubarrayTask =
    Task
        "maxSubarray"
        "Define `maxSubarray :: [Int] -> Int` as the maximum sum over all \
        \NON-EMPTY contiguous sub-arrays of its argument (Kadane's algorithm; \
        \with an all-negative input the answer is the largest single element). \
        \For example maxSubarray [-2,1,-3,4,-1,2,1,-5,4] == 6 (the run \
        \[4,-1,2,1]) and maxSubarray [-3,-1,-2] == -1. The computation is pure \
        \(no IO)."
        ( ByValue
            "maxSubarray [-2,1,-3,4,-1,2,1,-5,4] == 6 \
            \&& maxSubarray [-3,-1,-2] == (-1) \
            \&& maxSubarray [5] == 5"
        )

{- | Count of primes strictly below n. Validated: there are 25 primes below 100
and 4 primes below 10 (2,3,5,7).
-}
countPrimesTask :: Task
countPrimesTask =
    Task
        "countPrimes"
        "Define `countPrimes :: Int -> Int` as the number of prime numbers \
        \STRICTLY LESS than its argument (so countPrimes 10 counts 2,3,5,7 = 4, \
        \and countPrimes 2 == 0). For example countPrimes 100 == 25. The \
        \computation is pure (no IO)."
        ( ByValue
            "countPrimes 100 == 25 && countPrimes 10 == 4 \
            \&& countPrimes 2 == 0 && countPrimes 0 == 0"
        )

{- | Longest strictly-increasing-subsequence length. Validated:
[10,9,2,5,3,7,101,18] -> 4, [0,1,0,3,2,3] -> 4, repeated elements -> 1, [] -> 0.
-}
lisLengthTask :: Task
lisLengthTask =
    Task
        "lisLength"
        "Define `lisLength :: [Int] -> Int` as the length of the longest \
        \STRICTLY increasing subsequence of its argument (elements need not be \
        \contiguous; equal elements do not count as increasing). For example \
        \lisLength [10,9,2,5,3,7,101,18] == 4 and lisLength [0,1,0,3,2,3] == 4. \
        \The computation is pure (no IO)."
        ( ByValue
            "lisLength [10,9,2,5,3,7,101,18] == 4 \
            \&& lisLength [0,1,0,3,2,3] == 4 \
            \&& lisLength [7,7,7] == 1 && lisLength [] == 0"
        )

{- | Minimal-coin change over unlimited denominations. Validated:
minCoins [1,2,5] 11 == Just 3 (5+5+1), minCoins [2] 3 == Nothing,
minCoins _ 0 == Just 0.
-}
coinChangeTask :: Task
coinChangeTask =
    Task
        "minCoins"
        "Define `minCoins :: [Int] -> Int -> Maybe Int` returning the FEWEST \
        \coins (drawn from the given unlimited denominations) that sum exactly \
        \to the target, or Nothing if the target cannot be made; `minCoins _ 0` \
        \is Just 0. For example minCoins [1,2,5] 11 == Just 3 and \
        \minCoins [2] 3 == Nothing. The computation is pure (no IO)."
        ( ByValue
            "minCoins [1,2,5] 11 == Just 3 && minCoins [2] 3 == Nothing \
            \&& minCoins [1,2,5] 0 == Just 0 && minCoins [1,5,10,25] 30 == Just 2"
        )

{- | N-queens solution count. Validated: queens 6 == 4, queens 8 == 92,
queens 1 == 1, queens 4 == 2 (the classic counts).
-}
nQueensTask :: Task
nQueensTask =
    Task
        "queens"
        "Define `queens :: Int -> Int` as the number of distinct solutions to \
        \the n-queens problem: ways to place n mutually non-attacking queens on \
        \an n×n board (no two share a row, column, or diagonal). For example \
        \queens 6 == 4 and queens 8 == 92. The computation is pure (no IO)."
        ( ByValue
            "queens 6 == 4 && queens 8 == 92 && queens 1 == 1 && queens 4 == 2"
        )

{- | Lattice-path DP: monotone right/down paths across an m×n grid of cells (the
binomial coefficient). Validated: gridPaths 3 3 == 20, gridPaths 2 3 == 10,
gridPaths 0 5 == 1.
-}
gridPathsTask :: Task
gridPathsTask =
    Task
        "gridPaths"
        "Define `gridPaths :: Int -> Int -> Integer` as the number of distinct \
        \monotone lattice paths from the top-left corner to the bottom-right \
        \corner of an m-by-n grid of CELLS, moving only one cell right or one \
        \cell down at each step (so gridPaths m n counts paths over an (m+1) by \
        \(n+1) grid of corners). For example gridPaths 3 3 == 20 and \
        \gridPaths 2 3 == 10, and either dimension 0 gives exactly one path. The \
        \computation is pure (no IO)."
        ( ByValue
            "gridPaths 3 3 == 20 && gridPaths 2 3 == 10 \
            \&& gridPaths 0 5 == 1 && gridPaths 1 1 == 2"
        )
