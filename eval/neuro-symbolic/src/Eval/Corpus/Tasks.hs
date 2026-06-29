{- | Phase-0.1 hard out-of-distribution corpus: task literals.

Each task pairs a fold tag with an 'Eval.Task.Task'. The HeldOut tasks force
discovery of a package outside the seed index (megaparsec, async, JuicyPixels,
fgl, conduit, text-metrics, split, extra, network-uri) whose deliverable is a
uniquely-named function the local resolver pins cleanly; the InIndex tasks are
composition controls solvable on base + a seed-index package (vector,
containers). See docs/neuro-symbolic-corpus.md for the durable manifest.
-}
module Eval.Corpus.Tasks (
    Fold (..),
    hardTasks,
) where

import Eval.Corpus.Capability (capabilityTasks)
import Eval.Task (Grader (..), Task (..))

{- | Which fold a task belongs to: 'InIndex' / 'HeldOut' tag the search-lever
folds (reference inside vs outside the seed index); 'Capability' is the
discovery fold whose lever is the @search_capability@ tool, not the server's
auto-resolver. See "Eval.Corpus.Capability".
-}
data Fold = InIndex | HeldOut | Capability
    deriving (Show, Eq)

{- | The fold-tagged corpus: the eleven Phase-0.1 search-lever tasks plus the
six capability-discovery tasks (five un-hand-rollable + one control). Graders
are the real constructors the grader strings in the spec denote.
-}
hardTasks :: [(Fold, Task)]
hardTasks =
    [ (InIndex, rollingAnomalyIndicesTask)
    , (HeldOut, evalExprTask)
    , (InIndex, topRegionsTask)
    , (HeldOut, concurrentWordTotalsTask)
    , (HeldOut, thumbInfoTask)
    , (HeldOut, shortestTask)
    , (HeldOut, orderTotalTask)
    , (HeldOut, kittenDistanceTask)
    , (HeldOut, slidingWindowsTask)
    , (HeldOut, disjointPairTask)
    , (HeldOut, validURIsTask)
    ]
        ++ [(Capability, t) | t <- capabilityTasks]

kittenDistanceTask :: Task
kittenDistanceTask =
    Task
        "kittenDistance"
        "Using the text-metrics library (module Data.Text.Metrics), compute the \
        \Levenshtein edit distance between the two words \"kitten\" and \
        \\"sitting\" with its `levenshtein` function. Note `levenshtein` takes \
        \Text arguments (levenshtein :: Text -> Text -> Int), so pack the String \
        \literals. Define the deliverable binding `kittenDistance :: Int` as that \
        \distance. The computation is pure (no IO)."
        (ByValue "kittenDistance == 3")

slidingWindowsTask :: Task
slidingWindowsTask =
    Task
        "slidingWindows"
        "Using the split library (module Data.List.Split), use its `divvy` \
        \function (divvy :: Int -> Int -> [a] -> [[a]]) to produce every \
        \contiguous length-3 sliding window of the inline list [1,2,3,4,5] :: \
        \[Int], where each window is offset by one from the previous \
        \(divvy 3 1). Define the deliverable binding `slidingWindows :: [[Int]]` \
        \as that list of windows. The computation is pure (no IO)."
        (ByValue "slidingWindows == [[1,2,3],[2,3,4],[3,4,5]]")

disjointPairTask :: Task
disjointPairTask =
    Task
        "disjointPair"
        "Using the extra library (module Data.List.Extra), use its `disjointOrd` \
        \function (disjointOrd :: Ord a => [a] -> [a] -> Bool, which decides \
        \whether two lists share no element) to test two inline pairs of Int \
        \lists: first [1,2,3] against [4,5], then [1,2,3] against [4,1]. Define \
        \the deliverable binding `disjointPair :: (Bool, Bool)` as the pair of \
        \those two results, in that order. The computation is pure (no IO)."
        (ByValue "disjointPair == (True, False)")

validURIsTask :: Task
validURIsTask =
    Task
        "validURIs"
        "Using the network-uri library (module Network.URI), use its `isURI` \
        \function (isURI :: String -> Bool, which tests whether a string is a \
        \valid absolute URI) to classify each string in this inline list, in \
        \order:\n\n\
        \    [\"http://example.com/p\", \"not a uri\", \"ftp://host/f\"]\n\n\
        \Define the deliverable binding `validURIs :: [Bool]` as the list of \
        \per-string results, preserving order. The computation is pure (no IO)."
        (ByValue "validURIs == [True, False, True]")

rollingAnomalyIndicesTask :: Task
rollingAnomalyIndicesTask =
    Task
        "rollingAnomalyIndices"
        "You are given this numeric time series as an inline literal:\n\n\
        \    series = [10, 11, 9, 10, 12, 11, 10, 40, 11, 9, 10, 12, 11, 50, 10, 9] :: [Double]\n\n\
        \Detect anomalies with a trailing rolling baseline. For each index i, let \
        \the baseline be the WINDOW = 5 points immediately PRECEDING i (i.e. \
        \indices i-5 .. i-1), so the baseline does not include the point itself. \
        \Compute the baseline's mean and its population standard deviation (divide \
        \by the window size N=5, not N-1). Flag index i as an anomaly when the \
        \baseline standard deviation is positive and the point deviates from the \
        \baseline mean by more than k = 3.0 standard deviations, that is \
        \abs (series!!i - mean) > 3.0 * std. Indices i < 5 (which have no full \
        \preceding window) are never flagged.\n\n\
        \Define `anomalyIndices :: [Int]` as the sorted list of flagged 0-based \
        \indices into `series`."
        (ByValue "anomalyIndices == [7, 13]")

evalExprTask :: Task
evalExprTask =
    Task
        "evalExpr"
        "Write a recursive-descent / operator-precedence parser for arithmetic \
        \expressions over the operators + - * / with parentheses and the usual \
        \precedence (* and / bind tighter than + and -, all left-associative). \
        \Use a parser-combinator library (megaparsec). Define \
        \`evalExpr :: String -> Maybe Double` which parses and evaluates the \
        \expression, returning `Just` the value on success and `Nothing` if the \
        \whole string is not a well-formed expression. Numbers are non-negative \
        \integer or decimal literals; arbitrary surrounding/internal whitespace \
        \is allowed. Examples that must hold: evalExpr \"2 + 3 * 4\" == Just 14.0, \
        \evalExpr \"(2 + 3) * 4\" == Just 20.0, evalExpr \"10 / 4 - 1\" == Just 1.5, \
        \evalExpr \"2 +\" == Nothing."
        ( ByValue
            "evalExpr \"2 + 3 * 4\" == Just 14.0 && evalExpr \"(2 + 3) * 4\" == Just 20.0 \
            \&& (case evalExpr \"10 / 4 - 1\" of { Just v -> abs (v - 1.5) < 1e-9; Nothing -> False }) \
            \&& evalExpr \"2 +\" == Nothing"
        )

topRegionsTask :: Task
topRegionsTask =
    Task
        "topRegions"
        "You are given two inline relations.\n\n\
        \customers :: [(String, String)]  -- (custId, region)\n\
        \customers =\n\
        \  [ (\"c1\",\"North\"), (\"c2\",\"South\"), (\"c3\",\"North\")\n\
        \  , (\"c4\",\"East\"),  (\"c5\",\"West\") ]\n\n\
        \orders :: [(String, Int)]  -- (custId, amount)\n\
        \orders =\n\
        \  [ (\"c1\",100), (\"c1\",50), (\"c2\",200), (\"c3\",100)\n\
        \  , (\"c4\",70),  (\"c4\",30), (\"c6\",999) ]\n\n\
        \Inner-join orders to customers on custId (orders whose custId is not in \
        \customers are dropped, and so is \"c6\"; customers with no orders \
        \contribute nothing), group the joined rows by region, and sum the order \
        \amounts per region. Then define\n\n\
        \  topRegions :: [(String, Int)]\n\n\
        \as the top 2 regions by total amount, in descending order of total. With \
        \the data above this is [(\"North\",250),(\"South\",200)] (North = \
        \100+50+100, South = 200, East = 70+30 = 100 which falls outside the top \
        \2). Use whatever libraries you find appropriate."
        (ByValue "topRegions == [(\"North\",250),(\"South\",200)]")

concurrentWordTotalsTask :: Task
concurrentWordTotalsTask =
    Task
        "concurrentWordTotals"
        "You are given three text chunks (inline below). Word-count each chunk \
        \CONCURRENTLY (process the three chunks in parallel, one task per chunk), \
        \then combine the per-chunk word-frequency maps into a single total. \
        \Words are whitespace-separated, case-sensitive, taken verbatim (no \
        \punctuation stripping needed — the inputs contain only lowercase letters \
        \and spaces).\n\n\
        \The three chunks:\n\
        \  chunkA = \"the quick brown fox the fox\"\n\
        \  chunkB = \"the lazy dog the dog the cat\"\n\
        \  chunkC = \"fox dog cat the the the\"\n\n\
        \Define the deliverable binding `wordTotals :: Data.Map.Strict.Map String Int` \
        \mapping each word to the TOTAL number of times it appears across all \
        \three chunks combined. The result must be deterministic even though the \
        \three chunks are counted concurrently. You will need a concurrency \
        \primitive that maps an IO action over a list of inputs and runs them in \
        \parallel while preserving a combinable result (look beyond base for the \
        \right package). Binding it at the top level via `wordTotals <- ...` in a \
        \cell is fine."
        ( ByValue
            "wordTotals == Data.Map.Strict.fromList \
            \[(\"brown\",1),(\"cat\",2),(\"dog\",3),(\"fox\",3),(\"lazy\",1),(\"quick\",1),(\"the\",8)]"
        )

thumbInfoTask :: Task
thumbInfoTask =
    Task
        "thumbInfo"
        "Using the JuicyPixels library, generate an 8x8 RGB image entirely in \
        \memory (no file I/O) where the pixel at column x, row y is \
        \`PixelRGB8 (fromIntegral (x * 16)) (fromIntegral (y * 16)) 128` (x and y \
        \range 0..7). Downscale it to a 4x4 thumbnail by averaging each \
        \non-overlapping 2x2 block of the source into one thumbnail pixel: each \
        \thumbnail channel is the integer mean (sum of the four source values \
        \divided by 4) of that channel over the 2x2 block. Define \
        \`thumbInfo :: ((Int, Int), PixelRGB8)` where the first component is the \
        \thumbnail's (width, height) and the second component is the thumbnail \
        \pixel at column 1, row 1."
        (ByValue "thumbInfo == ((4, 4), PixelRGB8 40 40 128)")

shortestTask :: Task
shortestTask =
    Task
        "shortest"
        "Using the fgl library, build this weighted directed graph and compute a \
        \single-source single-target shortest path. Nodes are the integers 1..6. \
        \The directed, weighted edges (from, to, weight) are exactly:\n\n\
        \  (1,2,7), (1,3,9), (1,6,14),\n\
        \  (2,3,10), (2,4,15),\n\
        \  (3,4,11), (3,6,2),\n\
        \  (4,5,6),\n\
        \  (6,5,9)\n\n\
        \Define `shortest :: Maybe Int` as the length of the shortest directed \
        \path from node 1 to node 5 (the sum of edge weights along the cheapest \
        \route), or Nothing if node 5 is unreachable from node 1."
        (ByValue "shortest == Just 20")

orderTotalTask :: Task
orderTotalTask =
    Task
        "orderTotal"
        "Using the conduit streaming library, build a pipeline that processes \
        \this inline list of orders, where each order is \
        \(productName, unitPrice, quantity):\n\n\
        \    orders :: [(String, Int, Int)]\n\
        \    orders =\n\
        \      [ (\"widget\", 250, 3)\n\
        \      , (\"gadget\", 999, 1)\n\
        \      , (\"widget\", 250, 0)\n\
        \      , (\"gizmo\", 1200, 2)\n\
        \      , (\"gadget\", 999, 4)\n\
        \      ]\n\n\
        \Stream `orders` through a conduit pipeline that: drops any order whose \
        \quantity is 0, computes each remaining order's line total \
        \(unitPrice * quantity), and sums those line totals. Define the \
        \deliverable binding `orderTotal :: Int` as that aggregate. The \
        \computation must be pure (no IO). Expected value: 250*3 + 999*1 + 1200*2 \
        \+ 999*4 = 750 + 999 + 2400 + 3996 = 8145."
        (ByValue "orderTotal == 8145")
