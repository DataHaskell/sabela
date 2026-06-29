-- | siza-bench: Runs an unseen-package task set
module Main (main) where

import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Client.TLS (newTlsManager)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Eval.Agent (defaultBudget)
import Eval.Bench (BenchConfig (..), renderReportFull, runBench)
import qualified Eval.Corpus as Corpus
import qualified Eval.Corpus.Reasoning as Reasoning
import Eval.Task (Grader (..), Task (..))
import Siza.Transport (newConn)

benchTasks :: [Task]
benchTasks =
    [ Task
        "jsonSum"
        "Parse the JSON array text \"[10, 20, 30]\" into a list of integers and \
        \define `jsonSum :: Int` as the sum of its elements."
        (ByValue "jsonSum == 60")
    , Task
        "dateDays"
        "Define `dateDays :: Integer` as the number of days from 2020-01-01 to \
        \2020-04-10 (the later date minus the earlier one)."
        (ByValue "dateDays == 100")
    , Task
        "barChart"
        "Using the granite library, plot the bars [(\"Q1\", 12), (\"Q2\", 18), \
        \(\"Q3\", 9)] as a bar chart and show the chart in the notebook."
        ByRender
    , Task
        "revenueTotal"
        "A CSV `revenue.csv` with columns `month` and `revenue` is in the working \
        \directory. Using the dataframe library, load it and define \
        \`revenueTotal :: Double` as the total revenue across all months."
        (ByValue "abs (revenueTotal - 600) < 0.001")
    , Task
        "revenueChart"
        "A CSV `revenue.csv` with columns `month` and `revenue` is in the working \
        \directory. Using the dataframe and granite libraries, load it and plot \
        \revenue by month as a bar chart titled \"Monthly Revenue\", and show the \
        \chart in the notebook."
        ByRender
    , Task
        "topMonth"
        "A CSV `revenue.csv` with columns `month` and `revenue` is in the working \
        \directory. Using the dataframe library, load it and define \
        \`topMonth :: String` as the month with the highest revenue."
        (ByValue "topMonth == \"Mar\"")
    , Task
        "topMonthOut"
        "A CSV `revenue.csv` with columns `month` and `revenue` is in the working \
        \directory. Using the dataframe library, load it and print the month with \
        \the highest revenue in the notebook."
        ByOutput
    , Task
        "loadInspect"
        "A CSV `revenue.csv` with columns `month` and `revenue` is in the working \
        \directory. Using the dataframe library, first load it and show the \
        \DataFrame in one cell. Then in a second cell, print the total revenue \
        \across all months."
        (BySteps [ByOutputHas ["Jan", "Feb", "Mar"], ByOutputHas ["600"]])
    , Task
        "revenuePipeline"
        "A CSV `revenue.csv` with columns `month` and `revenue` is in the working \
        \directory. Using the dataframe library, first load it and show the \
        \DataFrame in one cell. Then in a second cell, plot revenue by month as a \
        \bar chart with the granite library and show the chart."
        (BySteps [ByOutputHas ["Jan", "Feb", "Mar"], ByRender])
    , Task
        "nycTaxiStats"
        "Download a sample of the NYC yellow taxi trip dataset (a CSV from a public \
        \URL) and, using the dataframe library, show descriptive statistics (count, \
        \mean, min, max) for the trip distance and fare amount columns."
        Untested
    , Task
        "symbolicRegression"
        "Implement a simple symbolic regression in Haskell: given the points \
        \[(1.0, 1.0), (2.0, 4.0), (3.0, 9.0), (4.0, 16.0)], search over simple \
        \arithmetic expressions built from x, +, *, and numeric constants to find one \
        \whose predictions fit the points, and print the best expression and its \
        \total squared error."
        Untested
    , Task
        "movieLensClassify"
        "Download the MovieLens 100k dataset and train a simple model to predict \
        \whether a user rates a movie above 3 stars; report the classification \
        \accuracy on a held-out split."
        Untested
    ]

main :: IO ()
main = do
    model <- T.pack . fromMaybe "gpt-oss:20b" <$> lookupEnv "SIZA_EVAL_MODEL"
    seeds <- parseSeeds <$> lookupEnv "SIZA_BENCH_SEEDS"
    pool <-
        corpusPool <$> lookupEnv "SIZA_BENCH_CORPUS" <*> lookupEnv "SIZA_BENCH_FOLD"
    tasks <- selectTasks pool <$> lookupEnv "SIZA_BENCH_TASKS"
    bin <- fromMaybe defaultBin <$> lookupEnv "SABELA_BIN"
    transcripts <-
        fromMaybe "/tmp/siza-bench-transcripts" <$> lookupEnv "SIZA_BENCH_TRANSCRIPTS"
    mgr <- newTlsManager
    conn <- newConn
    let cfg = BenchConfig mgr conn model defaultBudget 12 bin 3100 transcripts
    TIO.putStrLn
        ( "siza-bench \183 "
            <> model
            <> " \183 seeds "
            <> T.pack (show seeds)
            <> " \183 "
            <> T.pack (show (length tasks))
            <> " tasks \183 fresh server per run from port 3100"
        )
    outcomes <- runBench cfg tasks seeds
    TIO.putStr (renderReportFull outcomes)

-- | The Sabela server binary each run spawns; override with @SABELA_BIN@.
defaultBin :: FilePath
defaultBin =
    "dist-newstyle/build/aarch64-osx/ghc-9.12.2/sabela-0.1.0.0/x/sabela/build/sabela/sabela"

-- | Seeds from @SIZA_BENCH_SEEDS@ (comma list), defaulting to three.
parseSeeds :: Maybe String -> [Int]
parseSeeds = maybe dflt (orDefault dflt . mapMaybe (readMaybe . trim) . splitComma)
  where
    dflt = [1, 2, 3]

{- | Choose the task pool: @SIZA_BENCH_CORPUS=hard@ swaps in the Phase-0.1 hard
corpus (filtered by @SIZA_BENCH_FOLD=in-index|held-out|all@ via 'Corpus.selectFold');
@SIZA_BENCH_CORPUS=reasoning@ swaps in the reasoning corpus (filtered by
@SIZA_BENCH_FOLD@ with a category name via 'Reasoning.selectReasoning');
anything else keeps the original 'benchTasks'.
-}
corpusPool :: Maybe String -> Maybe String -> [Task]
corpusPool (Just "hard") fold = Corpus.selectFold (T.pack . trim <$> fold)
corpusPool (Just "reasoning") fold =
    Reasoning.selectReasoning (T.pack . trim <$> fold)
corpusPool _ _ = benchTasks

{- | Tasks selected by id from @SIZA_BENCH_TASKS@ (comma list), defaulting to the
whole pool.
-}
selectTasks :: [Task] -> Maybe String -> [Task]
selectTasks pool Nothing = pool
selectTasks pool (Just s) =
    orDefault pool (filter ((`elem` want) . T.unpack . taskId) pool)
  where
    want = map trim (splitComma s)

orDefault :: [a] -> [a] -> [a]
orDefault d [] = d
orDefault _ xs = xs

splitComma :: String -> [String]
splitComma s = case break (== ',') s of
    (a, ',' : rest) -> a : splitComma rest
    (a, _) -> [a]

trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ')
