{-# LANGUAGE OverloadedStrings #-}

{- | Phase-0.2 search-lever gate: an A/B test that holds grammar fixed ON in both
arms and toggles only the search lever (the @SABELA_HOOGLE_RESOLVE@ env var on the
spawned server). Grades with the same ByValue/firstGreen markers as the bench and
reports a two-proportion z plus per-task and calls-to-green cost.
-}
module Eval.Gate (
    GateLever (..),
    runGate,
    runGateResuming,
    summariseGate,
    gateByTask,
    renderGate,
    searchEnv,
    module Eval.GateResult,
) where

import Control.Monad (forM, forM_, unless)
import Data.List (nub)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client.TLS (newTlsManager)
import System.Directory (createDirectoryIfMissing)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))

import Eval.Agent (
    AgentRun (..),
    Driver (..),
    GrammarMode (GrammarOn),
    runEpisodeWith',
 )
import Eval.Bench (
    ArmCost (..),
    ArmResult (..),
    BenchConfig (..),
    Comparison (..),
    RunStat (..),
    armCost,
    passRate,
    round1,
    tshow,
    twoProportionZ,
    withFreshServerEnv,
 )
import Eval.GateResult
import Eval.Ollama (chatSeeded)
import Eval.Task (Task, Verdict (Surfaced), grade, taskId)
import Eval.Tools (catalogue, dispatch)
import Eval.Transcript (renderTranscript)

{- | Which lever the A/B toggles. 'ResolverLever' flips the server's
@SABELA_HOOGLE_RESOLVE@ auto-resolver (the in-index / held-out folds);
'CapabilityLever' flips the gate-process @SABELA_CAPABILITY_SEARCH@ that gates
whether the @search_capability@ tool is in the model's catalogue (the
capability fold). The two are isolated: the capability lever never sets the
resolver, and keeps it OFF in both arms.
-}
data GateLever = ResolverLever | CapabilityLever
    deriving (Show, Eq)

{- | The env the ON arm sets on the spawned SERVER. Only the resolver lever
touches the server; the capability lever toggles the gate process instead, so
its server env is empty in both arms.
-}
searchEnv :: GateLever -> SearchMode -> [(String, String)]
searchEnv ResolverLever SearchOn = [("SABELA_HOOGLE_RESOLVE", "1")]
searchEnv _ _ = []

{- | Toggle the capability-search tool in the GATE process before its catalogue
is built. Episodes run sequentially, so process-env toggling is safe. A no-op
for the resolver lever, which never touches this var.
-}
setCapabilityEnv :: GateLever -> SearchMode -> IO ()
setCapabilityEnv CapabilityLever SearchOn = setEnv "SABELA_CAPABILITY_SEARCH" "1"
setCapabilityEnv CapabilityLever SearchOff = unsetEnv "SABELA_CAPABILITY_SEARCH"
setCapabilityEnv ResolverLever _ = pure ()

{- | Run the gate over @tasks@ x @seeds@ x both search modes, one fresh server per
run. Grammar is forced ON in every episode; @lever@ picks which knob the A/B flips.
-}
runGate ::
    BenchConfig -> GateLever -> [Task] -> [Int] -> IO [(Text, SearchMode, RunStat)]
runGate cfg lever tasks seeds =
    forM (zip [0 ..] (gateRuns tasks seeds)) $ \(i, (task, seed, mode)) -> do
        (st, _stopped) <-
            withFreshServerEnv
                cfg
                (bcBasePort cfg + i)
                (searchEnv lever mode)
                (\base -> runArmGate cfg lever base seed mode task)
        pure (taskId task, mode, st)

-- | The full (task, seed, mode) cross-product, both modes per (task, seed).
gateRuns :: [Task] -> [Int] -> [(Task, Int, SearchMode)]
gateRuns tasks seeds =
    [ (task, seed, mode)
    | task <- tasks
    , seed <- seeds
    , mode <- [SearchOff, SearchOn]
    ]

{- | Resumable gate: each completed episode is appended as one JSONL row to
@resultsFile@; a relaunch reads the file, skips episodes already recorded, and
continues. Returns the full accumulated set (the file re-read at the end), so the
report covers every relaunch's progress.
-}
runGateResuming ::
    BenchConfig -> GateLever -> FilePath -> [Task] -> [Int] -> IO [GateResult]
runGateResuming cfg lever resultsFile tasks seeds = do
    prior <- readGateResults resultsFile
    let done = Set.fromList (map gateKey prior)
        runs = gateRuns tasks seeds
        total = length runs
    forM_ (zip [0 ..] runs) $ \(i, (task, seed, mode)) ->
        unless (isDone done (taskId task) seed mode) $ do
            (st, stopped) <-
                withFreshServerEnv
                    cfg
                    (bcBasePort cfg + i)
                    (searchEnv lever mode)
                    (\base -> runArmGate cfg lever base seed mode task)
            let gr =
                    GateResult
                        (taskId task)
                        seed
                        mode
                        (rsPass st)
                        (rsTurns st)
                        (rsCalls st)
                        stopped
            appendGateResult resultsFile gr
            TIO.putStrLn (progressLine (i + 1) total task seed mode st)
    readGateResults resultsFile

progressLine :: Int -> Int -> Task -> Int -> SearchMode -> RunStat -> Text
progressLine k total task seed mode st =
    "["
        <> tshow k
        <> "/"
        <> tshow total
        <> "] "
        <> taskId task
        <> " s"
        <> tshow seed
        <> " "
        <> modeText mode
        <> " -> "
        <> (if rsPass st then "pass" else "fail")

{- | One gate run: always GrammarOn. The resolver lever was set on the server;
the capability lever is toggled HERE in the gate process before 'catalogue' is
read, so the ON arm's model sees @search_capability@ and the OFF arm's does not.

Returns the run's 'arStopped' alongside the 'RunStat' so the caller can record
the infra-vs-task signal. An episode that finishes at turn 0 is the chat-error
path ('arStopped' == "error"): a stale keep-alive socket in a reused 'Manager'
can kill the first POST and survive the in-loop retries. We therefore use a
FRESH 'Manager' per episode and retry the whole episode up to
'maxEpisodeRetries' times on a 0-turn run, logging the captured error each time.
-}
runArmGate ::
    BenchConfig ->
    GateLever ->
    Text ->
    Int ->
    SearchMode ->
    Task ->
    IO (RunStat, Text)
runArmGate cfg lever base seed mode task = do
    setCapabilityEnv lever mode
    cat <- catalogue
    let attempt = do
            mgr <- newTlsManager
            let driver =
                    Driver
                        { drvChat =
                            \msgs ->
                                chatSeeded
                                    False
                                    (Just seed)
                                    mgr
                                    (bcModel cfg)
                                    msgs
                                    cat
                        , drvDispatch = dispatch (bcConn cfg) base
                        , drvNow = realToFrac <$> getPOSIXTime
                        , drvVerify =
                            (== Surfaced) . fst <$> grade (bcConn cfg) base task
                        }
            runEpisodeWith' GrammarOn (bcBudget cfg) driver task (bcMaxTurns cfg)
        retryLoop n run
            | arTurns run >= 1 = pure run
            | n >= maxEpisodeRetries = pure run
            | otherwise = do
                putStrLn $
                    "[gate] "
                        <> T.unpack (taskId task)
                        <> " s"
                        <> show seed
                        <> " "
                        <> T.unpack (modeText mode)
                        <> ": 0-turn infra failure (retry "
                        <> show (n + 1)
                        <> "/"
                        <> show maxEpisodeRetries
                        <> "): "
                        <> T.unpack (arFinal run)
                attempt >>= retryLoop (n + 1)
    run <- attempt >>= retryLoop 0
    saveGateTranscript cfg task seed mode run
    (v, _) <- grade (bcConn cfg) base task
    pure (RunStat (v == Surfaced) (arTurns run) (arToolCalls run), arStopped run)

-- | How many times a 0-turn (infra/chat-error) episode is re-run from scratch.
maxEpisodeRetries :: Int
maxEpisodeRetries = 2

-- | Write a run's transcript to @<dir>/<task>-s<seed>-<mode>.md@; @""@ disables.
saveGateTranscript ::
    BenchConfig -> Task -> Int -> SearchMode -> AgentRun -> IO ()
saveGateTranscript cfg task seed mode run = case bcTranscriptDir cfg of
    "" -> pure ()
    dir -> do
        createDirectoryIfMissing True dir
        let name =
                T.unpack (taskId task)
                    <> "-s"
                    <> show seed
                    <> "-"
                    <> T.unpack (modeText mode)
                    <> ".md"
        TIO.writeFile (dir </> name) (renderTranscript (taskId task) (arTranscript run))

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
        [ "Arm A (SearchOff): " <> rate a
        , "Arm B (SearchOn):  " <> rate b
        , "B - A: " <> tshow (round3 diff) <> "   z = " <> tshow (round3 z)
        , "significant at 5%: " <> tshow (abs z > 1.96)
        ]
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

round3 :: Double -> Double
round3 x = fromIntegral (round (x * 1000) :: Int) / 1000
