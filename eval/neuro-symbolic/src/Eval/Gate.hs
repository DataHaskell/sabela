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
    armOrder,
    capabilityEnvFor,
    module Eval.GateReport,
    module Eval.GateResult,
) where

import Control.Monad (forM, forM_, unless, when)
import Data.List (nub)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client.TLS (newTlsManager)
import System.Environment (setEnv, unsetEnv)

import Eval.Agent (
    AgentRun (..),
    Driver (..),
    GrammarMode (GrammarOn),
    runEpisodeWith',
 )
import Eval.Bench (
    BenchConfig (..),
    RunStat (..),
    tshow,
    withFreshServerEnv,
 )
import Eval.Episode (EpisodeMeta (..), retryFreshSeed, saveEpisodeIn)
import Eval.GateReport
import Eval.GateResult
import Eval.Ollama (chatSeeded)
import Eval.Provenance (RunProvenance (..), nowIso)
import Eval.Task (
    Task,
    Verdict (Surfaced),
    grade,
    gradeVerify,
    taskId,
    taskPrompt,
 )
import Eval.Tools (catalogue, dispatch)
import Eval.TranscriptLint (lintLine, lintMessages, stopIssues)
import Siza.Agent.Transcript (contextChars)

{- | Which lever the A/B toggles. 'ResolverLever' flips the server's
@SABELA_HOOGLE_RESOLVE@ auto-resolver; 'CapabilityLever' flips the gate-process
@SABELA_CAPABILITY_SEARCH@ that gates SHIP semantic ENRICHMENT inside the
@discover@ tool (the lexical channel and catalogue are constant).
'ServerFlagLever' toggles a named default-ON server flag (the self-healing
features), so its OFF arm sets the var to @0@ rather than leaving it unset.
-}
data GateLever = ResolverLever | CapabilityLever | ServerFlagLever String
    deriving (Eq, Show)

{- | The env the arm sets on the spawned SERVER. The resolver lever's ON arm
sets its var; a 'ServerFlagLever' pins its var in both arms (the flags default
ON, so OFF must be explicit); the capability lever touches the gate process only.
-}
searchEnv :: GateLever -> SearchMode -> [(String, String)]
searchEnv ResolverLever SearchOn = [("SABELA_HOOGLE_RESOLVE", "1")]
-- The flag defaults ON when unset, so the OFF arm must pin it to 0.
searchEnv ResolverLever SearchOff = [("SABELA_HOOGLE_RESOLVE", "0")]
searchEnv (ServerFlagLever var) SearchOn = [(var, "1")]
searchEnv (ServerFlagLever var) SearchOff = [(var, "0")]
searchEnv _ _ = []

{- | Toggle the capability backend in the GATE process; @discover@'s routing
reads it at dispatch time. Episodes run sequentially, so process-env toggling
is safe.
-}
setCapabilityEnv :: GateLever -> SearchMode -> IO ()
setCapabilityEnv lever mode = case capabilityEnvFor lever mode of
    Just v -> setEnv "SABELA_CAPABILITY_SEARCH" v
    Nothing -> unsetEnv "SABELA_CAPABILITY_SEARCH"

{- | The capability backend per lever and arm. Only 'CapabilityLever' varies it
between arms; every other gate keeps it at the production baseline (ON) in both
arms — a knob that is not the lever must not differ from production.
-}
capabilityEnvFor :: GateLever -> SearchMode -> Maybe String
capabilityEnvFor CapabilityLever SearchOn = Just "1"
capabilityEnvFor CapabilityLever SearchOff = Nothing
capabilityEnvFor _ _ = Just "1"

{- | Run the gate over @tasks@ x @seeds@ x both search modes, one fresh server per
run. Grammar is forced ON in every episode; @lever@ picks which knob the A/B flips.
-}
runGate ::
    BenchConfig -> GateLever -> [Task] -> [Int] -> IO [(Text, SearchMode, RunStat)]
runGate cfg lever tasks seeds =
    forM (zip [0 ..] (gateRuns tasks seeds)) $ \(i, (task, seed, mode)) -> do
        (st, _stopped, _ctx) <-
            withFreshServerEnv
                cfg
                (bcBasePort cfg + i)
                (searchEnv lever mode)
                (\base -> runArmGate cfg lever base seed mode task)
        pure (taskId task, mode, st)

{- | The full (task, seed, mode) cross-product, both modes per (task, seed),
first arm alternating per pair ('armOrder'). Resume-safe: episodes are keyed by
(task, seed, mode), not position.
-}
gateRuns :: [Task] -> [Int] -> [(Task, Int, SearchMode)]
gateRuns tasks seeds =
    concat
        [ [(task, seed, mode) | mode <- armOrder i]
        | (i, (task, seed)) <-
            zip [0 ..] [(t, s) | t <- tasks, s <- seeds]
        ]

{- | Which arm runs first for the i-th (task, seed) pair: alternating, so
dep-heavy tasks do not always pay the cold cabal install in the same arm.
-}
armOrder :: Int -> [SearchMode]
armOrder i
    | even (i :: Int) = [SearchOff, SearchOn]
    | otherwise = [SearchOn, SearchOff]

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
            (st, stopped, ctx) <-
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
                        ctx
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
the capability lever is toggled HERE in the gate process; both arms see the
same @discover@ tool, but only the ON arm's routing reaches the capability index.

Returns the run's 'arStopped' alongside the 'RunStat' so the caller can record
the infra-vs-task signal. A 0-turn episode is the chat-error path ('arStopped'
== "error"): it is retried from scratch with a FRESH 'Manager' and a FRESH seed
each time (R6.11 — a bit-identical seeded replay can never succeed), and every
seed tried is recorded in the transcript's episode-config header.
-}
runArmGate ::
    BenchConfig ->
    GateLever ->
    Text ->
    Int ->
    SearchMode ->
    Task ->
    IO (RunStat, Text, Int)
runArmGate cfg lever base seed mode task = do
    setCapabilityEnv lever mode
    cat <- catalogue
    let attempt s = do
            when (s /= seed) $
                putStrLn $
                    "[gate] "
                        <> T.unpack (taskId task)
                        <> " s"
                        <> show seed
                        <> " "
                        <> T.unpack (modeText mode)
                        <> ": 0-turn infra failure — retrying with fresh seed "
                        <> show s
            mgr <- newTlsManager
            let driver =
                    Driver
                        { drvChat =
                            \msgs ->
                                chatSeeded False (Just s) mgr (bcModel cfg) msgs cat
                        , drvDispatch = dispatch (bcConn cfg) base
                        , drvNow = realToFrac <$> getPOSIXTime
                        , drvVerify = gradeVerify (bcConn cfg) base task
                        }
            runEpisodeWith'
                GrammarOn
                (bcBudget cfg)
                driver
                (taskPrompt task)
                (bcMaxTurns cfg)
    (run, seedsTried) <-
        retryFreshSeed maxEpisodeRetries seed ((>= 1) . arTurns) attempt
    saveGateEpisode cfg lever base task seed seedsTried mode run
    (v, _) <- grade (bcConn cfg) base task
    pure
        ( RunStat (v == Surfaced) (arTurns run) (arToolCalls run)
        , arStopped run
        , contextChars (arTranscript run)
        )

-- | How many times a 0-turn (infra/chat-error) episode is re-run from scratch.
maxEpisodeRetries :: Int
maxEpisodeRetries = 2

{- | Persist the episode with its full configuration (R8.1) — task, arm, the
lever env it ran under, seeds, model, stop reason, lint verdict — and VOID-flag
a byte-identical arm pair at write time (R8.2); @""@ disables.
-}
saveGateEpisode ::
    BenchConfig ->
    GateLever ->
    Text ->
    Task ->
    Int ->
    [Int] ->
    SearchMode ->
    AgentRun ->
    IO ()
saveGateEpisode cfg lever base task seed seedsTried mode run =
    case bcTranscriptDir cfg of
        "" -> pure ()
        dir -> do
            runTime <- nowIso
            mFlag <- saveEpisodeIn dir (meta runTime) (arTranscript run)
            mapM_ (\p -> putStrLn ("[gate] pair flag written: " <> p)) mFlag
  where
    prov = bcProvenance cfg
    meta runTime =
        EpisodeMeta
            { emTask = taskId task
            , emArm = modeText mode
            , emLevers = levers
            , emSeed = seed
            , emSeedsTried = seedsTried
            , emModel = bcModel cfg
            , emStopped = arStopped run
            , emFinal = arFinal run
            , emLint =
                lintLine
                    ( lintMessages (arTranscript run)
                        <> stopIssues (arStopped run) (arFinal run)
                    )
            , emRunId = rpRunId prov
            , emCommit = rpCommit prov
            , emBuildTime = rpBuildTime prov
            , emRunTime = runTime
            , emEndpoint = base
            , emRelinkProbe = rpRelink prov
            }
    levers =
        [(T.pack k, T.pack v) | (k, v) <- searchEnv lever mode]
            <> [
                   ( "SABELA_CAPABILITY_SEARCH"
                   , maybe "unset" T.pack (capabilityEnvFor lever mode)
                   )
               ]
