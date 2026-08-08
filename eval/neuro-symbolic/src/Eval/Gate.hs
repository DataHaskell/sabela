{-# LANGUAGE OverloadedStrings #-}

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
import Eval.Tools (dispatch, episodeCatalogue)
import Eval.TranscriptLint (lintLine, lintMessages, stopIssues)
import Siza.Agent.Transcript (contextChars)

data GateLever = ResolverLever | CapabilityLever | ServerFlagLever String
    deriving (Eq, Show)

searchEnv :: GateLever -> SearchMode -> [(String, String)]
searchEnv ResolverLever SearchOn = [("SABELA_HOOGLE_RESOLVE", "1")]
searchEnv ResolverLever SearchOff = [("SABELA_HOOGLE_RESOLVE", "0")]
searchEnv (ServerFlagLever var) SearchOn = [(var, "1")]
searchEnv (ServerFlagLever var) SearchOff = [(var, "0")]
searchEnv _ _ = []

setCapabilityEnv :: GateLever -> SearchMode -> IO ()
setCapabilityEnv lever mode = case capabilityEnvFor lever mode of
    Just v -> setEnv "SABELA_CAPABILITY_SEARCH" v
    Nothing -> unsetEnv "SABELA_CAPABILITY_SEARCH"

capabilityEnvFor :: GateLever -> SearchMode -> Maybe String
capabilityEnvFor CapabilityLever SearchOn = Just "1"
capabilityEnvFor CapabilityLever SearchOff = Nothing
capabilityEnvFor _ _ = Just "1"

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

gateRuns :: [Task] -> [Int] -> [(Task, Int, SearchMode)]
gateRuns tasks seeds =
    concat
        [ [(task, seed, mode) | mode <- armOrder i]
        | (i, (task, seed)) <-
            zip [0 ..] [(t, s) | t <- tasks, s <- seeds]
        ]

armOrder :: Int -> [SearchMode]
armOrder i
    | even (i :: Int) = [SearchOff, SearchOn]
    | otherwise = [SearchOn, SearchOff]

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
    cat <- episodeCatalogue
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
                        , drvVerify = const (gradeVerify (bcConn cfg) base task)
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

maxEpisodeRetries :: Int
maxEpisodeRetries = 2

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
