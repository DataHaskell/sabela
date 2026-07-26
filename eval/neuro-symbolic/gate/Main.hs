module Main (main) where

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Client.TLS (newTlsManager)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Eval.Agent (EpisodeBudget (..), defaultBudget)
import Eval.Bench (BenchConfig (..))
import qualified Eval.Corpus as Corpus
import Eval.Episode (
    defaultToolTimeout,
    naNote,
    readNaFlags,
    readSaturatedFlags,
    readVoidFlags,
    saturatedNote,
    voidNote,
 )
import Eval.Gate (
    GateLever (..),
    GateResult (..),
    readGateResults,
    renderGateResults,
    runGateResuming,
 )
import Eval.GateMetrics (renderGateMetrics)
import Eval.Provenance (
    RunProvenance (..),
    captureProvenanceCheckedSelf,
    freshRunDirUnder,
 )
import Eval.ReportGuard (guardReportDirFor)
import Eval.Task (Task (..), taskId)
import Siza.Transport (newConn)

main :: IO ()
main = do
    model <- T.pack . fromMaybe "gpt-oss:20b" <$> lookupEnv "SIZA_EVAL_MODEL"
    seeds <- parseSeeds <$> lookupEnv "SIZA_GATE_SEEDS"
    foldSel <- normFold <$> lookupEnv "SIZA_GATE_FOLD"
    bin <- fromMaybe defaultBin <$> lookupEnv "SABELA_BIN"
    prov <- captureProvenanceCheckedSelf bin
    transcripts <-
        fromMaybe (freshRunDirUnder "/tmp/siza-gate-transcripts" prov)
            <$> lookupEnv "SIZA_GATE_TRANSCRIPTS"
    resultsFile <-
        fromMaybe "/tmp/siza-gate-results.jsonl" <$> lookupEnv "SIZA_GATE_RESULTS"
    taskSel <- lookupEnv "SIZA_BENCH_TASKS"
    budget <- envBudget
    maxTurns <- maybe 12 read <$> lookupEnv "SIZA_EVAL_MAX_TURNS"
    toolTimeout <- defaultToolTimeout
    mgr <- newTlsManager
    conn <- newConn
    let cfg =
            BenchConfig mgr conn model budget maxTurns bin 3300 transcripts prov
        tasks = selectTasks (Corpus.selectFold (Just foldSel)) taskSel
    TIO.putStrLn
        (banner model seeds foldSel (length tasks) budget maxTurns toolTimeout)
    case foldSel of
        "all" -> reportGap cfg resultsFile seeds
        _ -> do
            _ <- runGateResuming cfg (leverFor foldSel) resultsFile tasks seeds
            rs <- readGateResults resultsFile
            printGuarded cfg rs

envBudget :: IO EpisodeBudget
envBudget = do
    d <-
        maybe (ebMaxRepairs defaultBudget) read <$> lookupEnv "SIZA_EVAL_MAX_REPAIRS"
    secs <-
        maybe (ebDeadlineSecs defaultBudget) read
            <$> lookupEnv "SIZA_EVAL_DEADLINE_SECS"
    pure defaultBudget{ebMaxRepairs = d, ebDeadlineSecs = secs}

printGuarded :: BenchConfig -> [GateResult] -> IO ()
printGuarded cfg rs = do
    let dir = bcTranscriptDir cfg
    voids <- readVoidFlags dir
    nas <- readNaFlags dir
    sats <- readSaturatedFlags dir
    let excluded = voids ++ nas ++ sats
        live = [g | g <- rs, (grTask g, grSeed g) `notElem` excluded]
    metrics <- renderGateMetrics dir live
    guarded <-
        guardReportDirFor dir (rpRunId (bcProvenance cfg)) $
            voidNote voids
                <> naNote nas
                <> saturatedNote sats
                <> renderGateResults live
                <> metrics
    TIO.putStr guarded

normFold :: Maybe String -> Text
normFold m = case fmap (T.toLower . T.strip . T.pack) m of
    Just "in-index" -> "in-index"
    Just "capability" -> "capability"
    Just "reasoning" -> "reasoning"
    Just "hole-fit" -> "hole-fit"
    Just "arity-fix" -> "arity-fix"
    Just "live-grammar" -> "live-grammar"
    Just "self-heal" -> "self-heal"
    Just "all" -> "all"
    _ -> "held-out"

leverFor :: Text -> GateLever
leverFor "capability" = CapabilityLever
leverFor "reasoning" = CapabilityLever
leverFor "hole-fit" = ServerFlagLever "SABELA_HOLE_FIT"
leverFor "arity-fix" = ServerFlagLever "SABELA_ARITY_FIX"
leverFor "live-grammar" = ServerFlagLever "SABELA_LIVE_GRAMMAR"
leverFor "self-heal" = ServerFlagLever "SABELA_SELF_HEAL_REENTER"
leverFor "type-resolve" = ServerFlagLever "SABELA_TYPE_RESOLVE"
leverFor _ = ResolverLever

leverName :: Text -> Text
leverName f
    | f `elem` ["capability", "reasoning"] =
        "SABELA_CAPABILITY_SEARCH (gate-process tool toggle)"
    | f == "hole-fit" = "SABELA_HOLE_FIT (server, default ON)"
    | f == "arity-fix" = "SABELA_ARITY_FIX (server, default ON)"
    | f == "live-grammar" = "SABELA_LIVE_GRAMMAR (server, default ON)"
    | f == "self-heal" = "SABELA_SELF_HEAL_REENTER (server, default ON)"
    | otherwise = "SABELA_HOOGLE_RESOLVE (server)"

reportGap :: BenchConfig -> FilePath -> [Int] -> IO ()
reportGap cfg resultsFile seeds = do
    let inIdxTasks = Corpus.selectFold (Just "in-index")
        inIdxIds = map taskId inIdxTasks
    _ <- runGateResuming cfg ResolverLever resultsFile inIdxTasks seeds
    _ <-
        runGateResuming
            cfg
            ResolverLever
            resultsFile
            (Corpus.selectFold (Just "held-out"))
            seeds
    allResults <- readGateResults resultsFile
    voids <- readVoidFlags (bcTranscriptDir cfg)
    nas <- readNaFlags (bcTranscriptDir cfg)
    sats <- readSaturatedFlags (bcTranscriptDir cfg)
    let excluded = voids ++ nas ++ sats
        liveResults =
            [g | g <- allResults, (grTask g, grSeed g) `notElem` excluded]
        inIdx = [g | g <- liveResults, grTask g `elem` inIdxIds]
        held = [g | g <- liveResults, grTask g `notElem` inIdxIds]
    metrics <- renderGateMetrics (bcTranscriptDir cfg) liveResults
    guarded <-
        guardReportDirFor (bcTranscriptDir cfg) (rpRunId (bcProvenance cfg)) $
            voidNote voids
                <> naNote nas
                <> saturatedNote sats
                <> "== in-index ==\n"
                <> renderGateResults inIdx
                <> "\n== held-out ==\n"
                <> renderGateResults held
                <> "\nindex-vs-held-out gap: "
                <> renderGap inIdx held
                <> "\n\n"
                <> metrics
    TIO.putStr guarded

renderGap :: [GateResult] -> [GateResult] -> Text
renderGap inIdx held =
    "held-out "
        <> rate held
        <> " - in-index "
        <> rate inIdx
        <> " = "
        <> T.pack (show (round3 (passRate held - passRate inIdx)))
  where
    rate o = T.pack (show (passRate o))
    passRate o =
        let ok = length [() | g <- o, grPass g]
            n = length o
         in if n == 0 then 0 else fromIntegral ok / fromIntegral n :: Double

banner :: Text -> [Int] -> Text -> Int -> EpisodeBudget -> Int -> Int -> Text
banner model seeds fold nTasks budget maxTurns toolTimeout =
    "siza-gate \183 "
        <> model
        <> " \183 seeds "
        <> T.pack (show seeds)
        <> " \183 "
        <> T.pack (show nTasks)
        <> " tasks ("
        <> fold
        <> ") \183 grammar fixed ON \183 lever "
        <> leverName fold
        <> " \183 fresh server per run from port 3300"
        <> "\n  budget: deadline "
        <> T.pack (show (round (ebDeadlineSecs budget) :: Int))
        <> "s, max repairs "
        <> T.pack (show (ebMaxRepairs budget))
        <> ", max turns "
        <> T.pack (show maxTurns)
        <> ", tool timeout "
        <> T.pack (show toolTimeout)
        <> "s"

defaultBin :: FilePath
defaultBin =
    "dist-newstyle/build/aarch64-osx/ghc-9.12.2/sabela-0.1.0.0/x/sabela/build/sabela/sabela"

parseSeeds :: Maybe String -> [Int]
parseSeeds = maybe dflt (orDefault dflt . mapMaybe (readMaybe . trim) . splitComma)
  where
    dflt = [1]

selectTasks :: [Task] -> Maybe String -> [Task]
selectTasks pool Nothing = pool
selectTasks pool (Just s) =
    orDefault pool (filter ((`elem` want) . T.unpack . taskId) pool)
  where
    want = map trim (splitComma s)

round3 :: Double -> Double
round3 x = fromIntegral (round (x * 1000) :: Int) / 1000

orDefault :: [a] -> [a] -> [a]
orDefault d [] = d
orDefault _ xs = xs

splitComma :: String -> [String]
splitComma s = case break (== ',') s of
    (a, ',' : rest) -> a : splitComma rest
    (a, _) -> [a]

trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ')
