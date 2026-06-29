{-# LANGUAGE OverloadedStrings #-}

module Eval.Bench (
    BenchConfig (..),
    ArmResult (..),
    Comparison (..),
    RunStat (..),
    ArmCost (..),
    passRate,
    twoProportionZ,
    summariseRuns,
    renderComparison,
    byTask,
    renderReport,
    armCost,
    costByTask,
    renderReportFull,
    runArm,
    runBench,
    withFreshServer,
    withFreshServerEnv,
    waitHealth,
    round1,
    tshow,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad (forM)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client (Manager)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hClose, openFile)
import System.Process (
    CreateProcess (env, std_err, std_out),
    StdStream (UseHandle),
    createProcess,
    proc,
    terminateProcess,
 )

import Eval.Agent (
    AgentRun (..),
    Driver (..),
    EpisodeBudget,
    GrammarMode (..),
    runEpisodeWith',
 )
import Eval.Ollama (chatSeeded)
import Eval.Task (Task, Verdict (Surfaced), grade, taskId)
import Eval.Tools (catalogue, dispatch)
import Eval.Transcript (renderTranscript)
import Siza.Transport (Conn, getHealth)

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
        [ "Arm A (GrammarOff): " <> rate a
        , "Arm B (GrammarOn):  " <> rate b
        , "B - A: " <> tshow (round3 diff) <> "   z = " <> tshow (round3 z)
        , "significant at 5%: " <> tshow (abs z > 1.96)
        ]
  where
    rate r = tshow (arPasses r) <> "/" <> tshow (arRuns r)

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

data BenchConfig = BenchConfig
    { bcManager :: Manager
    , bcConn :: Conn
    , bcModel :: Text
    , bcBudget :: EpisodeBudget
    , bcMaxTurns :: Int
    , bcBinary :: FilePath
    , bcBasePort :: Int
    , bcTranscriptDir :: FilePath
    -- ^ Where to write one transcript per run; @""@ disables it.
    }

runArm :: BenchConfig -> Text -> GrammarMode -> Int -> Task -> IO RunStat
runArm cfg base mode seed task = do
    cat <- catalogue
    let driver =
            Driver
                { drvChat =
                    \msgs -> chatSeeded False (Just seed) (bcManager cfg) (bcModel cfg) msgs cat
                , drvDispatch = dispatch (bcConn cfg) base
                , drvNow = realToFrac <$> getPOSIXTime
                , drvVerify = (== Surfaced) . fst <$> grade (bcConn cfg) base task
                }
    run <- runEpisodeWith' mode (bcBudget cfg) driver task (bcMaxTurns cfg)
    saveTranscript cfg task seed mode run
    (v, _) <- grade (bcConn cfg) base task
    pure (RunStat (v == Surfaced) (arTurns run) (arToolCalls run))

-- | Write a run's transcript to @<dir>/<task>-s<seed>-<mode>.md@ unless disabled.
saveTranscript :: BenchConfig -> Task -> Int -> GrammarMode -> AgentRun -> IO ()
saveTranscript cfg task seed mode run = case bcTranscriptDir cfg of
    "" -> pure ()
    dir -> do
        createDirectoryIfMissing True dir
        let name = T.unpack (taskId task) <> "-s" <> show seed <> "-" <> modeTag mode <> ".md"
        TIO.writeFile (dir </> name) (renderTranscript (taskId task) (arTranscript run))

modeTag :: GrammarMode -> String
modeTag GrammarOff = "off"
modeTag GrammarOn = "on"

runBench :: BenchConfig -> [Task] -> [Int] -> IO [(Text, GrammarMode, RunStat)]
runBench cfg tasks seeds =
    forM (zip [0 ..] runs) $ \(i, (task, seed, mode)) -> do
        st <-
            withFreshServer
                cfg
                (bcBasePort cfg + i)
                (\base -> runArm cfg base mode seed task)
        pure (taskId task, mode, st)
  where
    runs =
        [ (task, seed, mode)
        | task <- tasks
        , seed <- seeds
        , mode <- [GrammarOff, GrammarOn]
        ]

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

withFreshServer :: BenchConfig -> Int -> (Text -> IO a) -> IO a
withFreshServer cfg port = withFreshServerEnv cfg port []

{- | As 'withFreshServer' but appends @extra@ to the spawned server's environment
(inheriting the parent env first). The gate uses this to set the search lever
flag on the ON arm only.
-}
withFreshServerEnv ::
    BenchConfig -> Int -> [(String, String)] -> (Text -> IO a) -> IO a
withFreshServerEnv cfg port extra action = do
    let dir = "/tmp/siza-bench-" <> show port
        base = "http://localhost:" <> T.pack (show port)
    createDirectoryIfMissing True dir
    writeFile (dir </> "global.md") ""
    writeFile
        (dir </> "revenue.csv")
        "month,revenue\nJan,100.0\nFeb,200.0\nMar,300.0\n"
    devnull <- openFile "/dev/null" WriteMode
    procEnv <- case extra of
        [] -> pure Nothing
        _ -> Just . (++ extra) <$> getEnvironment
    (_, _, _, ph) <-
        createProcess
            (proc (bcBinary cfg) [show port, dir, dir </> "global.md"])
                { std_out = UseHandle devnull
                , std_err = UseHandle devnull
                , env = procEnv
                }
    waitHealth (bcConn cfg) base
    action base `finally` (terminateProcess ph >> hClose devnull)

waitHealth :: Conn -> Text -> IO ()
waitHealth conn base = go (90 :: Int)
  where
    go 0 = pure ()
    go n = do
        h <- getHealth conn base
        case h of
            Just _ -> pure ()
            Nothing -> threadDelay 1000000 >> go (n - 1)

round3 :: Double -> Double
round3 x = fromIntegral (round (x * 1000) :: Int) / 1000

round1 :: Double -> Double
round1 x = fromIntegral (round (x * 10) :: Int) / 10

tshow :: (Show a) => a -> Text
tshow = T.pack . show
