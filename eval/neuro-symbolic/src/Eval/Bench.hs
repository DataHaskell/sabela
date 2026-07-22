{-# LANGUAGE OverloadedStrings #-}

{- | The two-arm bench driver: fresh server per episode, provenance-stamped
transcript persistence (R8.1/R8.3), and a loud abort when a spawned server
never turns healthy (M8). Reporting lives in "Eval.BenchReport" (re-exported).
-}
module Eval.Bench (
    BenchConfig (..),
    runArm,
    runBench,
    saveTranscript,
    modeTag,
    withFreshServer,
    withFreshServerEnv,
    waitHealth,
    waitHealthN,
    module Eval.BenchReport,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad (forM)
import Data.Text (Text)
import qualified Data.Text as T
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
import Eval.BenchReport
import Eval.ColdStart (purgeColdStart)
import Eval.Episode (EpisodeMeta (..), saveEpisodeIn)
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
import Siza.Transport (Conn, getHealth)

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
    , bcProvenance :: RunProvenance
    -- ^ Captured once at driver start; stamped into every header (R8.3).
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
                , drvVerify = gradeVerify (bcConn cfg) base task
                }
    run <-
        runEpisodeWith' mode (bcBudget cfg) driver (taskPrompt task) (bcMaxTurns cfg)
    saveTranscript cfg base task seed mode run
    (v, _) <- grade (bcConn cfg) base task
    pure (RunStat (v == Surfaced) (arTurns run) (arToolCalls run))

{- | Persist the run with its full config header (R8.1) — including the run's
provenance and the endpoint it drove — and VOID-flag a byte-identical arm
pair at write time (R8.2); @""@ disables.
-}
saveTranscript ::
    BenchConfig -> Text -> Task -> Int -> GrammarMode -> AgentRun -> IO ()
saveTranscript cfg base task seed mode run = case bcTranscriptDir cfg of
    "" -> pure ()
    dir -> do
        runTime <- nowIso
        mFlag <- saveEpisodeIn dir (meta runTime) (arTranscript run)
        mapM_ (\p -> putStrLn ("[bench] pair flag written: " <> p)) mFlag
  where
    prov = bcProvenance cfg
    meta runTime =
        EpisodeMeta
            { emTask = taskId task
            , emArm = T.pack (modeTag mode)
            , emLevers = [("grammar", T.pack (modeTag mode))]
            , emSeed = seed
            , emSeedsTried = [seed]
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

modeTag :: GrammarMode -> String
modeTag GrammarOff = "off"
modeTag GrammarOn = "on"

{- | Run every (task, seed, mode) episode and return each with its seed, so a
report can drop the (task, seed) pairs the applicability/VOID flags exclude
('Eval.Applicability.excludeFlagged').
-}
runBench ::
    BenchConfig -> [Task] -> [Int] -> IO [(Text, Int, GrammarMode, RunStat)]
runBench cfg tasks seeds =
    forM (zip [0 ..] runs) $ \(i, (task, seed, mode)) -> do
        purged <- purgeColdStart (taskId task)
        mapM_ (putStrLn . ("[bench] cold-start purge: " <>)) purged
        st <-
            withFreshServer
                cfg
                (bcBasePort cfg + i)
                (\base -> runArm cfg base mode seed task)
        pure (taskId task, seed, mode, st)
  where
    runs =
        [ (task, seed, mode)
        | task <- tasks
        , seed <- seeds
        , mode <- [GrammarOff, GrammarOn]
        ]

withFreshServer :: BenchConfig -> Int -> (Text -> IO a) -> IO a
withFreshServer cfg port = withFreshServerEnv cfg port []

{- | As 'withFreshServer' but appends @extra@ to the spawned server's environment
(inheriting the parent env first). The gate uses this to set the search lever
flag on the ON arm only. Aborts loudly when the server never reports healthy:
running an episode anyway would measure whatever answers on that port.
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
    r <- waitHealthN 90 (bcConn cfg) base
    case r of
        Left e -> do
            terminateProcess ph
            hClose devnull
            ioError (userError (T.unpack e))
        Right () ->
            action base `finally` (terminateProcess ph >> hClose devnull)

-- | 'waitHealthN' with the production budget; throws on an unhealthy server.
waitHealth :: Conn -> Text -> IO ()
waitHealth conn base = do
    r <- waitHealthN 90 conn base
    either (ioError . userError . T.unpack) pure r

{- | Poll @/api/ai/health@ up to @n@ times (1s apart). 'Left' names the
endpoint and says the episode must NOT run; a foreign process answering the
port never satisfies it, so a dead arm cannot masquerade as measured (M8).
-}
waitHealthN :: Int -> Conn -> Text -> IO (Either Text ())
waitHealthN n0 conn base = go n0
  where
    go 0 =
        pure . Left $
            "server never became healthy at "
                <> base
                <> " after "
                <> tshow n0
                <> "s: endpoint unreachable — aborting the episode; running it "
                <> "anyway would measure a dead or foreign endpoint, not the arm."
    go n = do
        h <- getHealth conn base
        case h of
            Just _ -> pure (Right ())
            Nothing -> threadDelay 1000000 >> go (n - 1)
