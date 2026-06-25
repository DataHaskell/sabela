{- | siza-eval: run one baseline task and print a JSON result line.

Expects @SABELA_URL@ to point at a freshly started Sabela server with an empty
notebook (the @run.sh@ orchestrator gives each task its own server for clean
isolation). Usage: @siza-eval <task-id>@, or @siza-eval all@.
-}
module Main (main) where

import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import System.Environment (getArgs, lookupEnv)

import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)

import Eval.Agent (AgentRun (..), EpisodeBudget (..), defaultBudget, runEpisode)
import Eval.Task (Task (..), Verdict (..), findTask, grade, tasks)
import Eval.Transcript (renderTranscript)
import Siza.Transport (Conn (..), Env (..), newConn)

main :: IO ()
main = do
    args <- getArgs
    model <- T.pack . fromMaybe "gpt-oss:20b" <$> lookupEnv "SIZA_EVAL_MODEL"
    maxTurns <- maybe 14 read <$> lookupEnv "SIZA_EVAL_MAX_TURNS"
    budget <- envBudget
    sessionDir <- lookupEnv "SIZA_EVAL_SESSION_DIR"
    mgr <- newTlsManager
    conn <- newConn
    let base = fromMaybe "http://localhost:3000" (envSabelaUrl (connEnv conn))
        chosen = case args of
            ("all" : _) -> tasks
            (tid : _) -> maybe [] pure (findTask (T.pack tid))
            [] -> tasks
    mapM_ (runOne sessionDir budget mgr conn base model maxTurns) chosen

envBudget :: IO EpisodeBudget
envBudget = do
    d <-
        maybe (ebMaxRepairs defaultBudget) read <$> lookupEnv "SIZA_EVAL_MAX_REPAIRS"
    secs <-
        maybe (ebDeadlineSecs defaultBudget) read
            <$> lookupEnv "SIZA_EVAL_DEADLINE_SECS"
    pure defaultBudget{ebMaxRepairs = d, ebDeadlineSecs = secs}

runOne ::
    Maybe FilePath ->
    EpisodeBudget ->
    Manager ->
    Conn ->
    Text ->
    Text ->
    Int ->
    Task ->
    IO ()
runOne sessionDir budget mgr conn base model maxTurns task = do
    t0 <- getCurrentTime
    run <- runEpisode budget mgr conn base model task maxTurns
    writeTranscript sessionDir task run
    (verdict, evidence) <- grade conn base task
    t1 <- getCurrentTime
    let secs = realToFrac (diffUTCTime t1 t0) :: Double
    LBS8.putStrLn (encode (result task run verdict evidence secs model))

{- | Write the episode's message log to @<dir>/<task>.md@ when
@SIZA_EVAL_SESSION_DIR@ is set, so a run leaves a readable transcript.
-}
writeTranscript :: Maybe FilePath -> Task -> AgentRun -> IO ()
writeTranscript Nothing _ _ = pure ()
writeTranscript (Just dir) task run = do
    createDirectoryIfMissing True dir
    TIO.writeFile
        (dir ++ "/" ++ T.unpack (taskId task) ++ ".md")
        (renderTranscript (taskId task) (arTranscript run))

{- | A diff is surfaced (the task passed) only when its covering test is green;
a 'ProposeTest' verdict is not a pass — it stages a test for the user. The
@verdict@/@proposed@ fields record which path C3 took.
-}
result :: Task -> AgentRun -> Verdict -> Text -> Double -> Text -> Value
result task run verdict evidence secs model =
    object
        [ "task" .= taskId task
        , "model" .= model
        , "passed" .= (verdict == Surfaced)
        , "verdict" .= verdictTag verdict
        , "proposed_test" .= proposedTest verdict
        , "turns" .= arTurns run
        , "tool_calls" .= arToolCalls run
        , "stopped" .= arStopped run
        , "seconds" .= secs
        , "final" .= T.take 400 (arFinal run)
        , "evidence" .= T.take 400 evidence
        ]

verdictTag :: Verdict -> Text
verdictTag Surfaced = "surfaced"
verdictTag (Withheld _) = "withheld"
verdictTag (ProposeTest _) = "propose_test"

proposedTest :: Verdict -> Text
proposedTest (ProposeTest cell) = T.take 400 cell
proposedTest _ = ""
