module Eval.Agent (
    module Siza.Agent.Loop,
    runEpisode,
    runEpisodeDebug,
) where

import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client (Manager)
import Sabela.LLM.Ollama.Client (chat, chatSeeded)
import Siza.Transport (Conn)

import Eval.Task (Task (..), gradeVerify)
import Siza.Agent.Loop
import Siza.Agent.Tools (catalogue, dispatch)

runEpisode ::
    EpisodeBudget -> Manager -> Conn -> Text -> Text -> Task -> Int -> IO AgentRun
runEpisode budget mgr conn base model task maxTurns = do
    cat <- catalogue
    let driver =
            Driver
                { drvChat = \msgs -> chat mgr model msgs cat
                , drvDispatch = dispatch conn base
                , drvNow = realToFrac <$> getPOSIXTime
                , drvVerify = gradeVerify conn base task
                }
    runEpisodeWith budget driver (taskPrompt task) maxTurns

runEpisodeDebug ::
    (Text -> IO ()) ->
    EpisodeBudget ->
    Manager ->
    Conn ->
    Text ->
    Text ->
    Task ->
    Int ->
    IO AgentRun
runEpisodeDebug emit budget mgr conn base model task maxTurns = do
    cat <- catalogue
    let driver =
            Driver
                { drvChat = \msgs -> chatSeeded True Nothing mgr model msgs cat
                , drvDispatch = dispatch conn base
                , drvNow = realToFrac <$> getPOSIXTime
                , drvVerify = gradeVerify conn base task
                }
    runEpisodeTraced emit GrammarOn budget driver (taskPrompt task) maxTurns
