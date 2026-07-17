{- | Grade-wired entrypoints over the shared 'Siza.Agent.Loop'. The loop itself
lives in siza-client (so the product @siza chat@ shares it); this module keeps the
benchmark 'Task'/grader wiring — 'runEpisode' and 'runEpisodeDebug', which build a
'Driver' whose verify step is the objective grader — and re-exports the loop
surface under the @Eval.Agent@ name its importers already use.
-}
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

import Eval.Task (Task (..), Verdict (Surfaced), grade)
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
                , drvVerify = (== Surfaced) . fst <$> grade conn base task
                }
    runEpisodeWith budget driver (taskPrompt task) maxTurns

{- | Like 'runEpisode' but streams the whole conversation to @emit@ as it happens
and turns on the model's reasoning channel, for the single-run debug path. The
benchmark path stays silent and thinking-free.
-}
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
                , drvVerify = (== Surfaced) . fst <$> grade conn base task
                }
    runEpisodeTraced emit GrammarOn budget driver (taskPrompt task) maxTurns
