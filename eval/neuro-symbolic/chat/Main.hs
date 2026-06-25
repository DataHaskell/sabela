{- | siza-chat: an interactive harness session. Connects to a running Sabela
server (@SABELA_URL@, default @http://localhost:3007@) and drives the local model
(@SIZA_EVAL_MODEL@, default @gpt-oss:20b@) through the eval harness loop on that
notebook, reading free-form requests from the terminal. Set @SIZA_EVAL_DEBUG@ to
stream the full audit (system prompt, inputs, the model's thinking, tool calls,
and outcomes) as each turn runs. @SIZA_EVAL_MAX_TURNS@ (default 12),
@SIZA_EVAL_MAX_REPAIRS@, and @SIZA_EVAL_DEADLINE_SECS@ size the per-request budget.
-}
module Main (main) where

import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import Network.HTTP.Client.TLS (newTlsManager)
import System.Environment (lookupEnv)

import Eval.Agent (EpisodeBudget (..), defaultBudget)
import Eval.Chat (runChat)
import Siza.Transport (Conn (..), Env (..), newConn)

main :: IO ()
main = do
    model <- T.pack . fromMaybe "gpt-oss:20b" <$> lookupEnv "SIZA_EVAL_MODEL"
    debug <- isJust <$> lookupEnv "SIZA_EVAL_DEBUG"
    maxTurns <- maybe 12 read <$> lookupEnv "SIZA_EVAL_MAX_TURNS"
    budget <- envBudget
    mgr <- newTlsManager
    conn <- newConn
    let base = fromMaybe "http://localhost:3007" (envSabelaUrl (connEnv conn))
    runChat debug budget maxTurns mgr conn base model

-- | Episode budget from the same env knobs as siza-eval, over 'defaultBudget'.
envBudget :: IO EpisodeBudget
envBudget = do
    d <-
        maybe (ebMaxRepairs defaultBudget) read <$> lookupEnv "SIZA_EVAL_MAX_REPAIRS"
    secs <-
        maybe (ebDeadlineSecs defaultBudget) read
            <$> lookupEnv "SIZA_EVAL_DEADLINE_SECS"
    pure defaultBudget{ebMaxRepairs = d, ebDeadlineSecs = secs}
