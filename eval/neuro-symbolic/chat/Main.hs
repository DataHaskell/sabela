{- | siza-chat: an interactive harness session. Connects to a running Sabela
server (@SABELA_URL@, default @http://localhost:3007@) and drives the local model
(@SIZA_EVAL_MODEL@, default @gpt-oss:20b@) through the eval harness loop on that
notebook, reading free-form requests from the terminal. The full audit (system
prompt, inputs, the model's thinking, tool calls, outcomes, and a circularity
marker when a call repeats) streams by default; set @SIZA_EVAL_QUIET@ for a terse
progress view. @SIZA_EVAL_MAX_TURNS@ (default 100), @SIZA_EVAL_MAX_REPAIRS@
(default 50), and @SIZA_EVAL_DEADLINE_SECS@ (default 1800) size the per-request
budget — generous, to watch how long a hard request takes to converge.
-}
module Main (main) where

import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import Network.HTTP.Client.TLS (newTlsManager)
import System.Environment (lookupEnv)

import Eval.Agent (EpisodeBudget (..), defaultBudget)
import Eval.Chat (runChat)
import Siza.Transport (Conn (..), Env (..), newConn)

main :: IO ()
main = do
    model <- T.pack . fromMaybe "gpt-oss:20b" <$> lookupEnv "SIZA_EVAL_MODEL"
    debug <- isNothing <$> lookupEnv "SIZA_EVAL_QUIET"
    maxTurns <- maybe 100 read <$> lookupEnv "SIZA_EVAL_MAX_TURNS"
    budget <- envBudget
    mgr <- newTlsManager
    conn <- newConn
    let base = fromMaybe "http://localhost:3007" (envSabelaUrl (connEnv conn))
    runChat debug budget maxTurns mgr conn base model

{- | Episode budget over 'defaultBudget' with chat-generous defaults (50 repair
rounds, a 30-minute deadline); the same env knobs as siza-eval override them.
-}
envBudget :: IO EpisodeBudget
envBudget = do
    d <- maybe 50 read <$> lookupEnv "SIZA_EVAL_MAX_REPAIRS"
    secs <- maybe 1800 read <$> lookupEnv "SIZA_EVAL_DEADLINE_SECS"
    pure defaultBudget{ebMaxRepairs = d, ebDeadlineSecs = secs}
