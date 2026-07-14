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
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (getArgs, lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.Process (callProcess)

import Eval.Agent (EpisodeBudget (..), defaultBudget)
import Eval.Chat (runChat)
import Eval.Preflight (ensureOllama)
import Siza.Transport (Conn (..), Env (..), newConn)

main :: IO ()
main =
    getArgs >>= \case
        ("update" : rest) -> runUpdate rest
        _ -> runChatSession

{- | @siza-chat update [--names-only|--hoogle-only]@ refreshes the LOCAL Hoogle +
Hackage-names search cache the resolver queries (never the public services). It
runs the repo's tools/update-search-cache.sh, located by walking up from the cwd.
-}
runUpdate :: [String] -> IO ()
runUpdate flags = do
    cwd <- getCurrentDirectory
    mScript <- findUp cwd ("tools" </> "update-search-cache.sh")
    case mScript of
        Nothing ->
            putStrLn
                "could not find tools/update-search-cache.sh above the current \
                \directory; run siza-chat update from inside the sabela repo"
        Just script -> callProcess "bash" (script : flags)

-- | The nearest ancestor of @dir@ (inclusive) containing @rel@, if any.
findUp :: FilePath -> FilePath -> IO (Maybe FilePath)
findUp dir rel = do
    hit <- doesFileExist (dir </> rel)
    if hit
        then pure (Just (dir </> rel))
        else
            let parent = takeDirectory dir
             in if parent == dir then pure Nothing else findUp parent rel

runChatSession :: IO ()
runChatSession = do
    model <- T.pack . fromMaybe "gpt-oss:20b" <$> lookupEnv "SIZA_EVAL_MODEL"
    debug <- isNothing <$> lookupEnv "SIZA_EVAL_QUIET"
    maxTurns <- maybe 100 read <$> lookupEnv "SIZA_EVAL_MAX_TURNS"
    budget <- envBudget
    mgr <- newTlsManager
    ensureOllama mgr
    conn <- newConn
    let base = fromMaybe "http://localhost:3000" (envSabelaUrl (connEnv conn))
    runChat debug budget maxTurns mgr conn base model

{- | Episode budget over 'defaultBudget' with chat-generous defaults (50 repair
rounds, a 30-minute deadline); the same env knobs as siza-eval override them.
-}
envBudget :: IO EpisodeBudget
envBudget = do
    d <- maybe 50 read <$> lookupEnv "SIZA_EVAL_MAX_REPAIRS"
    secs <- maybe 1800 read <$> lookupEnv "SIZA_EVAL_DEADLINE_SECS"
    pure defaultBudget{ebMaxRepairs = d, ebDeadlineSecs = secs}
