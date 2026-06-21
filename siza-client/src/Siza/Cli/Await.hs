{- | The CLI adapter for @siza await-idle@: loop the typed @await_idle@ tool
past @timedOut@ to an overall budget, then exit on the terminal state.

This is the binary home of the old @siza-await-idle.sh@ wrapper. It is a pure
waiting loop — it branches only on the terminal @state@/@waited@ tags the
server returns, never on observed cell content. Exit codes mirror the script:
0 settled idle, 4 kernel absent / died, 5 timed out, 6 transport failure.
-}
module Siza.Cli.Await (
    awaitBudgetParser,
    runAwaitIdle,
) where

import Data.Aeson (Value, object)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Options.Applicative
import Sabela.AI.Capabilities.ToolName (ToolName (AwaitIdle))
import Sabela.AI.Types (toolOutcomeValue)
import Siza.Transport (Conn, callTool)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)

{- | The optional positional overall budget (seconds) before giving up,
defaulting to 180 — the same default the bash wrapper used.
-}
awaitBudgetParser :: Parser Int
awaitBudgetParser =
    argument
        auto
        ( metavar "SECONDS"
            <> value 180
            <> help "overall budget before giving up (default 180)"
        )

{- | Loop the @await_idle@ tool until the kernel settles or the budget runs
out. Each call returns a @waited@ tag and a fresh @status@; the typed
@status.state.state@ tag is what @idle@ is read from. Re-loops only past
@timedOut@; any terminal state exits.
-}
runAwaitIdle :: Conn -> Text -> Int -> IO ()
runAwaitIdle conn base budget = do
    now <- getCurrentTime
    let deadline = addUTCTime (fromIntegral budget) now
    loop deadline
  where
    loop deadline = do
        res <- callTool conn base AwaitIdle (object [])
        case res of
            Left e -> die 6 ("await_idle call failed: " <> T.unpack e)
            Right outcome -> step deadline (toolOutcomeValue outcome)
    step deadline v = do
        let state = textField ["status", "state", "state"] "cold" v
            waited = textField ["waited"] "timedOut" v
        case () of
            _
                | state == "cold" ->
                    die 4 "kernel absent - no session to wait for."
                | state == "idle" -> hPutStrLn stderr "idle." >> exitSuccess
                | waited == "kernelDead" -> die 4 "kernel died mid-wait."
                | otherwise ->
                    pastBudget deadline >>= \over ->
                        if over
                            then
                                die
                                    5
                                    ( "timed out after "
                                        <> show budget
                                        <> "s (state="
                                        <> T.unpack state
                                        <> ")."
                                    )
                            else loop deadline

pastBudget :: UTCTime -> IO Bool
pastBudget deadline = (>= deadline) <$> getCurrentTime

-- | Read a dotted-path text field, falling back to a default when absent.
textField :: [Text] -> Text -> Value -> Text
textField path def v = case foldl step (Just v) path of
    Just (A.String s) -> s
    _ -> def
  where
    step (Just (A.Object o)) k = KM.lookup (K.fromText k) o
    step _ _ = Nothing

die :: Int -> String -> IO a
die code msg = do
    hPutStrLn stderr ("siza: await-idle: " <> msg)
    exitWith (ExitFailure code)
