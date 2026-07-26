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

awaitBudgetParser :: Parser Int
awaitBudgetParser =
    argument
        auto
        ( metavar "SECONDS"
            <> value 180
            <> help "overall budget before giving up (default 180)"
        )

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
