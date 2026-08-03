{-# LANGUAGE OverloadedStrings #-}

module Siza.Cli.Chat (
    ChatOpts (..),
    chatBudget,
    chatOptsParser,
    defaultMaxRepairs,
    maxRepairsEnv,
    minMaxRepairs,
    resolveMaxRepairs,
    runChatCommand,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client.TLS (newTlsManager)
import Options.Applicative
import Siza.Agent.Chat (ChatConfig (..), runChat)
import Siza.Agent.Loop (EpisodeBudget (..), defaultBudget)
import Siza.Agent.Preflight (ensureOllama)
import Siza.Transport (Conn, applyUrlOverride, getHealth)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data ChatOpts = ChatOpts
    { coModel :: Text
    , coUrl :: Maybe Text
    , coTimeout :: Int
    , coMaxTurns :: Int
    , coMaxRepairs :: Maybe Int
    , coVerbose :: Bool
    }
    deriving (Show)

chatOptsParser :: Parser ChatOpts
chatOptsParser =
    ChatOpts
        <$> modelOpt
        <*> optional urlOpt
        <*> timeoutOpt
        <*> maxTurnsOpt
        <*> optional maxRepairsOpt
        <*> verboseOpt
  where
    modelOpt =
        T.pack
            <$> strOption
                ( long "model"
                    <> value "gpt-oss:20b"
                    <> showDefault
                    <> metavar "MODEL"
                    <> help "Ollama model tag"
                )
    urlOpt =
        T.pack
            <$> strOption
                ( long "url"
                    <> metavar "URL"
                    <> help "Sabela server URL; overrides $SABELA_URL (default: the discovered server)"
                )
    timeoutOpt =
        option
            auto
            ( long "timeout"
                <> value 1800
                <> showDefault
                <> metavar "SECS"
                <> help "Per-request wall-clock cap"
            )
    maxTurnsOpt =
        option
            auto
            ( long "max-turns"
                <> value 40
                <> showDefault
                <> metavar "N"
                <> help "Max harness turns per request"
            )
    maxRepairsOpt =
        option
            auto
            ( long "max-repairs"
                <> metavar "N"
                <> help
                    ( "Max harness repair rounds per request (minimum "
                        ++ show minMaxRepairs
                        ++ "); without it, $"
                        ++ maxRepairsEnv
                        ++ " and then "
                        ++ show defaultMaxRepairs
                    )
            )
    verboseOpt =
        switch
            ( long "verbose"
                <> help "Stream the full audit (system prompt, thinking, tool JSON)"
            )

maxRepairsEnv :: String
maxRepairsEnv = "SIZA_MAX_REPAIRS"

defaultMaxRepairs :: Int
defaultMaxRepairs = 8

{- | The loop tests the repair count before its first turn, so a budget below
one ends the episode with nothing asked of the model.
-}
minMaxRepairs :: Int
minMaxRepairs = 1

-- | The flag beats the environment, which beats the built-in default.
resolveMaxRepairs :: Maybe Int -> Maybe String -> Int
resolveMaxRepairs given env =
    max minMaxRepairs (fromMaybe defaultMaxRepairs (given <|> (readMaybe =<< env)))

{- | The one place a chat request's stop conditions are built, so the repair
budget cannot drift from the turn and wall-clock budgets beside it.
-}
chatBudget :: Int -> ChatOpts -> EpisodeBudget
chatBudget repairs opts =
    defaultBudget
        { ebMaxRepairs = repairs
        , ebDeadlineSecs = fromIntegral (coTimeout opts)
        }

runChatCommand ::
    ChatOpts ->
    ((Conn -> IO ()) -> IO ()) ->
    (Conn -> Maybe Text -> (Text -> IO ()) -> IO ()) ->
    (Text -> IO ()) ->
    (String -> IO ()) ->
    IO ()
runChatCommand opts withConn resolveBase warnNonLocal noServer = do
    mgr <- newTlsManager
    ensureOllama mgr
    applyUrlOverride (coUrl opts)
    withConn $ \conn ->
        resolveBase conn (coUrl opts) $ \base -> do
            mh <- getHealth conn base
            case mh of
                Nothing -> noServer ("chat: no server reachable at " <> T.unpack base)
                Just _ -> do
                    warnNonLocal base
                    repairs <-
                        resolveMaxRepairs (coMaxRepairs opts)
                            <$> lookupEnv maxRepairsEnv
                    let budget = chatBudget repairs opts
                        cfg =
                            ChatConfig
                                { ccModel = coModel opts
                                , ccVerbose = coVerbose opts
                                , ccBudget = budget
                                , ccMaxTurns = coMaxTurns opts
                                , ccRequestTimeoutSecs = coTimeout opts + 30
                                }
                    runChat cfg mgr conn base
