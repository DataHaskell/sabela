{-# LANGUAGE OverloadedStrings #-}

module Siza.Cli.Chat (
    ChatOpts (..),
    chatOptsParser,
    runChatCommand,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client.TLS (newTlsManager)
import Options.Applicative
import Siza.Agent.Chat (ChatConfig (..), runChat)
import Siza.Agent.Loop (EpisodeBudget (..), defaultBudget)
import Siza.Agent.Preflight (ensureOllama)
import Siza.Transport (Conn, applyUrlOverride, getHealth)

data ChatOpts = ChatOpts
    { coModel :: Text
    , coUrl :: Maybe Text
    , coTimeout :: Int
    , coMaxTurns :: Int
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
    verboseOpt =
        switch
            ( long "verbose"
                <> help "Stream the full audit (system prompt, thinking, tool JSON)"
            )

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
                    let budget =
                            defaultBudget
                                { ebMaxRepairs = 8
                                , ebDeadlineSecs = fromIntegral (coTimeout opts)
                                }
                        cfg =
                            ChatConfig
                                { ccModel = coModel opts
                                , ccVerbose = coVerbose opts
                                , ccBudget = budget
                                , ccMaxTurns = coMaxTurns opts
                                , ccRequestTimeoutSecs = coTimeout opts + 30
                                }
                    runChat cfg mgr conn base
