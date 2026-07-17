{-# LANGUAGE OverloadedStrings #-}

{- | The @siza chat@ subcommand: parse its options and run the interactive
local-model loop. Kept out of 'Siza.Cli' so the top-level dispatch stays small,
mirroring the other @Siza.Cli.*@ subcommand modules.
-}
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

{- | @siza chat@ options. The model and server URL default to the ollama tag and
the discovered server; the limits are sized for interactive use, not eval.
-}
data ChatOpts = ChatOpts
    { coModel :: Text
    , coUrl :: Maybe Text
    , coTimeout :: Int
    , coMaxTurns :: Int
    , coVerbose :: Bool
    }
    deriving (Show)

{- | @siza chat [--model M] [--url U] [--timeout SECS] [--max-turns N] [--verbose]@:
drive a local ollama model over the notebook. Defaults to @gpt-oss:20b@ and the
discovered server; @--verbose@ streams the full audit, not the terse progress view.
-}
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
                <> value 300
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

{- | Preflight ollama, resolve and reach the target server (@--url@ or discovered),
then hand off to the interactive loop. The hard request cap sits above the loop's
own deadline so the graceful stop wins.
-}
runChatCommand ::
    ChatOpts ->
    -- | Connect + guard hub auth (the caller's @withConn@).
    ((Conn -> IO ()) -> IO ()) ->
    -- | Resolve the base URL (explicit @--url@ or the discovered server).
    (Conn -> Maybe Text -> (Text -> IO ()) -> IO ()) ->
    -- | Warn when the target is non-localhost.
    (Text -> IO ()) ->
    -- | Report no reachable server and exit.
    (String -> IO ()) ->
    IO ()
runChatCommand opts withConn resolveBase warnNonLocal noServer = do
    mgr <- newTlsManager
    ensureOllama mgr
    -- Before withConn: the hub-token attach keys on SABELA_URL, so a --url set
    -- after the Conn is built would target the hub unauthenticated.
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
