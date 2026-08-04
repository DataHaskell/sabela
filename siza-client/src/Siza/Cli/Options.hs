{-# LANGUAGE LambdaCase #-}

{- | The command-line surface: what @siza@ accepts and how each subcommand's
arguments are read. Running a parsed 'Command' is "Siza.Cli"'s job.
-}
module Siza.Cli.Options (
    Command (..),
    Source (..),
    parseCommand,
) where

import Data.Aeson (Value, eitherDecodeStrict, object)
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative

import Sabela.AI.Capabilities.ToolName (ToolName, parseToolName)
import Siza.Cli.Await (awaitBudgetParser)
import Siza.Cli.Chat (ChatOpts, chatOptsParser)
import Siza.Cli.Retro (RetroTarget, retroTargetParser)
import Siza.Security (Policy, advisoryPolicy, strictPolicy)

data Source = Stdin | FromFile FilePath
    deriving (Show)

data Command
    = Discover
    | Health
    | Tool Policy ToolName Value
    | Check Source Policy
    | Annotate Int Bool
    | Retro RetroTarget
    | Await Int
    | Login (Maybe Text)
    | Logout
    | Mcp
    | Tools
    | Chat ChatOpts
    deriving (Show)

parseCommand :: ParserInfo Command
parseCommand =
    info
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc "Drive a running Sabela notebook over /api/ai/*."
            <> header "siza - typed client for the Sabela AI tool surface"
        )

commandParser :: Parser Command
commandParser = hsubparser (foldMap cmd subcommands)
  where
    cmd (name, parser, desc) = command name (info parser (progDesc desc))

subcommands :: [(String, Parser Command, String)]
subcommands =
    [ ("discover", pure Discover, "List live Sabela servers as JSON.")
    , ("health", pure Health, "Probe the first live server's health.")
    , ("tool", toolParser, "Invoke an AI tool: siza tool <name> [json].")
    , ("check", checkParser, "Pre-flight parse + scan: siza check [-|FILE].")
    , ("annotate", annotateParser, "Infer unsigned binds: siza annotate CELL_ID.")
    ,
        ( "retro"
        , Retro <$> retroTargetParser
        , "Episode metrics: siza retro [FILE|--transcript FILE]."
        )
    ,
        ( "await-idle"
        , Await <$> awaitBudgetParser
        , "Block until idle: siza await-idle [SECONDS]."
        )
    , ("login", loginParser, "Authorize against a hub: siza login [HUB_URL].")
    , ("logout", pure Logout, "Forget the saved hub token.")
    , ("mcp", pure Mcp, "Serve the AI tool surface over MCP on stdio.")
    , ("tools", pure Tools, "List the tools an agent is offered, with usage.")
    ,
        ( "chat"
        , Chat <$> chatOptsParser
        , "Pair with a local model on the notebook: siza chat [--model M] [--url U]."
        )
    ]

loginParser :: Parser Command
loginParser =
    Login
        <$> optional
            ( T.pack
                <$> argument
                    str
                    ( metavar "HUB_URL"
                        <> help "Hub origin (default: $SABELA_URL), e.g. https://sabela.datahaskell.com"
                    )
            )

annotateParser :: Parser Command
annotateParser =
    Annotate
        <$> argument auto (metavar "CELL_ID" <> help "Cell id to annotate")
        <*> switch
            ( long "source"
                <> help "Emit the annotated source instead of a report"
            )

checkParser :: Parser Command
checkParser =
    Check
        <$> argument
            readSource
            ( metavar "SOURCE"
                <> value Stdin
                <> help "Source: '-' or absent for stdin, else a file path"
            )
        <*> policyFlag

policyFlag :: Parser Policy
policyFlag =
    flag
        advisoryPolicy
        strictPolicy
        ( long "strict"
            <> help "Block on a denied capability (default: advise only)"
        )

readSource :: ReadM Source
readSource = eitherReader $ \case
    "-" -> Right Stdin
    p -> Right (FromFile p)

toolParser :: Parser Command
toolParser =
    Tool
        <$> policyFlag
        <*> argument readToolName (metavar "TOOL" <> help "Tool name, e.g. list_cells")
        <*> argument
            readJson
            (metavar "JSON" <> value (object []) <> help "JSON input (default {})")

readToolName :: ReadM ToolName
readToolName = eitherReader $ \s ->
    maybe (Left ("unknown tool: " <> s)) Right (parseToolName (T.pack s))

readJson :: ReadM Value
readJson = eitherReader $ \s ->
    either
        (Left . ("invalid JSON input: " <>))
        Right
        (eitherDecodeStrict (BS8.pack s))
