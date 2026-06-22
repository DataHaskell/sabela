{- | The typed @siza@ command surface, parsed with optparse-applicative.

A 'Command' is a sum type, so an unknown subcommand or a bad tool name is a
parse failure rather than a string compared at runtime. 'runCommand' is the
dispatch the executable's @main@ wraps.
-}
module Siza.Cli (
    Command (..),
    parseCommand,
    runCommand,
    main,
) where

import Data.Aeson (Value, eitherDecodeStrict, object)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Either (fromLeft)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import Sabela.AI.Capabilities.ToolName (
    ToolName (InsertCell, ProposeEdit, ReplaceCellSource),
    parseToolName,
 )
import Sabela.AI.Types (
    ToolOutcome,
    toolOutcomeIsError,
    toolOutcomeValue,
 )
import Siza.Cli.Annotate (runAnnotate)
import Siza.Cli.Await (awaitBudgetParser, runAwaitIdle)
import Siza.Cli.Provenance (logToolCall)
import Siza.Cli.Retro (RetroTarget, retroTargetParser, runRetro)
import Siza.Discover (Server (..), defaultLocalUrl, discover, serverValue)
import Siza.HubToken (TokenStatus (..), statusForUrl)
import Siza.Language (
    Diagnostic,
    Severity (Error),
    dgSeverity,
    renderDiagnostic,
 )
import Siza.Login (runLogin, runLogout)
import Siza.Preflight (preflight, vettedSource)
import Siza.Provenance (Preflight (Preflight))
import Siza.Security (Policy, advisoryPolicy, scanSource, strictPolicy)
import Siza.Transport (Conn, Env (..), callTool, connEnv, getHealth, newConn)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

{- | Where @check@ reads its source: @-@ (or absent) is stdin, otherwise a
file path.
-}
data Source = Stdin | FromFile FilePath
    deriving (Show)

{- | The typed subcommand surface. 'Tool' carries a validated 'ToolName'
and the raw JSON input it forwards (typed inputs converge in a later phase).
'Check' runs the pre-flight parse on a cell source.
-}
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
    deriving (Show)

-- | The full parser, with @--help@ and per-subcommand help.
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

-- | The subcommand table: name, parser, one-line description.
subcommands :: [(String, Parser Command, String)]
subcommands =
    [ ("discover", pure Discover, "List live Sabela servers as JSON.")
    , ("health", pure Health, "Probe the first live server's health.")
    , ("tool", toolParser, "Invoke an AI tool: siza tool <name> [json].")
    , ("check", checkParser, "Pre-flight parse + scan: siza check [-|FILE].")
    , ("annotate", annotateParser, "Infer unsigned binds: siza annotate CELL_ID.")
    , ("retro", Retro <$> retroTargetParser, "Provenance metrics: siza retro [FILE].")
    ,
        ( "await-idle"
        , Await <$> awaitBudgetParser
        , "Block until idle: siza await-idle [SECONDS]."
        )
    , ("login", loginParser, "Authorize against a hub: siza login [HUB_URL].")
    , ("logout", pure Logout, "Forget the saved hub token.")
    ]

{- | @siza login [HUB_URL]@: run the browser-approved device flow against the
hub and save a short-lived token. The URL defaults to @SABELA_URL@.
-}
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

{- | @siza annotate CELL_ID [--source]@: read the cell, infer types for its
unsigned top-level binds from the live session, and print a report (or, with
@--source@, the annotated source).
-}
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

{- | @--strict@ selects 'Block' mode: a denied capability fails the check.
The default is advisory — findings are surfaced as warnings, exit stays zero.
-}
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

{- | Resolve a connection, dispatch the command, and print JSON. Exits
non-zero when a tool returns @isError@ or when no live server is found.
-}
runCommand :: Command -> IO ()
runCommand = \case
    Check src policy -> runCheck src policy
    Retro target -> runRetro target
    Logout -> rawConn runLogout
    Login mUrl -> rawConn $ \conn ->
        runLogin conn (fromMaybe defaultLocalUrl (mUrl <|> envSabelaUrl (connEnv conn)))
    Await budget ->
        withConn $ \conn -> withFirstServer conn $ \srv ->
            runAwaitIdle conn (srvBaseUrl srv) budget
    Annotate cellId asSource ->
        withConn $ \conn -> withFirst conn $ \base ->
            runAnnotate conn base cellId asSource
    Discover -> withConn $ \conn -> do
        servers <- discover conn
        printJson (A.Array (foldMap (pure . serverValue) servers))
    Health -> withConn $ \conn -> withFirst conn $ \base -> do
        mh <- getHealth conn base
        maybe (noServer "health") printJson mh
    Tool policy name input -> do
        mpf <- gateMutation policy name input
        withConn $ \conn -> withFirstServer conn $ \srv -> do
            let base = srvBaseUrl srv
            warnNonLocal base
            res <- callTool conn base name input
            logToolCall conn srv name input mpf res
            either fatal emitOutcome res

{- | A connection for the data commands: fails fast with a clear message when
the saved hub token for the target has expired, rather than sending an
unauthenticated request that the hub answers with the login page.
-}
withConn :: (Conn -> IO ()) -> IO ()
withConn k = do
    conn <- newConn
    guardHubAuth (connEnv conn)
    k conn

-- | A connection with no expiry guard, for @login@/@logout@ themselves.
rawConn :: (Conn -> IO ()) -> IO ()
rawConn k = newConn >>= k

guardHubAuth :: Env -> IO ()
guardHubAuth env
    | Nothing <- envToken env
    , Just url <- envSabelaUrl env = do
        st <- statusForUrl url
        case st of
            Expired -> do
                hPutStrLn
                    stderr
                    ("siza: hub token for " <> T.unpack url <> " expired; run 'siza login'.")
                exitFailure
            _ -> pure ()
    | otherwise = pure ()

{- | Run the 'preflight' parse + security gate over a mutation tool's source
before the call leaves the client, so the agent-facing @siza tool@ surface
exercises the same gate the typed 'Siza.Preflight' API enforces by
construction. A syntax error always blocks; a denied capability blocks only
under @--strict@ (advisory findings are surfaced and the call proceeds).
Returns the 'Preflight' verdict for the provenance log — the one thing the
server never sees. Non-mutation tools and a malformed @source@ field pass
straight through with no verdict.
-}
gateMutation :: Policy -> ToolName -> Value -> IO (Maybe Preflight)
gateMutation policy name input
    | name `elem` [ReplaceCellSource, InsertCell, ProposeEdit]
    , Just src <- sourceField input = do
        res <- preflight policy src
        case res of
            Left ds -> do
                emitDiagnostics ds
                hPutStrLn stderr ("siza: pre-flight blocked " <> show name)
                exitFailure
            Right v -> do
                let adv = advisories v
                emitDiagnostics adv
                pure (Just (Preflight True adv True))
    | otherwise = pure Nothing
  where
    advisories v = fromLeft [] (scanSource policy (vettedSource v))

-- | The @source@ string of a tool input, if present.
sourceField :: Value -> Maybe Text
sourceField = \case
    A.Object o -> case KM.lookup "source" o of
        Just (A.String s) -> Just s
        _ -> Nothing
    _ -> Nothing

{- | @siza check@: read a cell source, run the pre-flight parse and the
security scan, print every diagnostic, and exit non-zero when there is any
error — a syntax error, or (under @--strict@) a denied capability. Advisory
warnings are printed but keep the exit zero. No server needed.
-}
runCheck :: Source -> Policy -> IO ()
runCheck src policy = do
    txt <- case src of
        Stdin -> TIO.getContents
        FromFile p -> TIO.readFile p
    let ds = either id id (scanSource policy txt)
    emitDiagnostics ds
    if any isError ds then exitFailure else exitSuccess

isError :: Diagnostic -> Bool
isError d = dgSeverity d == Error

emitDiagnostics :: [Diagnostic] -> IO ()
emitDiagnostics = mapM_ (TIO.hPutStrLn stderr . renderDiagnostic)

emitOutcome :: ToolOutcome -> IO ()
emitOutcome o = do
    printJson (toolOutcomeValue o)
    if toolOutcomeIsError o then exitFailure else exitSuccess

withFirst :: Conn -> (Text -> IO ()) -> IO ()
withFirst conn k = withFirstServer conn (k . srvBaseUrl)

withFirstServer :: Conn -> (Server -> IO ()) -> IO ()
withFirstServer conn k = do
    servers <- discover conn
    case listToMaybe servers of
        Nothing -> noServer "no live server"
        Just s -> k s

warnNonLocal :: Text -> IO ()
warnNonLocal base
    | any
        (`T.isPrefixOf` base)
        ["http://localhost:", "http://127.0.0.1:", "http://[::1]:"] =
        pure ()
    | otherwise =
        hPutStrLn
            stderr
            ( "siza: sending data to non-localhost URL ("
                <> T.unpack base
                <> ") - ensure this is intentional."
            )

noServer :: String -> IO ()
noServer ctx = do
    hPutStrLn
        stderr
        ("siza: " <> ctx <> ": no live Sabela server. Set SABELA_URL or start sabela.")
    exitFailure

fatal :: Text -> IO ()
fatal e = hPutStrLn stderr ("siza: " <> T.unpack e) >> exitFailure

printJson :: Value -> IO ()
printJson = LBS8.putStrLn . A.encode

-- | Parse argv and run.
main :: IO ()
main = execParser parseCommand >>= runCommand
