module Siza.Cli (
    Command (..),
    parseCommand,
    runCommand,
    haskellCodePayload,
    main,
) where

import Data.Aeson (Value)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import Sabela.AI.Capabilities.ToolName (
    ToolName (InsertCell, ProposeEdit, ReplaceCellSource),
 )
import Sabela.AI.Types (
    ToolOutcome,
    toolOutcomeIsError,
    toolOutcomeValue,
 )
import Siza.Agent.Tools (toolSurfaceHelp)
import Siza.Cli.Annotate (runAnnotate)
import Siza.Cli.Await (runAwaitIdle)
import Siza.Cli.Chat (runChatCommand)
import Siza.Cli.Options (Command (..), Source (..), parseCommand)
import Siza.Cli.Provenance (logToolCall)
import Siza.Cli.Retro (runRetro)
import Siza.Discover (Server (..), defaultLocalUrl, discover, serverValue)
import Siza.HubToken (TokenStatus (..), statusForUrl)
import Siza.Language (
    Diagnostic,
    Severity (Error),
    dgSeverity,
    renderDiagnostic,
 )
import Siza.Login (runLogin, runLogout)
import Siza.Mcp (runMcp)
import Siza.Preflight (preflight, vettedSource)
import Siza.Provenance (Preflight (Preflight))
import Siza.Security (Policy, scanSource)
import Siza.Transport (Conn, Env (..), callTool, connEnv, getHealth, newConn)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

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
    Tools -> TIO.putStr toolSurfaceHelp
    Mcp ->
        withConn $ \conn -> withFirstServer conn $ \srv ->
            runMcp conn (srvBaseUrl srv)
    Chat opts ->
        runChatCommand opts withConn resolveChatBase warnNonLocal noServer
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

resolveChatBase :: Conn -> Maybe Text -> (Text -> IO ()) -> IO ()
resolveChatBase _ (Just u) k = k u
resolveChatBase conn Nothing k = withFirst conn k

withConn :: (Conn -> IO ()) -> IO ()
withConn k = do
    conn <- newConn
    guardHubAuth (connEnv conn)
    k conn

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

gateMutation :: Policy -> ToolName -> Value -> IO (Maybe Preflight)
gateMutation policy name input
    | name `elem` [ReplaceCellSource, InsertCell, ProposeEdit]
    , haskellCodePayload input
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
    advisories v = either id (const []) (scanSource policy (vettedSource v))

sourceField :: Value -> Maybe Text
sourceField = \case
    A.Object o -> case KM.lookup "source" o of
        Just (A.String s) -> Just s
        _ -> Nothing
    _ -> Nothing

{- | Only a Haskell code payload faces the Haskell parser: the wire carries
the cell's declared type, and parsing prose or Python as Haskell blocks
every valid insert. Absent fields default to code, the server's default.
-}
haskellCodePayload :: Value -> Bool
haskellCodePayload v =
    stringField "cell_type" v /= Just "ProseCell"
        && maybe True (== "Haskell") (stringField "language" v)

stringField :: Text -> Value -> Maybe Text
stringField k = \case
    A.Object o -> case KM.lookup (Key.fromText k) o of
        Just (A.String s) -> Just s
        _ -> Nothing
    _ -> Nothing

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

main :: IO ()
main = execParser parseCommand >>= runCommand
