{- | Typed HTTP transport to a running Sabela server's @/api/ai/*@ surface.

Speaks the same wire shape the bash @siza-tool.sh@ did — @POST /api/ai/tool@
with body @{name, input}@ and a typed decode of @{isError, result}@ into
'Sabela.AI.Types.ToolOutcome' — but in typed Haskell over http-client.
-}
module Siza.Transport (
    Conn (..),
    Env (..),
    resolveEnv,
    applyUrlOverride,
    newConn,
    callTool,
    getHealth,
    getTools,
    aiHeaders,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value, eitherDecode, object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Network.HTTP.Client (
    Manager,
    Request (..),
    RequestBody (RequestBodyLBS),
    Response (..),
    ResponseTimeout,
    httpLbs,
    parseRequest,
    responseTimeoutMicro,
    responseTimeoutNone,
 )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Status (statusCode)
import Sabela.AI.Capabilities.ToolName (ToolName, toolWireName)
import Sabela.AI.Types (ToolOutcome (..))
import Siza.HubToken (TokenStatus (..), statusForUrl)
import Siza.Transport.Failure (
    classifyDecode,
    classifyStatus,
    classifyTransport,
    renderFailure,
 )
import System.Environment (lookupEnv, setEnv)
import Text.Read (readMaybe)

-- | Process-level configuration read once from the environment.
data Env = Env
    { envSabelaUrl :: Maybe Text
    , envToken :: Maybe Text
    , envSession :: Text
    , envCookie :: Maybe Text
    , envToolTimeout :: Int
    }
    deriving (Show)

-- | A live connection: the shared TLS manager plus the resolved 'Env'.
data Conn = Conn
    { connManager :: Manager
    , connEnv :: Env
    }

{- | Read @SABELA_URL@/@SABELA_AI_TOKEN@/@SABELA_SESSION@/@SABELA_COOKIE@.
The session defaults to a stable per-terminal id. @SABELA_COOKIE@ carries the
hub's @_sabela_session@ OAuth cookie for driving a notebook hosted behind the
hub (e.g. sabela.datahaskell.com), where the bearer token never reaches the
backend.
-}
resolveEnv :: IO Env
resolveEnv = do
    murl <- nonEmpty <$> lookupEnv "SABELA_URL"
    mtok <- nonEmpty <$> lookupEnv "SABELA_AI_TOKEN"
    msess <- nonEmpty <$> lookupEnv "SABELA_SESSION"
    mcookie <- nonEmpty <$> lookupEnv "SABELA_COOKIE"
    mtimeout <- (>>= readMaybe) <$> lookupEnv "SABELA_TOOL_TIMEOUT"
    hostName <- fromMaybe "host" . nonEmpty <$> lookupEnv "HOSTNAME"
    ppid <- fromMaybe "0" . nonEmpty <$> lookupEnv "PPID"
    let session = fromMaybe ("siza-" <> hostName <> "-" <> ppid) msess
    pure
        Env
            { envSabelaUrl = murl
            , envToken = mtok
            , envSession = session
            , envCookie = mcookie
            , envToolTimeout = fromMaybe 60 mtimeout
            }
  where
    nonEmpty = fmap T.pack . (>>= \s -> if null s then Nothing else Just s)

{- | Make an explicit @--url@ the process @SABELA_URL@, so flag and env are ONE
knob: discovery and the hub-token attach both key on 'envSabelaUrl', which only
'resolveEnv' fills. Run before 'newConn'; 'Nothing' leaves the env untouched.
-}
applyUrlOverride :: Maybe Text -> IO ()
applyUrlOverride = mapM_ (setEnv "SABELA_URL" . T.unpack)

newConn :: IO Conn
newConn = do
    mgr <- newTlsManager
    env <- resolveEnv >>= attachHubToken
    pure (Conn mgr env)

{- | Attach a saved @siza login@ token as the bearer when the target is a hub
('SABELA_URL') and no explicit 'SABELA_AI_TOKEN' was given. An explicit token
always wins. An expired token is left unattached; the data-command path
('Siza.Cli') turns that into a clear "run siza login" error rather than a
silent unauthenticated request.
-}
attachHubToken :: Env -> IO Env
attachHubToken env
    | Just _ <- envToken env = pure env
    | Just url <- envSabelaUrl env = do
        st <- statusForUrl url
        pure $ case st of
            Valid t -> env{envToken = Just t}
            _ -> env
    | otherwise = pure env

{- | Common request headers: content type, session, an optional bearer token
(localhost trust model), and an optional @Cookie:@ (hub trust model). The hub
strips @Authorization@ but not @X-Sabela-Session@, so the session header always
flows through; the cookie is what authenticates a hub-hosted notebook.
-}
aiHeaders :: Env -> [Header]
aiHeaders env =
    ("content-type", "application/json")
        : ("X-Sabela-Session", TE.encodeUtf8 (envSession env))
        : maybe
            []
            (\t -> [("Authorization", "Bearer " <> TE.encodeUtf8 t)])
            (envToken env)
            <> maybe [] (\c -> [("Cookie", TE.encodeUtf8 c)]) (envCookie env)

{- | @GET base/api/ai/health@. 'Nothing' if the server is unreachable or the
body is not the expected JSON object; 2-second timeout, like the bash probe.
-}
getHealth :: Conn -> Text -> IO (Maybe Value)
getHealth conn base = do
    let env = connEnv conn
    er <-
        try (mkGet env (base <> "/api/ai/health")) ::
            IO (Either SomeException Request)
    case er of
        Left _ -> pure Nothing
        Right req0 -> do
            let req = req0{responseTimeout = responseTimeoutMicro 2000000}
            res <-
                try (httpLbs req (connManager conn)) ::
                    IO (Either SomeException (Response LBS.ByteString))
            pure $ case res of
                Left _ -> Nothing
                Right r -> either (const Nothing) Just (eitherDecode (responseBody r))

{- | @GET base/api/ai/tools@ → the @[ToolDef]@ catalogue as a raw JSON array,
the source of truth an MCP @tools/list@ projects from. 'Nothing' on any error;
10-second timeout.
-}
getTools :: Conn -> Text -> IO (Maybe Value)
getTools conn base = do
    let env = connEnv conn
    er <-
        try (mkGet env (base <> "/api/ai/tools")) ::
            IO (Either SomeException Request)
    case er of
        Left _ -> pure Nothing
        Right req0 -> do
            let req = req0{responseTimeout = responseTimeoutMicro 10000000}
            res <-
                try (httpLbs req (connManager conn)) ::
                    IO (Either SomeException (Response LBS.ByteString))
            pure $ case res of
                Left _ -> Nothing
                Right r -> either (const Nothing) Just (eitherDecode (responseBody r))

mkGet :: Env -> Text -> IO Request
mkGet env url = do
    req <- parseRequest (T.unpack url)
    pure req{method = "GET", requestHeaders = aiHeaders env}

-- | A tool-call response timeout: @0@ (or negative) means no timeout.
toolTimeout :: Int -> ResponseTimeout
toolTimeout n
    | n <= 0 = responseTimeoutNone
    | otherwise = responseTimeoutMicro (n * 1000000)

{- | @POST base/api/ai/tool@ with body @{name, input}@, decoding the
@{isError, result}@ envelope into a typed 'ToolOutcome'. Failures render
through the single 'Siza.Transport.Failure' envelope: non-2xx statuses are
classified BEFORE any payload decode (R6.9), and exception records never
cross the surface raw (R6.3).
-}
callTool :: Conn -> Text -> ToolName -> Value -> IO (Either Text ToolOutcome)
callTool conn base name input = do
    let env = connEnv conn
        infra = renderFailure . classifyTransport (envToolTimeout env)
        body = object ["name" .= toolWireName name, "input" .= input]
    er <-
        try (parseRequest (T.unpack (base <> "/api/ai/tool"))) ::
            IO (Either SomeException Request)
    case er of
        Left e -> pure (Left (infra e))
        Right req0 -> do
            let req =
                    req0
                        { method = "POST"
                        , requestHeaders = aiHeaders env
                        , requestBody = RequestBodyLBS (A.encode body)
                        , responseTimeout = toolTimeout (envToolTimeout env)
                        }
            res <-
                try (httpLbs req (connManager conn)) ::
                    IO (Either SomeException (Response LBS.ByteString))
            pure $ case res of
                Left e -> Left (infra e)
                Right r
                    | ok (statusCode (responseStatus r)) ->
                        decodeOutcome (responseBody r)
                    | otherwise ->
                        Left
                            ( renderFailure
                                (classifyStatus (statusCode (responseStatus r)))
                            )
  where
    ok sc = sc >= 200 && sc < 300

-- | Decode the @{isError, result}@ envelope into a 'ToolOutcome'.
decodeOutcome :: LBS.ByteString -> Either Text ToolOutcome
decodeOutcome raw = case eitherDecode raw of
    Left _ ->
        Left
            ( renderFailure
                ( classifyDecode
                    (TE.decodeUtf8With TEE.lenientDecode (LBS.toStrict raw))
                )
            )
    Right (A.Object o) ->
        let isErr = case KM.lookup "isError" o of
                Just (A.Bool b) -> b
                _ -> False
            result = fromMaybe (A.object []) (KM.lookup "result" o)
         in Right (if isErr then ToolErr result else ToolOk result)
    Right v -> Right (ToolOk v)
