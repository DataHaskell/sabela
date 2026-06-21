{- | Typed HTTP transport to a running Sabela server's @/api/ai/*@ surface.

Speaks the same wire shape the bash @siza-tool.sh@ did — @POST /api/ai/tool@
with body @{name, input}@ and a typed decode of @{isError, result}@ into
'Sabela.AI.Types.ToolOutcome' — but in typed Haskell over http-client.
-}
module Siza.Transport (
    Conn (..),
    Env (..),
    resolveEnv,
    newConn,
    callTool,
    getHealth,
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
import Network.HTTP.Client (
    Manager,
    Request (..),
    RequestBody (RequestBodyLBS),
    Response (..),
    httpLbs,
    parseRequest,
    responseTimeoutMicro,
 )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Header (Header)
import Sabela.AI.Capabilities.ToolName (ToolName, toolWireName)
import Sabela.AI.Types (ToolOutcome (..))
import Siza.HubToken (TokenStatus (..), statusForUrl)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

-- | Process-level configuration read once from the environment.
data Env = Env
    { envSabelaUrl :: Maybe Text
    , envToken :: Maybe Text
    , envSession :: Text
    , envCookie :: Maybe Text
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
    hostName <- fromMaybe "host" . nonEmpty <$> lookupEnv "HOSTNAME"
    ppid <- fromMaybe "0" . nonEmpty <$> lookupEnv "PPID"
    let session = fromMaybe ("siza-" <> hostName <> "-" <> ppid) msess
    pure
        Env
            { envSabelaUrl = murl
            , envToken = mtok
            , envSession = session
            , envCookie = mcookie
            }
  where
    nonEmpty = fmap T.pack . (>>= \s -> if null s then Nothing else Just s)

newConn :: IO Conn
newConn = do
    mgr <- newTlsManager
    env <- resolveEnv >>= attachHubToken
    pure (Conn mgr env)

{- | Attach a saved @siza login@ token as the bearer when the target is a hub
('SABELA_URL') and no explicit 'SABELA_AI_TOKEN' was given. An expired token
for that hub prints a hint; an explicit token always wins.
-}
attachHubToken :: Env -> IO Env
attachHubToken env
    | Just _ <- envToken env = pure env
    | Just url <- envSabelaUrl env = do
        st <- statusForUrl url
        case st of
            Valid t -> pure env{envToken = Just t}
            Expired -> do
                hPutStrLn stderr "siza: saved hub token expired; run 'siza login' to refresh."
                pure env
            NoToken -> pure env
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

mkGet :: Env -> Text -> IO Request
mkGet env url = do
    req <- parseRequest (T.unpack url)
    pure req{method = "GET", requestHeaders = aiHeaders env}

{- | @POST base/api/ai/tool@ with body @{name, input}@, decoding the
@{isError, result}@ envelope into a typed 'ToolOutcome'. 60-second timeout,
matching the bash tool client.
-}
callTool :: Conn -> Text -> ToolName -> Value -> IO (Either Text ToolOutcome)
callTool conn base name input = do
    let env = connEnv conn
        body = object ["name" .= toolWireName name, "input" .= input]
    er <-
        try (parseRequest (T.unpack (base <> "/api/ai/tool"))) ::
            IO (Either SomeException Request)
    case er of
        Left e -> pure (Left (T.pack (show e)))
        Right req0 -> do
            let req =
                    req0
                        { method = "POST"
                        , requestHeaders = aiHeaders env
                        , requestBody = RequestBodyLBS (A.encode body)
                        , responseTimeout = responseTimeoutMicro 60000000
                        }
            res <-
                try (httpLbs req (connManager conn)) ::
                    IO (Either SomeException (Response LBS.ByteString))
            pure $ case res of
                Left e -> Left (T.pack (show e))
                Right r -> decodeOutcome (responseBody r)

-- | Decode the @{isError, result}@ envelope into a 'ToolOutcome'.
decodeOutcome :: LBS.ByteString -> Either Text ToolOutcome
decodeOutcome raw = case eitherDecode raw of
    Left e -> Left (T.pack e <> ": " <> TE.decodeUtf8 (LBS.toStrict raw))
    Right (A.Object o) ->
        let isErr = case KM.lookup "isError" o of
                Just (A.Bool b) -> b
                _ -> False
            result = fromMaybe (A.object []) (KM.lookup "result" o)
         in Right (if isErr then ToolErr result else ToolOk result)
    Right v -> Right (ToolOk v)
