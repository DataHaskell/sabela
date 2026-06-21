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
import System.Environment (lookupEnv)

-- | Process-level configuration read once from the environment.
data Env = Env
    { envSabelaUrl :: Maybe Text
    , envToken :: Maybe Text
    , envSession :: Text
    }
    deriving (Show)

-- | A live connection: the shared TLS manager plus the resolved 'Env'.
data Conn = Conn
    { connManager :: Manager
    , connEnv :: Env
    }

{- | Read @SABELA_URL@/@SABELA_AI_TOKEN@/@SABELA_SESSION@ exactly as the
bash scripts did; the session defaults to a stable per-terminal id.
-}
resolveEnv :: IO Env
resolveEnv = do
    murl <- nonEmpty <$> lookupEnv "SABELA_URL"
    mtok <- nonEmpty <$> lookupEnv "SABELA_AI_TOKEN"
    msess <- nonEmpty <$> lookupEnv "SABELA_SESSION"
    hostName <- fromMaybe "host" . nonEmpty <$> lookupEnv "HOSTNAME"
    ppid <- fromMaybe "0" . nonEmpty <$> lookupEnv "PPID"
    let session = fromMaybe ("siza-" <> hostName <> "-" <> ppid) msess
    pure Env{envSabelaUrl = murl, envToken = mtok, envSession = session}
  where
    nonEmpty = fmap T.pack . (>>= \s -> if null s then Nothing else Just s)

newConn :: IO Conn
newConn = Conn <$> newTlsManager <*> resolveEnv

-- | Common request headers: content type, session, and optional bearer token.
aiHeaders :: Env -> [Header]
aiHeaders env =
    ("content-type", "application/json")
        : ("X-Sabela-Session", TE.encodeUtf8 (envSession env))
        : maybe
            []
            (\t -> [("Authorization", "Bearer " <> TE.encodeUtf8 t)])
            (envToken env)

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
