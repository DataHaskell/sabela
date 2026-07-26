{-# LANGUAGE NumericUnderscores #-}

module Siza.Transport (
    defaultToolTimeoutSecs,
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
import Sabela.Session.Timeout (TimeoutConfig (..), defaultTimeoutConfig)
import Siza.HubToken (TokenStatus (..), statusForUrl)
import Siza.Transport.Failure (
    classifyDecode,
    classifyStatus,
    classifyTransport,
    renderFailure,
 )
import System.Environment (lookupEnv, setEnv)
import Text.Read (readMaybe)

data Env = Env
    { envSabelaUrl :: Maybe Text
    , envToken :: Maybe Text
    , envSession :: Text
    , envCookie :: Maybe Text
    , envToolTimeout :: Int
    }
    deriving (Show)

data Conn = Conn
    { connManager :: Manager
    , connEnv :: Env
    }

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
            , envToolTimeout = fromMaybe defaultToolTimeoutSecs mtimeout
            }
  where
    nonEmpty = fmap T.pack . (>>= \s -> if null s then Nothing else Just s)

defaultToolTimeoutSecs :: Int
defaultToolTimeoutSecs = serverCeilingSecs + transportMarginSecs
  where
    tc = defaultTimeoutConfig
    serverCeilingSecs =
        usToSecs (tcBuildUs tc + tcExecutionUs tc + tcResyncUs tc)
    usToSecs us = (us + 999_999) `div` 1_000_000

transportMarginSecs :: Int
transportMarginSecs = 60

applyUrlOverride :: Maybe Text -> IO ()
applyUrlOverride = mapM_ (setEnv "SABELA_URL" . T.unpack)

newConn :: IO Conn
newConn = do
    mgr <- newTlsManager
    env <- resolveEnv >>= attachHubToken
    pure (Conn mgr env)

attachHubToken :: Env -> IO Env
attachHubToken env
    | Just _ <- envToken env = pure env
    | Just url <- envSabelaUrl env = do
        st <- statusForUrl url
        pure $ case st of
            Valid t -> env{envToken = Just t}
            _ -> env
    | otherwise = pure env

aiHeaders :: Env -> [Header]
aiHeaders env =
    ("content-type", "application/json")
        : ("X-Sabela-Session", TE.encodeUtf8 (envSession env))
        : maybe
            []
            (\t -> [("Authorization", "Bearer " <> TE.encodeUtf8 t)])
            (envToken env)
            <> maybe [] (\c -> [("Cookie", TE.encodeUtf8 c)]) (envCookie env)

getHealth :: Conn -> Text -> IO (Maybe Value)
getHealth conn base = do
    let env = connEnv conn
    er <-
        try (mkGet env (base <> "/api/ai/health")) ::
            IO (Either SomeException Request)
    case er of
        Left _ -> pure Nothing
        Right req0 -> do
            let req = req0{responseTimeout = responseTimeoutMicro 2_000_000}
            res <-
                try (httpLbs req (connManager conn)) ::
                    IO (Either SomeException (Response LBS.ByteString))
            pure $ case res of
                Left _ -> Nothing
                Right r -> either (const Nothing) Just (eitherDecode (responseBody r))

getTools :: Conn -> Text -> IO (Maybe Value)
getTools conn base = do
    let env = connEnv conn
    er <-
        try (mkGet env (base <> "/api/ai/tools")) ::
            IO (Either SomeException Request)
    case er of
        Left _ -> pure Nothing
        Right req0 -> do
            let req = req0{responseTimeout = responseTimeoutMicro 10_000_000}
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

toolTimeout :: Int -> ResponseTimeout
toolTimeout n
    | n <= 0 = responseTimeoutNone
    | otherwise = responseTimeoutMicro (n * 1_000_000)

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
