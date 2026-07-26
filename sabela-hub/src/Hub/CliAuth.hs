{-# LANGUAGE OverloadedStrings #-}

module Hub.CliAuth (
    CliAuth,
    newCliAuth,
    resolveCliToken,
    revokeSessionTokens,
    handleCliStart,
    handleCliPoll,
    handleCliApprove,
    handleCliRevoke,
    cliAuthPage,
) where

import Control.Concurrent.STM (
    TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVar,
    readTVarIO,
 )
import Data.Aeson (Value (..), decode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as B8
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (
    NominalDiffTime,
    UTCTime,
    addUTCTime,
    diffUTCTime,
    getCurrentTime,
 )
import Hub.Auth (extractSessionId)
import Hub.CliAuth.Page (authorizePage, noticePage)
import Hub.OAuth (generateRandomToken)
import Hub.Pages (jsonError, jsonResponse)
import Hub.Types (HubConfig (..), SessionId (..))
import Network.HTTP.Types
import Network.Wai

data Pending = Pending
    { pUserCode :: Text
    , pCreated :: UTCTime
    , pToken :: Maybe Text
    }

data CliAuth = CliAuth
    { caPending :: TVar (Map.Map Text Pending)
    , caTokens :: TVar (Map.Map Text (SessionId, UTCTime))
    , caCsrf :: TVar (Map.Map Text (SessionId, UTCTime))
    , caTtl :: NominalDiffTime
    , caOrigin :: Text
    }

newCliAuth :: HubConfig -> IO CliAuth
newCliAuth cfg =
    CliAuth
        <$> newTVarIO Map.empty
        <*> newTVarIO Map.empty
        <*> newTVarIO Map.empty
        <*> pure (hcCliTokenTtl cfg)
        <*> pure (originFromRedirect (hcGoogleRedirectUri cfg))

requestTtl :: NominalDiffTime
requestTtl = 300

pollInterval :: Int
pollInterval = 2

maxPending :: Int
maxPending = 1000

resolveCliToken :: CliAuth -> Request -> IO (Maybe SessionId)
resolveCliToken ca req =
    case bearerToken req of
        Nothing -> pure Nothing
        Just tok -> do
            now <- getCurrentTime
            atomically $ do
                toks <- readTVar (caTokens ca)
                modifyTVar' (caTokens ca) (Map.filter ((> now) . snd))
                pure $ case Map.lookup tok toks of
                    Just (sid, expiry) | expiry > now -> Just sid
                    _ -> Nothing

revokeSessionTokens :: CliAuth -> SessionId -> IO ()
revokeSessionTokens ca sid =
    atomically $ modifyTVar' (caTokens ca) (Map.filter ((/= sid) . fst))

bearerToken :: Request -> Maybe Text
bearerToken req = do
    raw <- lookup hAuthorization (requestHeaders req)
    let (scheme, rest) = B8.break (== ' ') raw
    if T.toLower (TE.decodeUtf8 scheme) == "bearer"
        then nonEmpty (TE.decodeUtf8 (B8.dropWhile (== ' ') rest))
        else Nothing
  where
    nonEmpty t = if T.null t then Nothing else Just t

handleCliStart :: CliAuth -> Application
handleCliStart ca req respond = do
    now <- getCurrentTime
    full <- atomically $ do
        modifyTVar' (caPending ca) (Map.filter (notExpired now))
        (>= maxPending) . Map.size <$> readTVar (caPending ca)
    if full
        then
            respond $ jsonError status429 "Too many pending authorizations; retry shortly."
        else do
            deviceCode <- generateRandomToken
            userCode <- freshUserCode ca
            atomically $
                modifyTVar'
                    (caPending ca)
                    (Map.insert deviceCode (Pending userCode now Nothing))
            let origin = if T.null (caOrigin ca) then originOf req else caOrigin ca
            respond $
                jsonResponse status200 $
                    object
                        [ "deviceCode" .= deviceCode
                        , "userCode" .= userCode
                        , "verificationUri" .= (origin <> "/_hub/cli-auth")
                        , "interval" .= pollInterval
                        , "expiresIn" .= (round requestTtl :: Int)
                        ]

freshUserCode :: CliAuth -> IO Text
freshUserCode ca = do
    c <- T.toUpper . T.take 12 <$> generateRandomToken
    pend <- readTVarIO (caPending ca)
    if any ((== c) . pUserCode) (Map.elems pend) then freshUserCode ca else pure c

handleCliPoll :: CliAuth -> Application
handleCliPoll ca req respond = do
    body <- strictRequestBody req
    now <- getCurrentTime
    case decode body >>= strField "deviceCode" of
        Nothing -> respond $ jsonError status400 "Expected {deviceCode}."
        Just deviceCode -> do
            pend <- readTVarIO (caPending ca)
            case Map.lookup deviceCode pend of
                Just p
                    | notExpired now p -> case pToken p of
                        Just tok -> do
                            atomically $
                                modifyTVar' (caPending ca) (Map.delete deviceCode)
                            respond $
                                jsonResponse status200 $
                                    object
                                        [ "status" .= ("approved" :: Text)
                                        , "token" .= tok
                                        , "expiresIn" .= (round (caTtl ca) :: Int)
                                        ]
                        Nothing ->
                            respond $
                                jsonResponse status200 (object ["status" .= ("pending" :: Text)])
                _ -> respond $ jsonResponse status200 (object ["status" .= ("expired" :: Text)])

cliAuthPage :: CliAuth -> Application
cliAuthPage ca req respond =
    case extractSessionId req of
        Nothing -> respond $ noticePage status401 "Not signed in."
        Just sid -> do
            now <- getCurrentTime
            csrf <- generateRandomToken
            atomically $
                modifyTVar' (caCsrf ca) $
                    Map.insert csrf (sid, now) . Map.filter (fresh now)
            respond (authorizePage csrf)

handleCliApprove :: CliAuth -> Application
handleCliApprove ca req respond = do
    body <- strictRequestBody req
    now <- getCurrentTime
    case (extractSessionId req, decode body) of
        (Just sid, Just v)
            | Just userCode <- strField "userCode" v
            , Just csrf <- strField "csrf" v -> do
                token <- generateRandomToken
                ok <- atomically $ do
                    csrfs <- readTVar (caCsrf ca)
                    pend <- readTVar (caPending ca)
                    let csrfOk = case Map.lookup csrf csrfs of
                            Just (s, t) -> s == sid && fresh now (s, t)
                            Nothing -> False
                    case findByUserCode userCode pend of
                        Just (dc, p)
                            | csrfOk
                            , notExpired now p -> do
                                modifyTVar' (caCsrf ca) (Map.delete csrf)
                                modifyTVar' (caPending ca) $
                                    Map.insert dc p{pToken = Just token}
                                modifyTVar' (caTokens ca) $
                                    Map.insert token (sid, addUTCTime (caTtl ca) now)
                                pure True
                        _ -> pure False
                respond $
                    if ok
                        then jsonResponse status200 (object ["status" .= ("approved" :: Text)])
                        else jsonError status410 "This authorization request has expired."
        _ -> respond $ jsonError status400 "Expected {userCode, csrf}."

handleCliRevoke :: CliAuth -> Application
handleCliRevoke ca req respond =
    case bearerToken req of
        Nothing -> respond $ jsonError status400 "Expected a bearer token."
        Just tok -> do
            atomically $ modifyTVar' (caTokens ca) (Map.delete tok)
            respond $ jsonResponse status200 (object ["revoked" .= True])

notExpired :: UTCTime -> Pending -> Bool
notExpired now p = diffUTCTime now (pCreated p) < requestTtl

fresh :: UTCTime -> (a, UTCTime) -> Bool
fresh now (_, t) = diffUTCTime now t < requestTtl

findByUserCode :: Text -> Map.Map Text Pending -> Maybe (Text, Pending)
findByUserCode uc = find ((== uc) . pUserCode . snd) . Map.toList

strField :: Text -> Value -> Maybe Text
strField k v = case v of
    Object o -> case KM.lookup (Key.fromText k) o of
        Just (String s) -> Just s
        _ -> Nothing
    _ -> Nothing

originFromRedirect :: Text -> Text
originFromRedirect uri = case T.splitOn "/" uri of
    (scheme : "" : host : _) | not (T.null host) -> scheme <> "//" <> host
    _ -> ""

originOf :: Request -> Text
originOf req =
    scheme <> "://" <> maybe "localhost" TE.decodeUtf8 (requestHeaderHost req)
  where
    scheme
        | lookup "X-Forwarded-Proto" (requestHeaders req) == Just "https" = "https"
        | otherwise = "http"
