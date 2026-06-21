{-# LANGUAGE NumericUnderscores #-}

{- | The @siza login@ device-authorization client.

Mirrors 'Hub.CliAuth': POST @start@ to get a @deviceCode@/@userCode@, open the
browser at the authorize page, then poll until the user approves and the hub
returns a short-lived token, which is saved via "Siza.HubToken". No cookie
copy-paste — the only manual step is clicking Approve in a browser already
logged into the hub.
-}
module Siza.Login (
    runLogin,
    runLogout,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), eitherDecode, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (addUTCTime, getCurrentTime)
import Network.HTTP.Client (
    Request (..),
    RequestBody (RequestBodyLBS),
    Response (..),
    httpLbs,
    parseRequest,
    responseTimeoutMicro,
 )
import Siza.HubToken (HubToken (..), clearHubToken, saveHubToken)
import Siza.Transport (Conn (..))
import System.Exit (exitFailure)
import System.Info (os)
import System.Process (callProcess)

{- | Run the device-authorization flow against @hubUrl@: start, open the
browser, poll to approval, and persist the token. Exits non-zero on failure.
-}
runLogin :: Conn -> Text -> IO ()
runLogin conn hubUrl = do
    let base = T.dropWhileEnd (== '/') hubUrl
    est <- postJson conn (base <> "/_hub/cli-auth/start") (object [])
    case est of
        Left e -> die ("could not reach the hub at " <> base <> ": " <> e)
        Right v -> case (strField "deviceCode" v, strField "userCode" v) of
            (Just device, Just user) -> approveAndSave conn base v device user
            _ -> die "unexpected response from the hub's cli-auth/start"

approveAndSave :: Conn -> Text -> Value -> Text -> Text -> IO ()
approveAndSave conn base v device user = do
    let interval = intField "interval" 2 v
        expiresIn = intField "expiresIn" 300 v
        url = base <> "/_hub/cli-auth?code=" <> user
    TIO.putStrLn
        ("To authorize siza, approve this request in your browser (code " <> user <> "):")
    TIO.putStrLn ("  " <> url)
    openBrowser url
    TIO.putStrLn "Waiting for approval..."
    res <- pollLoop conn base device interval expiresIn
    case res of
        Left e -> die e
        Right (token, ttl) -> do
            now <- getCurrentTime
            saveHubToken (HubToken base token (addUTCTime (fromIntegral ttl) now))
            TIO.putStrLn
                ( "Authorized. Token saved; expires in ~"
                    <> T.pack (show (ttl `div` 60))
                    <> " min. siza now drives "
                    <> base
                    <> " when SABELA_URL points there."
                )

-- | Poll @cli-auth/poll@ every @interval@ seconds until approval or timeout.
pollLoop :: Conn -> Text -> Text -> Int -> Int -> IO (Either Text (Text, Int))
pollLoop conn base device interval remaining
    | remaining <= 0 = pure (Left "authorization timed out; re-run siza login")
    | otherwise = do
        threadDelay (interval * 1_000_000)
        res <-
            postJson conn (base <> "/_hub/cli-auth/poll") (object ["deviceCode" .= device])
        case res of
            Left _ -> again
            Right v -> case strField "status" v of
                Just "approved" -> case strField "token" v of
                    Just tok -> pure (Right (tok, intField "expiresIn" 28_800 v))
                    Nothing -> pure (Left "hub approved but returned no token")
                Just "pending" -> again
                _ -> pure (Left "authorization request expired; re-run siza login")
  where
    again = pollLoop conn base device interval (remaining - interval)

-- | Forget the saved token. The hub-side token expires on its own TTL.
runLogout :: IO ()
runLogout = do
    clearHubToken
    TIO.putStrLn "Logged out: local siza token cleared."

-- ---------------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------------

{- | Open a URL in the user's browser, cross-platform. Best-effort: on failure
the URL is already printed above for the user to open by hand.
-}
openBrowser :: Text -> IO ()
openBrowser url = do
    let u = T.unpack url
        (cmd, args) = case os of
            "darwin" -> ("open", [u])
            "mingw32" -> ("cmd", ["/c", "start", "", u])
            _ -> ("xdg-open", [u])
    _ <- try (callProcess cmd args) :: IO (Either SomeException ())
    pure ()

-- | A JSON POST returning the decoded body. 15s timeout (these are quick).
postJson :: Conn -> Text -> Value -> IO (Either Text Value)
postJson conn url payload = do
    er <- try (parseRequest (T.unpack url)) :: IO (Either SomeException Request)
    case er of
        Left e -> pure (Left (T.pack (show e)))
        Right req0 -> do
            let req =
                    req0
                        { method = "POST"
                        , requestHeaders = [("content-type", "application/json")]
                        , requestBody = RequestBodyLBS (encode payload)
                        , responseTimeout = responseTimeoutMicro 15_000_000
                        }
            res <-
                try (httpLbs req (connManager conn)) ::
                    IO (Either SomeException (Response LBS.ByteString))
            pure $ case res of
                Left e -> Left (T.pack (show e))
                Right r -> either (Left . T.pack) Right (eitherDecode (responseBody r))

strField :: Text -> Value -> Maybe Text
strField k v = case v of
    Object o -> case KM.lookup (Key.fromText k) o of
        Just (String s) -> Just s
        _ -> Nothing
    _ -> Nothing

intField :: Text -> Int -> Value -> Int
intField k def v = case v of
    Object o -> case KM.lookup (Key.fromText k) o of
        Just (Number n) -> round n
        _ -> def
    _ -> def

die :: Text -> IO ()
die e = TIO.putStrLn ("siza: " <> e) >> exitFailure
