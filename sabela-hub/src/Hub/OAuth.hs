{-# LANGUAGE OverloadedStrings #-}

module Hub.OAuth (
    googleAuthUrl,
    exchangeCodeForEmail,
    generateRandomToken,
) where

import Data.Aeson (Value (..), decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hub.Types
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (urlEncode)
import qualified System.IO

-- | Build the Google OAuth authorization URL.
googleAuthUrl :: HubConfig -> Text -> Text
googleAuthUrl cfg state =
    "https://accounts.google.com/o/oauth2/v2/auth?"
        <> "client_id="
        <> hcGoogleClientId cfg
        <> "&redirect_uri="
        <> encodeParam (hcGoogleRedirectUri cfg)
        <> "&response_type=code"
        <> "&scope=email%20profile"
        <> "&state="
        <> state
        <> "&prompt=select_account"

-- | Exchange an authorization code for the user's email.
exchangeCodeForEmail :: HC.Manager -> HubConfig -> Text -> IO (Either Text Text)
exchangeCodeForEmail mgr cfg code = do
    let body =
            B8.intercalate
                "&"
                [ "code=" <> TE.encodeUtf8 code
                , "client_id=" <> TE.encodeUtf8 (hcGoogleClientId cfg)
                , "client_secret=" <> TE.encodeUtf8 (hcGoogleClientSecret cfg)
                , "redirect_uri=" <> TE.encodeUtf8 (hcGoogleRedirectUri cfg)
                , "grant_type=authorization_code"
                ]
    initReq <- HC.parseRequest "https://oauth2.googleapis.com/token"
    let req =
            initReq
                { HC.method = "POST"
                , HC.requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
                , HC.requestBody = HC.RequestBodyBS body
                }
    resp <- HC.httpLbs req mgr
    let respBody = HC.responseBody resp
    case decode respBody :: Maybe (Map Text Value) of
        Just m
            | Just (String idToken) <- Map.lookup "id_token" m ->
                pure $ extractEmailFromJwt idToken
        _ ->
            pure $ Left ("Token exchange failed: " <> TE.decodeUtf8 (BL.toStrict respBody))

{- | Extract the email from a JWT ID token payload.
No signature verification needed — we received it directly from Google over HTTPS.
-}
extractEmailFromJwt :: Text -> Either Text Text
extractEmailFromJwt jwt =
    case T.splitOn "." jwt of
        [_, payload, _] ->
            case decodeBase64Url (TE.encodeUtf8 payload) of
                Nothing -> Left "Base64 decode failed"
                Just decoded ->
                    case decode (BL.fromStrict decoded) :: Maybe (Map Text Value) of
                        Just m
                            | Just (String email) <- Map.lookup "email" m ->
                                Right email
                        _ -> Left "No email in ID token"
        _ -> Left "Invalid JWT format"

-- | Decode base64url (JWT uses URL-safe base64 without padding).
decodeBase64Url :: BS.ByteString -> Maybe BS.ByteString
decodeBase64Url input =
    let
        -- Add padding
        padLen = (4 - BS.length input `mod` 4) `mod` 4
        padded = input <> B8.replicate padLen '='
        -- Replace URL-safe chars
        standard =
            BS.map (\c -> if c == 0x2D then 0x2B else if c == 0x5F then 0x2F else c) padded
     in
        Just (b64decode standard)

b64decode :: BS.ByteString -> BS.ByteString
b64decode input =
    let alphabet = B8.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        indexOf c = maybe 0 id (B8.elemIndex c alphabet)
        clean = B8.filter (/= '=') input
        -- Convert each base64 char to 6 bits
        sextets = map (\w -> indexOf (toEnum (fromIntegral w))) (BS.unpack clean)
        -- Group into 4-sextet blocks → 3 bytes each
        bytes = go sextets
     in BS.pack (map fromIntegral bytes)
  where
    go (a : b : c : d : rest) =
        let n = a * 262144 + b * 4096 + c * 64 + d
         in (n `div` 65536) `mod` 256
                : (n `div` 256) `mod` 256
                : n `mod` 256
                : go rest
    go [a, b, c] =
        let n = a * 262144 + b * 4096 + c * 64
         in [(n `div` 65536) `mod` 256, (n `div` 256) `mod` 256]
    go [a, b] =
        let n = a * 262144 + b * 4096
         in [(n `div` 65536) `mod` 256]
    go _ = []

-- | Generate a random hex token for session IDs and CSRF state.
generateRandomToken :: IO Text
generateRandomToken = do
    h <- System.IO.openBinaryFile "/dev/urandom" System.IO.ReadMode
    bytes <- BS.hGet h 24
    System.IO.hClose h
    pure $ TE.decodeUtf8 $ B8.concatMap toHex bytes
  where
    toHex c =
        let n = fromEnum c
            hi = n `div` 16
            lo = n `mod` 16
         in B8.pack [hexChar hi, hexChar lo]
    hexChar n
        | n < 10 = toEnum (n + fromEnum '0')
        | otherwise = toEnum (n - 10 + fromEnum 'a')

encodeParam :: Text -> Text
encodeParam = TE.decodeUtf8 . urlEncode True . TE.encodeUtf8
