{-# LANGUAGE OverloadedStrings #-}

{- | Network access control for the whole server. A loopback bind is open;
any other bind needs the access token as a bearer header, the @sabela_token@
cookie, or a one-time @?token=@ query that sets that cookie.
-}
module Sabela.Server.Access (
    Access (..),
    resolveAccess,
    isLoopbackHost,
    accessMiddleware,
    tokenAllowed,
    checkBearer,
    constEqBS,
) where

import Control.Exception (IOException, try)
import Data.Bits (xor, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (foldl', isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Network.HTTP.Types (
    HeaderName,
    hAuthorization,
    hContentType,
    hCookie,
    status401,
 )
import Network.Wai (
    Middleware,
    Request,
    mapResponseHeaders,
    queryString,
    requestHeaders,
    responseLBS,
 )
import System.Environment (lookupEnv)
import System.IO (IOMode (ReadMode), withBinaryFile)
import Text.Printf (printf)

-- | Whether every request must present the access token.
data Access = Open | TokenRequired Text
    deriving (Eq, Show)

{- | Decide access for a bind host. Off loopback the token comes from
@SABELA_TOKEN@, then @SABELA_AI_TOKEN@, else a fresh random one;
@SABELA_AUTH=none@ opts out for a container behind an authenticating proxy.
-}
resolveAccess :: String -> IO (Either String Access)
resolveAccess host
    | isLoopbackHost host = pure (Right Open)
    | otherwise = do
        auth <- lookupEnv "SABELA_AUTH"
        tok <- nonEmpty <$> lookupEnv "SABELA_TOKEN"
        aiTok <- nonEmpty <$> lookupEnv "SABELA_AI_TOKEN"
        case (auth, tok, aiTok) of
            (Just "none", _, _) -> pure (Right Open)
            (_, Just t, _) -> pure (Right (TokenRequired (T.pack t)))
            (_, _, Just t) -> pure (Right (TokenRequired (T.pack t)))
            _ -> fmap TokenRequired <$> randomToken
  where
    nonEmpty = maybe Nothing (\s -> if null s then Nothing else Just s)

-- | Loopback names and addresses; everything else counts as a network bind.
isLoopbackHost :: String -> Bool
isLoopbackHost h = h `elem` ["localhost", "::1"] || "127." `isPrefixOf` h

randomToken :: IO (Either String Text)
randomToken = do
    r <- try (withBinaryFile "/dev/urandom" ReadMode (`BS.hGet` 24))
    pure $ case r :: Either IOException BS.ByteString of
        Right bs | BS.length bs == 24 -> Right (T.pack (concatMap hex (BS.unpack bs)))
        _ -> Left "No random source; set SABELA_TOKEN to bind off loopback."
  where
    hex :: Word8 -> String
    hex = printf "%02x"

{- | Gate every route on the token. A matching @?token=@ query also sets the
cookie, so the browser keeps access for page loads, SSE and API calls.
-}
accessMiddleware :: Access -> Middleware
accessMiddleware Open baseApp req sendResp = baseApp req sendResp
accessMiddleware (TokenRequired tok) baseApp req sendResp
    | queryTokenMatches tok req =
        baseApp req (sendResp . mapResponseHeaders (setCookie :))
    | tokenAllowed tok req = baseApp req sendResp
    | otherwise = sendResp unauthorized
  where
    setCookie =
        ( "Set-Cookie"
        , "sabela_token=" <> TE.encodeUtf8 tok <> "; Path=/; HttpOnly; SameSite=Strict"
        )
    unauthorized =
        responseLBS
            status401
            [(hContentType, "text/plain; charset=utf-8")]
            "Access token required: open the URL sabela printed at startup."

-- | A request carrying the token as a bearer header, cookie or query.
tokenAllowed :: Text -> Request -> Bool
tokenAllowed tok req =
    checkBearer tok (requestHeaders req)
        || any (constEqBS tokBs) (cookieValues "sabela_token" (requestHeaders req))
        || queryTokenMatches tok req
  where
    tokBs = TE.encodeUtf8 tok

queryTokenMatches :: Text -> Request -> Bool
queryTokenMatches tok req = case lookup "token" (queryString req) of
    Just (Just v) -> constEqBS v (TE.encodeUtf8 tok)
    _ -> False

cookieValues ::
    BS.ByteString -> [(HeaderName, BS.ByteString)] -> [BS.ByteString]
cookieValues name hdrs =
    [ BS.drop 1 v
    | (h, raw) <- hdrs
    , h == hCookie
    , pair <- BC.split ';' raw
    , let (k, v) = BC.break (== '=') (BC.dropWhile (== ' ') pair)
    , k == name
    ]

checkBearer :: Text -> [(HeaderName, BS.ByteString)] -> Bool
checkBearer tok hdrs = case lookup hAuthorization hdrs of
    Just v -> constEqBS v ("Bearer " <> TE.encodeUtf8 tok)
    Nothing -> False

constEqBS :: BS.ByteString -> BS.ByteString -> Bool
constEqBS a b =
    BS.length a == BS.length b
        && foldl' (.|.) 0 (BS.zipWith xor a b) == (0 :: Word8)
