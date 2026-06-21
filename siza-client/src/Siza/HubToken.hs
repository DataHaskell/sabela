{- | Local storage for the short-lived token minted by @siza login@.

A single token (the hub you last logged into) is kept at
@${XDG_STATE_HOME:-~/.local/state}/sabela/hub-token.json@, beside the server
registry and provenance log. 'statusForUrl' is what 'Siza.Transport.newConn'
consults to auto-attach a valid token as the bearer; the pure 'evalStatus' core
makes the url-match + expiry decision testable without touching the disk.
-}
module Siza.HubToken (
    HubToken (..),
    TokenStatus (..),
    evalStatus,
    statusForUrl,
    hubTokenPath,
    loadHubToken,
    saveHubToken,
    clearHubToken,
) where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    eitherDecode,
    encode,
    object,
    withObject,
    (.:),
    (.=),
 )
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Sabela.AI.Provenance (stateBase)
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    removeFile,
 )
import System.FilePath (takeDirectory, (</>))

-- | A saved CLI token: the hub it authenticates, the opaque token, its expiry.
data HubToken = HubToken
    { htUrl :: Text
    , htToken :: Text
    , htExpiresAt :: UTCTime
    }
    deriving (Show, Eq)

instance ToJSON HubToken where
    toJSON t =
        object
            [ "url" .= htUrl t
            , "token" .= htToken t
            , "expiresAt" .= htExpiresAt t
            ]

instance FromJSON HubToken where
    parseJSON = withObject "HubToken" $ \o ->
        HubToken <$> o .: "url" <*> o .: "token" <*> o .: "expiresAt"

{- | The token's standing for a target URL: absent, present-for-this-hub-but-
expired, or usable. 'Expired' lets the caller print a "run siza login" hint
rather than silently failing the request.
-}
data TokenStatus = NoToken | Expired | Valid Text
    deriving (Show, Eq)

{- | Decide a token's standing against a target URL and the current time. A
token saved for a different hub reads as 'NoToken' (it can't authenticate this
target). URL comparison ignores a trailing slash.
-}
evalStatus :: Text -> UTCTime -> Maybe HubToken -> TokenStatus
evalStatus _ _ Nothing = NoToken
evalStatus url now (Just t)
    | normalizeUrl (htUrl t) /= normalizeUrl url = NoToken
    | now >= htExpiresAt t = Expired
    | otherwise = Valid (htToken t)

normalizeUrl :: Text -> Text
normalizeUrl = T.dropWhileEnd (== '/')

-- | 'evalStatus' against the saved token and the current time.
statusForUrl :: Text -> IO TokenStatus
statusForUrl url = evalStatus url <$> getCurrentTime <*> loadHubToken

hubTokenPath :: IO FilePath
hubTokenPath = (\b -> b </> "sabela" </> "hub-token.json") <$> stateBase

-- | Load the saved token, or 'Nothing' if absent or unreadable.
loadHubToken :: IO (Maybe HubToken)
loadHubToken = do
    path <- hubTokenPath
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            er <- try (LBS.readFile path) :: IO (Either SomeException LBS.ByteString)
            pure $ case er of
                Left _ -> Nothing
                Right raw -> either (const Nothing) Just (eitherDecode raw)

saveHubToken :: HubToken -> IO ()
saveHubToken t = do
    path <- hubTokenPath
    createDirectoryIfMissing True (takeDirectory path)
    LBS.writeFile path (encode t)

clearHubToken :: IO ()
clearHubToken = do
    path <- hubTokenPath
    exists <- doesFileExist path
    when exists (removeFile path)
