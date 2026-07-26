module Siza.Discover (
    Server (..),
    discover,
    serverValue,
    defaultLocalUrl,
) where

import Control.Monad (filterM)
import Data.Aeson (
    FromJSON (..),
    Value,
    eitherDecode,
    object,
    withObject,
    (.:?),
    (.=),
 )
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Provenance (stateBase)
import Siza.Transport (Conn (..), Env (..), getHealth)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

data Server = Server
    { srvBaseUrl :: Text
    , srvPort :: Maybe Int
    , srvPid :: Maybe Text
    , srvWorkDir :: Maybe Text
    , srvAuthRequired :: Maybe Bool
    , srvTokenHint :: Maybe Text
    }
    deriving (Eq, Show)

instance FromJSON Server where
    parseJSON = withObject "Server" $ \o ->
        Server . fromMaybe ""
            <$> o .:? "baseUrl"
            <*> o .:? "port"
            <*> o .:? "pid"
            <*> o .:? "workDir"
            <*> o .:? "authRequired"
            <*> o .:? "tokenHint"

serverValue :: Server -> Value
serverValue s =
    object
        [ "baseUrl" .= srvBaseUrl s
        , "port" .= srvPort s
        , "pid" .= srvPid s
        , "workDir" .= srvWorkDir s
        , "authRequired" .= srvAuthRequired s
        , "tokenHint" .= srvTokenHint s
        , "live" .= True
        ]

defaultLocalUrl :: Text
defaultLocalUrl = "http://localhost:3000"

discover :: Conn -> IO [Server]
discover conn =
    case envSabelaUrl (connEnv conn) of
        Just url -> probeOne conn url
        Nothing -> do
            servers <- scanRegistry conn
            if null servers then probeOne conn defaultLocalUrl else pure servers

probeOne :: Conn -> Text -> IO [Server]
probeOne conn url = do
    mh <- getHealth conn url
    pure $ case mh of
        Nothing -> []
        Just _ -> [Server url Nothing Nothing Nothing Nothing Nothing]

scanRegistry :: Conn -> IO [Server]
scanRegistry conn = do
    dir <- registryDir
    exists <- doesDirectoryExist dir
    if not exists
        then pure []
        else do
            files <- map (dir </>) . filter isJsonFile <$> listDirectory dir
            entries <- catMaybes <$> mapM readEntry files
            filterM (fmap isLive . getHealth conn . srvBaseUrl) entries
  where
    isJsonFile = T.isSuffixOf ".json" . T.pack
    isLive = isJust

readEntry :: FilePath -> IO (Maybe Server)
readEntry f = do
    raw <- LBS.readFile f
    pure (either (const Nothing) Just (eitherDecode raw))

registryDir :: IO FilePath
registryDir = (\b -> b </> "sabela" </> "servers") <$> stateBase
