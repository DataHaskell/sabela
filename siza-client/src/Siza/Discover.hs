{- | Discovery: find live Sabela servers the way @siza-discover.sh@ did.

Scans the registry @~/.local/state/sabela/servers/*.json@ (honouring
@XDG_STATE_HOME@), probes each entry's @/api/ai/health@, and keeps the live
ones. @SABELA_URL@ short-circuits the scan to a single probed URL.
-}
module Siza.Discover (
    Server (..),
    discover,
    serverValue,
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

-- | A registry entry augmented with its live health body.
data Server = Server
    { srvBaseUrl :: Text
    , srvPort :: Maybe Int
    , srvPid :: Maybe Text
    , srvWorkDir :: Maybe Text
    , srvAuthRequired :: Maybe Bool
    , srvTokenHint :: Maybe Text
    }
    deriving (Show, Eq)

instance FromJSON Server where
    parseJSON = withObject "Server" $ \o ->
        Server . fromMaybe ""
            <$> o .:? "baseUrl"
            <*> o .:? "port"
            <*> o .:? "pid"
            <*> o .:? "workDir"
            <*> o .:? "authRequired"
            <*> o .:? "tokenHint"

{- | Flatten a 'Server' back to the JSON the bash @discover@ printed, with
@live: true@ appended so the wire shape stays compatible.
-}
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

-- | Live servers. @SABELA_URL@, if set, short-circuits to a single probe.
discover :: Conn -> IO [Server]
discover conn =
    case envSabelaUrl (connEnv conn) of
        Just url -> probeOne conn url
        Nothing -> scanRegistry conn

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
