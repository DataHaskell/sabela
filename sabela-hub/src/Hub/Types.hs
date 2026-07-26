{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Hub.Types (
    UserId (..),
    SessionId (..),
    SessionKey (..),
    TaskId (..),
    TaskIp (..),
    TaskStatus (..),
    Session (..),
    SessionState (..),
    SessionKind (..),
    TaskConfig (..),
    DockerConfig (..),
    RunSpec (..),
    BackendKind (..),
    HubConfig (..),
    EcsBackend (..),
    ExportMode (..),
    parseExportMode,
    exportModeText,
    normalizeEmail,
    isLowerHex,
) where

import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime)

normalizeEmail :: Text -> Text
normalizeEmail = T.toLower . T.strip

isLowerHex :: Text -> Bool
isLowerHex t =
    not (T.null t) && T.all (\c -> isDigit c || (c >= 'a' && c <= 'f')) t

data ExportMode = ExpDashboard | ExpSlideshow | ExpNotebook
    deriving (Eq, Show)

parseExportMode :: Text -> Maybe ExportMode
parseExportMode "dashboard" = Just ExpDashboard
parseExportMode "slideshow" = Just ExpSlideshow
parseExportMode "notebook" = Just ExpNotebook
parseExportMode _ = Nothing

exportModeText :: ExportMode -> Text
exportModeText ExpDashboard = "dashboard"
exportModeText ExpSlideshow = "slideshow"
exportModeText ExpNotebook = "notebook"

newtype UserId = UserId Text
    deriving (Eq, Ord, Show)

newtype SessionId = SessionId Text
    deriving (Eq, Ord, Show)

data SessionKey
    = UserSession SessionId
    | ReattachPlaceholder TaskId
    deriving (Eq, Ord, Show)

newtype TaskId = TaskId Text
    deriving (Eq, Ord, Show)

newtype TaskIp = TaskIp {unTaskIp :: Text}
    deriving (Eq, Ord, Show)

data TaskStatus
    = TaskPending
    | TaskRunning TaskIp
    | TaskStopped
    deriving (Eq, Show)

data Session = Session
    { sessionTaskId :: TaskId
    , sessionState :: SessionState
    , sessionLastActivity :: UTCTime
    , sessionUserId :: UserId
    , sessionKind :: SessionKind
    , sessionIdleOverride :: Maybe NominalDiffTime
    }
    deriving (Show)

data SessionKind
    = Authed
    | Public
    deriving (Eq, Show)

data SessionState
    = SStarting
    | SReady TaskIp
    | SStopping
    deriving (Eq, Show)

data TaskConfig = TaskConfig
    { tcCluster :: Text
    , tcTaskDefinition :: Text
    , tcSubnets :: [Text]
    , tcSecurityGroups :: [Text]
    , tcRegion :: Text
    }
    deriving (Show)

data RunSpec = RunSpec
    { rsImage :: Text
    , rsName :: Text
    , rsNetwork :: Text
    , rsMemory :: Text
    , rsCpus :: Text
    , rsMounts :: [(Text, Text, Bool)]
    , rsEnv :: [(Text, Text)]
    , rsCmd :: [Text]
    }
    deriving (Eq, Show)

data DockerConfig = DockerConfig
    { dcImage :: Text
    , dcNetwork :: Text
    , dcDataRoot :: Text
    , dcEnv :: [(Text, Text)]
    , dcMemory :: Text
    , dcCpus :: Text
    , dcNamePrefix :: Text
    }
    deriving (Eq, Show)

data BackendKind
    = BackendDocker
    | BackendEcs
    deriving (Eq, Show)

data HubConfig = HubConfig
    { hcPort :: Int
    , hcBackend :: BackendKind
    , hcTaskConfig :: TaskConfig
    , hcDockerConfig :: DockerConfig
    , hcIdleTimeout :: NominalDiffTime
    , hcCliTokenTtl :: NominalDiffTime
    , hcBackendPort :: Int
    , hcGoogleClientId :: Text
    , hcGoogleClientSecret :: Text
    , hcGoogleRedirectUri :: Text
    , hcSharesDir :: Text
    , hcAllowlistFile :: Maybe FilePath
    , hcUsersDir :: Text
    , hcGalleryDir :: Text
    , hcAssetsDir :: Text
    , hcBootstrapAdmin :: Maybe Text
    , hcAdminContact :: Maybe Text
    }
    deriving (Show)

data EcsBackend = EcsBackend
    { ebRunTask :: TaskConfig -> UserId -> IO TaskId
    , ebDescribeTask :: TaskConfig -> TaskId -> IO TaskStatus
    , ebStopTask :: TaskConfig -> TaskId -> IO ()
    , ebListRunningTasks :: TaskConfig -> IO [TaskId]
    }
