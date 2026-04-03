module Hub.Types (
    UserId (..),
    SessionId (..),
    TaskId (..),
    TaskStatus (..),
    Session (..),
    SessionState (..),
    TaskConfig (..),
    HubConfig (..),
    EcsBackend (..),
) where

import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)

newtype UserId = UserId Text
    deriving (Eq, Ord, Show)

newtype SessionId = SessionId Text
    deriving (Eq, Ord, Show)

newtype TaskId = TaskId Text
    deriving (Eq, Show)

data TaskStatus
    = TaskPending
    | TaskRunning Text -- private IP
    | TaskStopped
    deriving (Eq, Show)

data Session = Session
    { sessionTaskId :: TaskId
    , sessionState :: SessionState
    , sessionLastActivity :: UTCTime
    , sessionUserId :: UserId
    }
    deriving (Show)

data SessionState
    = SStarting
    | SReady Text -- task private IP
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

data HubConfig = HubConfig
    { hcPort :: Int
    , hcTaskConfig :: TaskConfig
    , hcIdleTimeout :: NominalDiffTime
    , hcBackendPort :: Int
    , hcGoogleClientId :: Text
    , hcGoogleClientSecret :: Text
    , hcGoogleRedirectUri :: Text
    }
    deriving (Show)

-- | Record-of-functions interface for ECS operations.
data EcsBackend = EcsBackend
    { ebRunTask :: TaskConfig -> UserId -> IO TaskId
    , ebDescribeTask :: TaskConfig -> TaskId -> IO TaskStatus
    , ebStopTask :: TaskConfig -> TaskId -> IO ()
    }
