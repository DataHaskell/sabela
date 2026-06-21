{-# LANGUAGE OverloadedStrings #-}

module Test.MockEcs (
    mockEcsBackend,
    MockState (..),
    newMockState,
    testConfig,
) where

import Control.Concurrent.STM
import Hub.Types

data MockState = MockState
    { mockRunCalls :: TVar Int
    , mockStopCalls :: TVar Int
    , mockTaskStatus :: TVar TaskStatus
    , mockRunningTasks :: TVar [TaskId]
    }

newMockState :: IO MockState
newMockState = do
    runs <- newTVarIO 0
    stops <- newTVarIO 0
    status <- newTVarIO (TaskRunning (TaskIp "10.0.1.100"))
    running <- newTVarIO []
    pure
        MockState
            { mockRunCalls = runs
            , mockStopCalls = stops
            , mockTaskStatus = status
            , mockRunningTasks = running
            }

mockEcsBackend :: MockState -> EcsBackend
mockEcsBackend ms =
    EcsBackend
        { ebRunTask = \_ _ -> do
            atomically $ modifyTVar' (mockRunCalls ms) (+ 1)
            pure (TaskId "arn:aws:ecs:us-east-1:123:task/mock/abc123")
        , ebDescribeTask = \_ _ -> do
            readTVarIO (mockTaskStatus ms)
        , ebStopTask = \_ _ -> do
            atomically $ modifyTVar' (mockStopCalls ms) (+ 1)
        , ebListRunningTasks = \_ ->
            readTVarIO (mockRunningTasks ms)
        }

testConfig :: HubConfig
testConfig =
    HubConfig
        { hcPort = 8080
        , hcBackend = BackendEcs
        , hcTaskConfig =
            TaskConfig
                { tcCluster = "test-cluster"
                , tcTaskDefinition = "test-task"
                , tcSubnets = ["subnet-abc"]
                , tcSecurityGroups = ["sg-abc"]
                , tcRegion = "us-east-1"
                }
        , hcDockerConfig =
            DockerConfig
                { dcImage = "datahaskell/sabela:latest"
                , dcNetwork = "sabela-net"
                , dcDataRoot = "/mnt/sabela"
                , dcEnv = []
                , dcMemory = "6g"
                , dcCpus = "2"
                , dcNamePrefix = "sabela-user-"
                }
        , hcIdleTimeout = 1800
        , hcBackendPort = 3000
        , hcGoogleClientId = "test-client-id"
        , hcGoogleClientSecret = "test-client-secret"
        , hcGoogleRedirectUri = "http://localhost:8080/_hub/oauth/callback"
        , hcSharesDir = "/tmp/sabela-test-shares"
        , hcAllowlistFile = Nothing
        , hcUsersDir = "/tmp/sabela-test-users"
        , hcGalleryDir = "/tmp/sabela-test-gallery"
        , hcAssetsDir = "/tmp/sabela-test-assets"
        , hcBootstrapAdmin = Nothing
        , hcAdminContact = Just "admin@datahaskell.com"
        }
