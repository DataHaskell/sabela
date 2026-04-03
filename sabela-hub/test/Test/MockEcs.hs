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
    }

newMockState :: IO MockState
newMockState = do
    runs <- newTVarIO 0
    stops <- newTVarIO 0
    status <- newTVarIO (TaskRunning "10.0.1.100")
    pure
        MockState{mockRunCalls = runs, mockStopCalls = stops, mockTaskStatus = status}

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
        }

testConfig :: HubConfig
testConfig =
    HubConfig
        { hcPort = 8080
        , hcTaskConfig =
            TaskConfig
                { tcCluster = "test-cluster"
                , tcTaskDefinition = "test-task"
                , tcSubnets = ["subnet-abc"]
                , tcSecurityGroups = ["sg-abc"]
                , tcRegion = "us-east-1"
                }
        , hcIdleTimeout = 1800
        , hcBackendPort = 3000
        , hcGoogleClientId = "test-client-id"
        , hcGoogleClientSecret = "test-client-secret"
        , hcGoogleRedirectUri = "http://localhost:8080/_hub/oauth/callback"
        }
