{-# LANGUAGE OverloadedStrings #-}

module Hub.Config (
    loadConfig,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Hub.Types
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

loadConfig :: IO HubConfig
loadConfig = do
    port <- envInt "HUB_PORT" 8080
    backendPort <- envInt "HUB_BACKEND_PORT" 3000
    idleMin <- envInt "HUB_IDLE_TIMEOUT_MIN" 30
    cluster <- envText "HUB_ECS_CLUSTER" "sabela"
    taskDef <- envText "HUB_ECS_TASK_DEF" "sabela"
    subnets <- envText "HUB_ECS_SUBNETS" ""
    sgs <- envText "HUB_ECS_SECURITY_GROUPS" ""
    region <- envText "HUB_ECS_REGION" "us-east-1"
    clientId <- envRequired "GOOGLE_CLIENT_ID"
    clientSecret <- envRequired "GOOGLE_CLIENT_SECRET"
    redirectUri <- envText "GOOGLE_REDIRECT_URI" ""
    pure
        HubConfig
            { hcPort = port
            , hcTaskConfig =
                TaskConfig
                    { tcCluster = cluster
                    , tcTaskDefinition = taskDef
                    , tcSubnets = splitComma subnets
                    , tcSecurityGroups = splitComma sgs
                    , tcRegion = region
                    }
            , hcIdleTimeout = fromIntegral (idleMin * 60 :: Int)
            , hcBackendPort = backendPort
            , hcGoogleClientId = clientId
            , hcGoogleClientSecret = clientSecret
            , hcGoogleRedirectUri = redirectUri
            }

splitComma :: Text -> [Text]
splitComma t
    | T.null t = []
    | otherwise = map T.strip (T.splitOn "," t)

envText :: String -> Text -> IO Text
envText name def = do
    val <- lookupEnv name
    pure $ maybe def T.pack val

envRequired :: String -> IO Text
envRequired name = do
    val <- lookupEnv name
    case val of
        Just v -> pure (T.pack v)
        Nothing -> error $ name ++ " environment variable is required"

envInt :: String -> Int -> IO Int
envInt name def = do
    val <- lookupEnv name
    pure $ fromMaybe def (val >>= readMaybe)
