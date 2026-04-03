{-# LANGUAGE OverloadedStrings #-}

module Hub.Ecs (
    cliEcsBackend,
) where

import qualified Data.Text as T
import Hub.Types
import System.Process (readProcess)

-- | ECS backend that uses the AWS CLI.
cliEcsBackend :: EcsBackend
cliEcsBackend =
    EcsBackend
        { ebRunTask = cliRunTask
        , ebDescribeTask = cliDescribeTask
        , ebStopTask = cliStopTask
        }

cliRunTask :: TaskConfig -> UserId -> IO TaskId
cliRunTask cfg (UserId email) = do
    let netCfg =
            "awsvpcConfiguration={subnets=["
                <> T.intercalate "," (tcSubnets cfg)
                <> "],securityGroups=["
                <> T.intercalate "," (tcSecurityGroups cfg)
                <> "],assignPublicIp=DISABLED}"
        -- Sanitize email for use as directory name (replace @ and . with _)
        userDir = "/mnt/sabela/users/" <> sanitize email
        overrides =
            "{\"containerOverrides\":[{\"name\":\"sabela\",\"command\":"
                <> "[\"/opt/bin/sabela\",\"3000\",\""
                <> userDir
                <> "\"]}]}"
    out <-
        aws
            cfg
            [ "ecs"
            , "run-task"
            , "--task-definition"
            , T.unpack (tcTaskDefinition cfg)
            , "--capacity-provider-strategy"
            , "capacityProvider=FARGATE_SPOT,weight=1"
            , "--network-configuration"
            , T.unpack netCfg
            , "--overrides"
            , T.unpack overrides
            , "--query"
            , "tasks[0].taskArn"
            , "--output"
            , "text"
            ]
    pure $ TaskId (T.strip (T.pack out))

sanitize :: T.Text -> T.Text
sanitize = T.map (\c -> if c == '@' || c == '.' then '_' else c)

cliDescribeTask :: TaskConfig -> TaskId -> IO TaskStatus
cliDescribeTask cfg (TaskId taskArn) = do
    status <-
        aws
            cfg
            [ "ecs"
            , "describe-tasks"
            , "--tasks"
            , T.unpack taskArn
            , "--query"
            , "tasks[0].lastStatus"
            , "--output"
            , "text"
            ]
    case T.strip (T.pack status) of
        "RUNNING" -> do
            ip <-
                aws
                    cfg
                    [ "ecs"
                    , "describe-tasks"
                    , "--tasks"
                    , T.unpack taskArn
                    , "--query"
                    , "tasks[0].attachments[0].details[?name=='privateIPv4Address'].value"
                    , "--output"
                    , "text"
                    ]
            pure $ TaskRunning (T.strip (T.pack ip))
        "STOPPED" -> pure TaskStopped
        _ -> pure TaskPending

cliStopTask :: TaskConfig -> TaskId -> IO ()
cliStopTask cfg (TaskId taskArn) = do
    _ <-
        aws
            cfg
            [ "ecs"
            , "stop-task"
            , "--task"
            , T.unpack taskArn
            ]
    pure ()

-- | Run an AWS CLI command with cluster and region from config.
aws :: TaskConfig -> [String] -> IO String
aws cfg args =
    readProcess
        "aws"
        ( args
            ++ [ "--cluster"
               , T.unpack (tcCluster cfg)
               , "--region"
               , T.unpack (tcRegion cfg)
               ]
        )
        ""
