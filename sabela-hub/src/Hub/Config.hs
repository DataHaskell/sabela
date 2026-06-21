{-# LANGUAGE OverloadedStrings #-}

module Hub.Config (
    loadConfig,
) where

import Data.Char (toLower)
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
    cliTokenMin <- envInt "HUB_CLI_TOKEN_TTL_MIN" 480
    cluster <- envText "HUB_ECS_CLUSTER" "sabela"
    taskDef <- envText "HUB_ECS_TASK_DEF" "sabela"
    subnets <- envText "HUB_ECS_SUBNETS" ""
    sgs <- envText "HUB_ECS_SECURITY_GROUPS" ""
    region <- envText "HUB_ECS_REGION" "us-east-1"
    clientId <- envRequired "GOOGLE_CLIENT_ID"
    clientSecret <- envRequired "GOOGLE_CLIENT_SECRET"
    redirectUri <- envText "GOOGLE_REDIRECT_URI" ""
    sharesDir <- envText "HUB_SHARES_DIR" "/mnt/sabela/shares"
    allowlistFile <- lookupEnv "HUB_ALLOWLIST_FILE"
    usersDir <- envText "HUB_USERS_DIR" "/mnt/sabela/users"
    galleryDir' <- envText "HUB_GALLERY_DIR" "/mnt/sabela/gallery"
    assetsDir <- envText "HUB_ASSETS_DIR" "static"
    bootstrapAdmin <- fmap T.pack <$> lookupEnv "HUB_BOOTSTRAP_ADMIN"
    adminContact <- fmap T.pack <$> lookupEnv "HUB_ADMIN_CONTACT"
    backendKind <- envBackend "HUB_BACKEND" BackendDocker
    dockerCfg <- loadDockerConfig
    pure
        HubConfig
            { hcPort = port
            , hcBackend = backendKind
            , hcTaskConfig =
                TaskConfig
                    { tcCluster = cluster
                    , tcTaskDefinition = taskDef
                    , tcSubnets = splitComma subnets
                    , tcSecurityGroups = splitComma sgs
                    , tcRegion = region
                    }
            , hcDockerConfig = dockerCfg
            , hcIdleTimeout = fromIntegral (idleMin * 60 :: Int)
            , hcCliTokenTtl = fromIntegral (cliTokenMin * 60 :: Int)
            , hcBackendPort = backendPort
            , hcGoogleClientId = clientId
            , hcGoogleClientSecret = clientSecret
            , hcGoogleRedirectUri = redirectUri
            , hcSharesDir = sharesDir
            , hcAllowlistFile = allowlistFile
            , hcUsersDir = usersDir
            , hcGalleryDir = galleryDir'
            , hcAssetsDir = assetsDir
            , hcBootstrapAdmin = bootstrapAdmin
            , hcAdminContact = adminContact
            }

{- | Build the Docker-backend config. Env defaults mirror the Lean/Python
paths from infra/task-definition.json so containers see the same layout.
-}
loadDockerConfig :: IO DockerConfig
loadDockerConfig = do
    image <- envText "HUB_DOCKER_IMAGE" "datahaskell/sabela:latest"
    network <- envText "HUB_DOCKER_NETWORK" "sabela-net"
    dataRoot <- envText "HUB_DOCKER_DATA_ROOT" "/mnt/sabela"
    memory <- envText "HUB_DOCKER_MEMORY" "6g"
    cpus <- envText "HUB_DOCKER_CPUS" "2"
    ghciCaps <- envText "HUB_GHCI_CAPS" cpus
    ghciHeap <- envText "HUB_GHCI_MAXHEAP" "4G"
    prefix <- envText "HUB_DOCKER_NAME_PREFIX" "sabela-user-"
    leanRepl <-
        envText "SABELA_LEAN_REPL" "/mnt/sabela/lean/repl/.lake/build/bin/repl"
    leanBase <- envText "SABELA_LEAN_BASE" "/mnt/sabela/lean/lean-base"
    elanHome <- envText "ELAN_HOME" "/mnt/sabela/lean/.elan"
    venv <- envText "VIRTUAL_ENV" "/mnt/sabela/python/venv"
    mAiToken <- lookupEnv "SABELA_AI_TOKEN"
    mAnthropic <- lookupEnv "ANTHROPIC_API_KEY"
    let baseEnv =
            [ ("CABAL_DIR", "/root/.cabal")
            , ("SABELA_DEBUG", "1")
            , ("SABELA_LEAN_REPL", leanRepl)
            , ("SABELA_LEAN_BASE", leanBase)
            , ("ELAN_HOME", elanHome)
            , ("VIRTUAL_ENV", venv)
            , ("SABELA_GHCI_CAPS", ghciCaps)
            , ("SABELA_GHCI_MAXHEAP", ghciHeap)
            ]
        optEnv =
            [("SABELA_AI_TOKEN", T.pack v) | Just v <- [mAiToken]]
                ++ [("ANTHROPIC_API_KEY", T.pack v) | Just v <- [mAnthropic]]
    pure
        DockerConfig
            { dcImage = image
            , dcNetwork = network
            , dcDataRoot = dataRoot
            , dcEnv = baseEnv ++ optEnv
            , dcMemory = memory
            , dcCpus = cpus
            , dcNamePrefix = prefix
            }

envBackend :: String -> BackendKind -> IO BackendKind
envBackend name def = do
    val <- lookupEnv name
    pure $ case fmap (map toLower) val of
        Just "ecs" -> BackendEcs
        Just "docker" -> BackendDocker
        _ -> def

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
