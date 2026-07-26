{-# LANGUAGE OverloadedStrings #-}

module Hub.Docker (
    dockerBackend,
    DockerOps (..),
    cliDockerOps,
    containerName,
    sanitize,
    userRunSpec,
    runArgs,
    inspectArgs,
    stopArgs,
    listArgs,
    parseInspect,
    parseList,
) where

import Control.Concurrent.STM
import Control.Exception (SomeException, bracket_, try)
import Control.Monad (void)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Hub.Types
import System.Process (readProcess)

data DockerOps = DockerOps
    { doRun :: RunSpec -> IO ()
    , doInspect :: Text -> IO TaskStatus
    , doRemove :: Text -> IO ()
    , doList :: Text -> IO [TaskId]
    }

dockerBackend :: DockerConfig -> DockerOps -> IO EcsBackend
dockerBackend dc ops = do
    inflight <- newTVarIO Set.empty
    pure
        EcsBackend
            { ebRunTask = \_ (UserId email) ->
                let name = containerName dc email
                 in withNameLock inflight name $ do
                        st <- doInspect ops name
                        case st of
                            TaskRunning _ -> pure (TaskId name)
                            TaskPending -> pure (TaskId name)
                            TaskStopped -> do
                                doRemove ops name
                                doRun ops (userRunSpec dc email)
                                pure (TaskId name)
            , ebDescribeTask = \_ (TaskId name) -> doInspect ops name
            , ebStopTask = \_ (TaskId name) -> doRemove ops name
            , ebListRunningTasks = \_ -> doList ops (dcNamePrefix dc)
            }

withNameLock :: TVar (Set Text) -> Text -> IO a -> IO a
withNameLock v name = bracket_ acquire release
  where
    acquire = atomically $ do
        s <- readTVar v
        if Set.member name s
            then retry
            else writeTVar v (Set.insert name s)
    release = atomically $ modifyTVar' v (Set.delete name)

containerName :: DockerConfig -> Text -> Text
containerName dc email = dcNamePrefix dc <> sanitize email

sanitize :: Text -> Text
sanitize = T.map (\c -> if c == '@' || c == '.' then '_' else c)

userRunSpec :: DockerConfig -> Text -> RunSpec
userRunSpec dc email =
    RunSpec
        { rsImage = dcImage dc
        , rsName = containerName dc email
        , rsNetwork = dcNetwork dc
        , rsMemory = dcMemory dc
        , rsCpus = dcCpus dc
        , rsMounts =
            [ (root <> "/users/" <> san, "/mnt/sabela/users/" <> san, False)
            , (root <> "/lean", "/mnt/sabela/lean", True)
            , (root <> "/python", "/mnt/sabela/python", True)
            ]
        , rsEnv = dcEnv dc
        , rsCmd = ["/opt/bin/sabela", "3000", "/mnt/sabela/users/" <> san]
        }
  where
    root = dcDataRoot dc
    san = sanitize email

runArgs :: RunSpec -> [String]
runArgs spec =
    [ "run"
    , "-d"
    , "--name"
    , T.unpack (rsName spec)
    , "--network"
    , T.unpack (rsNetwork spec)
    , "--memory"
    , T.unpack (rsMemory spec)
    , "--memory-swap"
    , T.unpack (rsMemory spec)
    , "--cpus"
    , T.unpack (rsCpus spec)
    , "--pids-limit"
    , "512"
    ]
        ++ concatMap mountFlag (rsMounts spec)
        ++ concatMap envFlag (rsEnv spec)
        ++ [T.unpack (rsImage spec)]
        ++ map T.unpack (rsCmd spec)
  where
    mountFlag (h, c, ro) =
        ["-v", T.unpack (h <> ":" <> c <> (if ro then ":ro" else ""))]
    envFlag (k, val) = ["-e", T.unpack (k <> "=" <> val)]

inspectArgs :: Text -> [String]
inspectArgs name =
    [ "inspect"
    , "-f"
    , "{{.State.Running}}|{{.State.Status}}"
    , T.unpack name
    ]

stopArgs :: Text -> [String]
stopArgs name = ["rm", "-f", T.unpack name]

listArgs :: Text -> [String]
listArgs prefix =
    [ "ps"
    , "--filter"
    , "name=" ++ T.unpack prefix
    , "--filter"
    , "status=running"
    , "--format"
    , "{{.Names}}"
    ]

parseInspect :: Text -> Text -> TaskStatus
parseInspect name out =
    case T.splitOn "|" (T.strip out) of
        ("true" : _) -> TaskRunning (TaskIp name)
        (_ : status : _) | isStopped status -> TaskStopped
        _ -> TaskPending
  where
    isStopped s = s `elem` ["exited", "dead", "removing"]

parseList :: Text -> [TaskId]
parseList = map TaskId . filter (not . T.null) . map T.strip . T.lines

cliDockerOps :: DockerOps
cliDockerOps =
    DockerOps
        { doRun = void . docker . runArgs
        , doInspect = \name -> do
            r <- tryDocker (inspectArgs name)
            pure $ maybe TaskStopped (parseInspect name . T.pack) r
        , doRemove = void . tryDocker . stopArgs
        , doList = \prefix -> parseList . T.pack <$> docker (listArgs prefix)
        }

docker :: [String] -> IO String
docker args = readProcess "docker" args ""

tryDocker :: [String] -> IO (Maybe String)
tryDocker args = do
    r <- try (readProcess "docker" args "") :: IO (Either SomeException String)
    pure (either (const Nothing) Just r)
