{-# LANGUAGE OverloadedStrings #-}

{- | Local-Docker backend for the hub (Phase 1, authed users).

The lifecycle/idempotency logic is built over a small 'DockerOps' record so it
is testable without a Docker daemon; 'cliDockerOps' is the real implementation
that shells out to @docker@. The argv builders and parsers are pure.

Container names are deterministic per user (@<prefix><sanitized-email>@) while
sessions are keyed per 'SessionId', so a user signing in twice must converge on
one container. 'dockerBackend' therefore makes create **idempotent and
name-locked**: concurrent @ebRunTask@ calls for the same user collapse to a
single @docker run@.
-}
module Hub.Docker (
    -- * Backend
    dockerBackend,
    DockerOps (..),
    cliDockerOps,

    -- * Pure helpers (exported for testing)
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

{- | The primitive Docker operations the backend builds on. The real
implementation is 'cliDockerOps'; tests inject an in-memory fake.
-}
data DockerOps = DockerOps
    { doRun :: RunSpec -> IO ()
    -- ^ @docker run -d@ a fresh container.
    , doInspect :: Text -> IO TaskStatus
    -- ^ Status of the named container; a missing container is 'TaskStopped'.
    , doRemove :: Text -> IO ()
    -- ^ @docker rm -f@; idempotent (a missing container is fine).
    , doList :: Text -> IO [TaskId]
    -- ^ Running container names with the given prefix.
    }

{- | Build the authed Docker backend. Allocates a per-name in-flight set so
concurrent creates for the same user collapse to one @docker run@.
-}
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

-- | Serialize an action per container name (STM gate).
withNameLock :: TVar (Set Text) -> Text -> IO a -> IO a
withNameLock v name = bracket_ acquire release
  where
    acquire = atomically $ do
        s <- readTVar v
        if Set.member name s
            then retry
            else writeTVar v (Set.insert name s)
    release = atomically $ modifyTVar' v (Set.delete name)

-- ---------------------------------------------------------------------------
-- Pure builders
-- ---------------------------------------------------------------------------

-- | Deterministic container name for a user (prefix + sanitized email).
containerName :: DockerConfig -> Text -> Text
containerName dc email = dcNamePrefix dc <> sanitize email

{- | Replace @\@@ and @.@ with @_@ so an email is a legal Docker name / path
component. (Same rule as "Hub.Ecs".)
-}
sanitize :: Text -> Text
sanitize = T.map (\c -> if c == '@' || c == '.' then '_' else c)

{- | The run spec for an authed user's notebook container. Mounts only the
user's own directory (RW) plus the shared Lean/Python toolchain (RO) - never
the @users\/@ parent - so a cell cannot read other tenants' notebooks via a
shell escape (@:!@) or raw file IO. @dcDataRoot@ is the EFS path on the host;
container paths stay the canonical @\/mnt\/sabela\/...@ the app expects.
-}
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

-- | @docker run@ argv for a 'RunSpec'.
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

-- | @docker inspect@ argv (running flag + status on one line).
inspectArgs :: Text -> [String]
inspectArgs name =
    [ "inspect"
    , "-f"
    , "{{.State.Running}}|{{.State.Status}}"
    , T.unpack name
    ]

-- | @docker rm -f@ argv.
stopArgs :: Text -> [String]
stopArgs name = ["rm", "-f", T.unpack name]

-- | @docker ps@ argv listing running container names with a prefix.
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

{- | Parse @docker inspect -f '{{.State.Running}}|{{.State.Status}}'@ output.
The address carried by 'TaskRunning' is the container name (resolved by Docker
embedded DNS on the shared network).
-}
parseInspect :: Text -> Text -> TaskStatus
parseInspect name out =
    case T.splitOn "|" (T.strip out) of
        ("true" : _) -> TaskRunning (TaskIp name)
        (_ : status : _) | isStopped status -> TaskStopped
        _ -> TaskPending
  where
    isStopped s = s `elem` ["exited", "dead", "removing"]

-- | Parse @docker ps --format '{{.Names}}'@ output into running 'TaskId's.
parseList :: Text -> [TaskId]
parseList = map TaskId . filter (not . T.null) . map T.strip . T.lines

-- ---------------------------------------------------------------------------
-- Real (CLI) DockerOps
-- ---------------------------------------------------------------------------

-- | Shell out to the @docker@ CLI.
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
