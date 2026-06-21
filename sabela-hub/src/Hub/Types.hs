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

{- | The one identity normalization: lowercase + trim, applied at the OAuth
callback boundary before a 'UserId' is minted and to every allowlist / role
comparison — a casing mismatch must never yield a second identity.
-}
normalizeEmail :: Text -> Text
normalizeEmail = T.toLower . T.strip

{- | Non-empty lowercase hex (the 'Hub.OAuth.generateRandomToken' /
'emailHash' charset). The path-traversal guard shared by share slugs and
user-dir keys.
-}
isLowerHex :: Text -> Bool
isLowerHex t =
    not (T.null t) && T.all (\c -> isDigit c || (c >= 'a' && c <= 'f')) t

{- | Which static export a share captures. Parsed at the @\?mode=@ query
boundary and threaded as a typed value through 'publishShare' /
'fetchExport' / 'Share.shareMode', so the only string the wire sees is
the rendered @"dashboard"@/@"slideshow"@/@"notebook"@ — typos can't
silently fall through to a default mid-pipeline.
-}
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

{- | The map key for 'SessionManager.smSessions'. A 'UserSession' wraps the
authenticated user's cookie value; a 'ReattachPlaceholder' is the internal
entry the hub creates at startup for a container that was already running
(see 'Hub.Session.reattachSessions'). Because the two constructors live in
disjoint slots of the map, a forged cookie can never resolve to a
placeholder — the safety property previously enforced by the
@"reattach:"@-prefix runtime guard is now structural.
-}
data SessionKey
    = UserSession SessionId
    | ReattachPlaceholder TaskId
    deriving (Eq, Ord, Show)

newtype TaskId = TaskId Text
    deriving (Eq, Ord, Show)

{- | The private IP (or container name, when behind Docker's name-based
networking) a running task is reachable at. Wrapping the bare 'Text' so
the @ip@ slot in 'TaskRunning' / 'SReady' can't be swapped with a 'TaskId'
or any other identifier.
-}
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
    {- ^ Per-session idle timeout; when 'Nothing', the global
    'hcIdleTimeout' applies. Public sandboxes use a shorter override.
    -}
    }
    deriving (Show)

{- | Whether a session belongs to an authenticated user or an anonymous
public-share visitor. Public sessions are reaped sooner and (in Phase 3b)
run on a separate, more strongly isolated backend.
-}
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

{- | A single container run request, shared by the Docker (authed) backend and
(Phase 3b) the Fargate microVM backend. Building one is pure so the argv it
produces is unit-testable without a daemon.
-}
data RunSpec = RunSpec
    { rsImage :: Text
    , rsName :: Text
    , rsNetwork :: Text
    , rsMemory :: Text
    , rsCpus :: Text
    , rsMounts :: [(Text, Text, Bool)]
    -- ^ Bind mounts as (hostPath, containerPath, readOnly).
    , rsEnv :: [(Text, Text)]
    , rsCmd :: [Text]
    }
    deriving (Eq, Show)

-- | Configuration for the local-Docker backend (Phase 1, authed users).
data DockerConfig = DockerConfig
    { dcImage :: Text
    , dcNetwork :: Text
    , dcDataRoot :: Text
    {- ^ EFS path on the host (e.g. @\/mnt\/sabela@). Per-user containers bind
    only their own subdir under it (RW) + shared tooling (RO), never the
    @users\/@ parent, so a cell cannot read other tenants' notebooks.
    -}
    , dcEnv :: [(Text, Text)]
    , dcMemory :: Text
    , dcCpus :: Text
    , dcNamePrefix :: Text
    {- ^ Container-name prefix; must not collide with the @sabela-hub@
    container (default @sabela-user-@).
    -}
    }
    deriving (Eq, Show)

{- | Which backend the hub spawns sessions on. @docker@ (default) is the
single-box authed backend; @ecs@ is the legacy Fargate path, kept for
rollback and reused for the public microVM substrate.
-}
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
    {- ^ Lifetime of a @siza login@ CLI token (@HUB_CLI_TOKEN_TTL_MIN@,
    default 8h). The token is bound to the approving browser session and
    dies with it; this caps it independently so a stale token can't linger.
    -}
    , hcBackendPort :: Int
    , hcGoogleClientId :: Text
    , hcGoogleClientSecret :: Text
    , hcGoogleRedirectUri :: Text
    , hcSharesDir :: Text
    {- ^ Host directory for published static shares (Phase 3a); default
    @\/mnt\/sabela\/shares@.
    -}
    , hcAllowlistFile :: Maybe FilePath
    {- ^ Signup allowlist file (@HUB_ALLOWLIST_FILE@), re-read per login.
    'Nothing' = open signup (dev mode; @app/Main.hs@ warns at startup).
    -}
    , hcUsersDir :: Text
    -- ^ Host directory for the user-role store (@HUB_USERS_DIR@).
    , hcGalleryDir :: Text
    -- ^ Host directory for the curated gallery index (@HUB_GALLERY_DIR@).
    , hcAssetsDir :: Text
    {- ^ Host directory for cacheable static assets served at
    @\/_hub\/assets\/<file>@ (@HUB_ASSETS_DIR@); the WASM runtime lives here.
    -}
    , hcBootstrapAdmin :: Maybe Text
    {- ^ Email ensured-admin at hydrate (@HUB_BOOTSTRAP_ADMIN@); without it and
    with no admin on disk the admin surface is inert.
    -}
    , hcAdminContact :: Maybe Text
    {- ^ Public request-access address (@HUB_ADMIN_CONTACT@); the gallery footer
    CTA and the share panel @mailto:@ degrade gracefully when unset.
    -}
    }
    deriving (Show)

-- | Record-of-functions interface for ECS operations.
data EcsBackend = EcsBackend
    { ebRunTask :: TaskConfig -> UserId -> IO TaskId
    , ebDescribeTask :: TaskConfig -> TaskId -> IO TaskStatus
    , ebStopTask :: TaskConfig -> TaskId -> IO ()
    , ebListRunningTasks :: TaskConfig -> IO [TaskId]
    }
