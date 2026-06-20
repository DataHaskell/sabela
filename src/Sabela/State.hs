{-# LANGUAGE OverloadedStrings #-}

module Sabela.State (
    App (..),
    newApp,
    clearCompiledModules,
    setBuilding,
    withBuilding,
    getAIStore,
    setAIStore,
    configureAI,
    updateAIConfig,
    AIConfigUpdate (..),
    broadcastNotebook,
    resolveCliHandleStore,

    -- * Re-exports for convenience
    module Sabela.State.Environment,
    module Sabela.State.EventBus,
    module Sabela.State.NotebookStore,
    module Sabela.State.SessionManager,
    module Sabela.State.DependencyTracker,
    module Sabela.State.WidgetStore,
    module Sabela.State.BridgeStore,
) where

import Control.Applicative ((<|>))
import Control.Concurrent.MVar (
    MVar,
    modifyMVar,
    modifyMVar_,
    newMVar,
    readMVar,
 )
import Control.Exception (bracket_)
import Data.Aeson (Value (..), eitherDecodeStrict, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)
import Sabela.AI.Handles (HandleStore, newHandleStore)
import Sabela.AI.Store (
    AIStore,
    getAIConfig,
    newAIStore,
    setAIFullConfig,
    setAIModel,
 )
import Sabela.Anthropic.Types (AnthropicConfig (..))
import Sabela.Model (NotebookEvent (..))
import Sabela.State.BridgeStore
import Sabela.State.DependencyTracker
import Sabela.State.Environment
import Sabela.State.EventBus
import Sabela.State.NotebookStore
import Sabela.State.SessionManager
import Sabela.State.WidgetStore
import System.Directory (
    canonicalizePath,
    createDirectoryIfMissing,
    doesFileExist,
 )
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

data App = App
    { appEnv :: Environment
    , appNotebook :: NotebookStore
    , appEvents :: EventBus
    , appSessions :: SessionManager
    , appDeps :: DependencyTracker
    , appWidgets :: WidgetStore
    , appBridge :: BridgeStore
    , appCompiledModules :: IORef (M.Map Text Text)
    {- ^ Module name → rendered source currently loaded into the live GHCi
    session. Drives the compile-phase skip; cleared on session restart.
    -}
    , appAI :: MVar (Maybe AIStore)
    , appHttpMgr :: Maybe Manager
    , appAiToken :: Maybe Text
    {- ^ If set, `/api/ai/*` requires `Authorization: Bearer <token>`.
    Comes from the @SABELA_AI_TOKEN@ env var at startup.
    -}
    , appCliSessions :: MVar (M.Map Text HandleStore)
    {- ^ Per-session handle stores for external CLI clients, keyed by
    the @X-Sabela-Session@ header. Created lazily on first request.
    -}
    , appBuilding :: IORef Bool
    {- ^ True while the kernel is doing off-lock build work — installing a
    cabal env, spawning/cold-starting GHCi, or compiling a @-- compile@
    module. Distinct from the run-lock @running@ axis so a driver can tell a
    cold start from a hung cell ('kernel_status' surfaces it as @compiling@).
    -}
    }

-- | Forget which compiled modules the live session has loaded.
clearCompiledModules :: App -> IO ()
clearCompiledModules app = writeIORef (appCompiledModules app) M.empty

-- | Flip the off-lock build flag (see 'appBuilding').
setBuilding :: App -> Bool -> IO ()
setBuilding app = writeIORef (appBuilding app)

{- | Run an action with the build flag raised, lowering it again even on
exception. Wrap cabal-env installs, cold starts, and @-- compile@ builds so
'kernel_status' reports @compiling@ while they run.
-}
withBuilding :: App -> IO a -> IO a
withBuilding app = bracket_ (setBuilding app True) (setBuilding app False)

-- | Read the current AI store (if configured).
getAIStore :: App -> IO (Maybe AIStore)
getAIStore = readMVar . appAI

-- | Set the AI store.
setAIStore :: App -> Maybe AIStore -> IO ()
setAIStore app val = modifyMVar_ (appAI app) (const (pure val))

data AIConfigUpdate = AIConfigUpdate
    { aicuApiKey :: Maybe Text
    , aicuModel :: Maybe Text
    }

{- | Configure AI with an API key at runtime (legacy single-field path).
Writes the key to <workdir>/.sabela/config.json and initializes the AIStore.
-}
configureAI :: App -> Text -> IO (Either Text ())
configureAI app apiKey =
    updateAIConfig app AIConfigUpdate{aicuApiKey = Just apiKey, aicuModel = Nothing}

{- | Apply partial updates to the AI config. If the store is not yet initialized
and no API key is supplied, returns an error. The API key and model are both
persisted to <workdir>/.sabela/config.json so changes survive server restarts.
-}
updateAIConfig :: App -> AIConfigUpdate -> IO (Either Text ())
updateAIConfig app upd = case appHttpMgr app of
    Nothing -> pure (Left "No HTTP manager available")
    Just mgr -> do
        mStore <- getAIStore app
        case (mStore, aicuApiKey upd) of
            (Nothing, Nothing) ->
                pure (Left "apiKey is required for first-time setup")
            (Nothing, Just key) -> do
                let model = fromMaybe (envAnthropicModel (appEnv app)) (aicuModel upd)
                    cfg =
                        AnthropicConfig
                            { acApiKey = key
                            , acModel = model
                            , acBaseUrl = T.pack "https://api.anthropic.com"
                            }
                store <- newAIStore cfg mgr
                setAIStore app (Just store)
                persistConfig app key model
                pure (Right ())
            (Just store, _) -> do
                oldCfg <- getAIConfig store
                let newKey = fromMaybe (acApiKey oldCfg) (aicuApiKey upd)
                    newModel = fromMaybe (acModel oldCfg) (aicuModel upd)
                    newCfg =
                        oldCfg{acApiKey = newKey, acModel = newModel}
                case (aicuApiKey upd, aicuModel upd) of
                    (Just _, _) -> setAIFullConfig store newCfg
                    (Nothing, Just m) -> setAIModel store m
                    _ -> pure ()
                persistConfig app newKey newModel
                pure (Right ())

{- | Look up (or lazily create) a per-CLI-session HandleStore keyed by the
@X-Sabela-Session@ header value. Isolates @explore_result@ handles between
concurrent external CLI clients.
-}
resolveCliHandleStore :: App -> Text -> IO HandleStore
resolveCliHandleStore app sid = modifyMVar (appCliSessions app) $ \m ->
    case M.lookup sid m of
        Just hs -> pure (m, hs)
        Nothing -> do
            hs <- newHandleStore
            pure (M.insert sid hs m, hs)

{- | Read the current notebook and broadcast it as an @EvNotebookChanged@ SSE
event. Call this after any mutation that changes the cell list, cell order, or
cell source outside the reactive execute pipeline — AI tool mutations, HTTP
insert/delete/reorder handlers, accepted edits.
-}
broadcastNotebook :: App -> IO ()
broadcastNotebook app = do
    nb <- readNotebook (appNotebook app)
    broadcast (appEvents app) (EvNotebookChanged nb)

persistConfig :: App -> Text -> Text -> IO ()
persistConfig app key model = do
    let configDir = envWorkDir (appEnv app) </> ".sabela"
        configFile = configDir </> "config.json"
        json = encode (object ["anthropicKey" .= key, "anthropicModel" .= model])
    createDirectoryIfMissing True configDir
    BS.writeFile configFile (BS.toStrict json)

newApp ::
    FilePath -> Set Text -> Maybe Manager -> Maybe Text -> [FilePath] -> IO App
newApp workDir globalDeps mHttpMgr mAiToken localPkgs = do
    absWork <- canonicalizePath workDir
    localAbs <- mapM canonicalizePath localPkgs
    tmpBase <- getCanonicalTemporaryDirectory
    tmpDir <- createTempDirectory tmpBase "sabela-server"
    debug <- isJust <$> lookupEnv "SABELA_DEBUG"
    (mApiKey, mSavedModel) <- resolveConfig absWork
    envModel <- lookupEnv "ANTHROPIC_MODEL"
    let defaultModel = "claude-sonnet-4-20250514"
        apiModel = fromMaybe defaultModel (envModel <|> fmap T.unpack mSavedModel)
        env =
            Environment
                { envWorkDir = absWork
                , envTmpDir = tmpDir
                , envGlobalDeps = globalDeps
                , envLocalPackages = localAbs
                , envDebugLog = debug
                , envAnthropicKey = T.pack <$> mApiKey
                , envAnthropicModel = T.pack apiModel
                }
    mAIStore <- case (mApiKey, mHttpMgr) of
        (Just key, Just mgr) -> do
            let cfg =
                    AnthropicConfig
                        { acApiKey = T.pack key
                        , acModel = T.pack apiModel
                        , acBaseUrl = T.pack "https://api.anthropic.com"
                        }
            Just <$> newAIStore cfg mgr
        _ -> pure Nothing
    aiVar <- newMVar mAIStore
    cliSessionsVar <- newMVar M.empty
    App env
        <$> newNotebookStore
        <*> newEventBus
        <*> newSessionManager
        <*> newDependencyTracker
        <*> newWidgetStore
        <*> newBridgeStore
        <*> newIORef M.empty
        <*> pure aiVar
        <*> pure mHttpMgr
        <*> pure mAiToken
        <*> pure cliSessionsVar
        <*> newIORef False

-- | Resolve API key + saved model. Env ANTHROPIC_API_KEY wins for the key.
resolveConfig :: FilePath -> IO (Maybe String, Maybe Text)
resolveConfig workDir = do
    mEnv <- lookupEnv "ANTHROPIC_API_KEY"
    (fileKey, fileModel) <- readConfigFile workDir
    pure (mEnv <|> fileKey, fileModel)

readConfigFile :: FilePath -> IO (Maybe String, Maybe Text)
readConfigFile workDir = do
    let configFile = workDir </> ".sabela" </> "config.json"
    exists <- doesFileExist configFile
    if not exists
        then pure (Nothing, Nothing)
        else do
            bs <- BS.readFile configFile
            case eitherDecodeStrict bs of
                Right (Object obj) ->
                    let key = case KM.lookup (Key.fromText "anthropicKey") obj of
                            Just (String s) -> Just (T.unpack s)
                            _ -> Nothing
                        model = case KM.lookup (Key.fromText "anthropicModel") obj of
                            Just (String s) | not (T.null s) -> Just s
                            _ -> Nothing
                     in pure (key, model)
                _ -> pure (Nothing, Nothing)
