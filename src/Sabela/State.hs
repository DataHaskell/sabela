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
    configJson,
    providerNameOf,
    AIConfigUpdate (..),
    defaultNumCtx,
    defaultToolLimit,
    getAINumCtx,
    getAIToolLimit,
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
import Data.Aeson (
    Result (..),
    Value (..),
    eitherDecodeStrict,
    encode,
    fromJSON,
    object,
    (.=),
 )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)
import Sabela.AI.Handles (HandleStore, newHandleStore)
import Sabela.AI.Store (
    AIStore,
    getAIConfig,
    getAIProvider,
    newAIStore,
    setAIFullConfig,
    setAIProvider,
 )
import Sabela.Anthropic.Types (AnthropicConfig (..))
import Sabela.LLM.Anthropic (anthropicProvider)
import Sabela.LLM.Ollama (ollamaProvider)
import Sabela.LLM.Provider (ModelProvider, mpName)
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
    , appAINumCtx :: IORef Int
    {- ^ Ollama @num_ctx@ (context window) for the in-notebook assistant,
    configurable from the AI settings modal and persisted to config.json.
    Baked into the Ollama provider when it is (re)built.
    -}
    , appAIToolLimit :: IORef Int
    -- ^ Per-turn cap on tool-call rounds the agentic loop will run.
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
    , aicuProvider :: Maybe Text
    -- ^ @"anthropic"@ | @"ollama"@; 'Nothing' keeps the current provider.
    , aicuNumCtx :: Maybe Int
    -- ^ Ollama @num_ctx@; 'Nothing' keeps the current value.
    , aicuToolLimit :: Maybe Int
    -- ^ Per-turn tool-call round cap; 'Nothing' keeps the current value.
    }

{- | Configure AI with an API key at runtime (legacy single-field path).
Writes the key to <workdir>/.sabela/config.json and initializes the AIStore.
-}
configureAI :: App -> Text -> IO (Either Text ())
configureAI app apiKey =
    updateAIConfig
        app
        AIConfigUpdate
            { aicuApiKey = Just apiKey
            , aicuModel = Nothing
            , aicuProvider = Nothing
            , aicuNumCtx = Nothing
            , aicuToolLimit = Nothing
            }

-- | Provider ids accepted in the config. Ollama needs no API key.
ollamaProviderId :: Text
ollamaProviderId = "ollama"

-- | Default Ollama model when the caller selects Ollama without naming one.
defaultOllamaModel :: Text
defaultOllamaModel = "gpt-oss:20b"

{- | Default Ollama context window. Matches the client's own fallback; holds a
whole agent episode so the oldest messages aren't silently evicted.
-}
defaultNumCtx :: Int
defaultNumCtx = 32768

-- | Default per-turn tool-call round cap for the agentic loop.
defaultToolLimit :: Int
defaultToolLimit = 25

-- | The configured Ollama @num_ctx@ for this workspace.
getAINumCtx :: App -> IO Int
getAINumCtx = readIORef . appAINumCtx

-- | The configured per-turn tool-call round cap for this workspace.
getAIToolLimit :: App -> IO Int
getAIToolLimit = readIORef . appAIToolLimit

{- | Build the backend the loop drives, from the selected provider + config.
@numCtx@ is baked into the Ollama provider (Anthropic ignores it).
-}
buildProvider :: Manager -> Text -> AnthropicConfig -> Int -> ModelProvider
buildProvider mgr provider cfg numCtx
    | provider == ollamaProviderId = ollamaProvider mgr (acModel cfg) numCtx
    | otherwise = anthropicProvider mgr cfg

-- | Recover the provider id from a live backend (its 'mpName' is @ollama:…@/@anthropic@).
providerNameOf :: ModelProvider -> Text
providerNameOf mp
    | ollamaProviderId `T.isPrefixOf` mpName mp = ollamaProviderId
    | otherwise = "anthropic"

{- | Apply partial updates to the AI config, then (re)select the matching
provider so the agentic loop drives the right backend. Anthropic requires an API
key for first-time setup; Ollama does not. Key + model + provider persist to
<workdir>/.sabela/config.json, so the choice survives a restart.
-}
updateAIConfig :: App -> AIConfigUpdate -> IO (Either Text ())
updateAIConfig app upd = case appHttpMgr app of
    Nothing -> pure (Left "No HTTP manager available")
    Just mgr -> do
        mStore <- getAIStore app
        case mStore of
            Nothing -> firstTime mgr
            Just store -> onExisting mgr store
  where
    firstTime mgr =
        let provider = fromMaybe "anthropic" (aicuProvider upd)
         in if provider /= ollamaProviderId && isNothing (aicuApiKey upd)
                then pure (Left "apiKey is required for first-time setup")
                else do
                    (numCtx, limit) <- applyKnobs
                    let key = fromMaybe "" (aicuApiKey upd)
                        model = fromMaybe (defaultModelFor provider) (aicuModel upd)
                        cfg = mkCfg key model
                    store <- newAIStore cfg mgr
                    setAIProvider store (buildProvider mgr provider cfg numCtx)
                    setAIStore app (Just store)
                    persistConfig app key model provider numCtx limit
                    pure (Right ())
    onExisting mgr store = do
        oldCfg <- getAIConfig store
        curProvider <- providerNameOf <$> getAIProvider store
        (numCtx, limit) <- applyKnobs
        let provider = fromMaybe curProvider (aicuProvider upd)
            newKey = fromMaybe (acApiKey oldCfg) (aicuApiKey upd)
            newModel = fromMaybe (acModel oldCfg) (aicuModel upd)
            newCfg = oldCfg{acApiKey = newKey, acModel = newModel}
        setAIFullConfig store newCfg
        setAIProvider store (buildProvider mgr provider newCfg numCtx)
        persistConfig app newKey newModel provider numCtx limit
        pure (Right ())
    -- Resolve the two knobs from the update (falling back to the current
    -- workspace values) and write them back so the loop + next rebuild see them.
    applyKnobs = do
        curNumCtx <- readIORef (appAINumCtx app)
        curLimit <- readIORef (appAIToolLimit app)
        let numCtx = fromMaybe curNumCtx (aicuNumCtx upd)
            limit = fromMaybe curLimit (aicuToolLimit upd)
        writeIORef (appAINumCtx app) numCtx
        writeIORef (appAIToolLimit app) limit
        pure (numCtx, limit)
    mkCfg key model =
        AnthropicConfig
            { acApiKey = key
            , acModel = model
            , acBaseUrl = T.pack "https://api.anthropic.com"
            }
    defaultModelFor p
        | p == ollamaProviderId = defaultOllamaModel
        | otherwise = envAnthropicModel (appEnv app)

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

persistConfig :: App -> Text -> Text -> Text -> Int -> Int -> IO ()
persistConfig app key model provider numCtx toolLimit = do
    let configDir = envWorkDir (appEnv app) </> ".sabela"
        configFile = configDir </> "config.json"
    createDirectoryIfMissing True configDir
    BS.writeFile
        configFile
        (BS.toStrict (encode (configJson key model provider numCtx toolLimit)))

{- | The @.sabela/config.json@ wire shape. Kept as a pure function so its keys
(@anthropicKey@ / @anthropicModel@ / @provider@ / @numCtx@ / @toolLimit@ — read
back by 'readConfigFile') are pinned by 'Test.ConfigWireSpec'.
-}
configJson :: Text -> Text -> Text -> Int -> Int -> Value
configJson key model provider numCtx toolLimit =
    object
        [ "anthropicKey" .= key
        , "anthropicModel" .= model
        , "provider" .= provider
        , "numCtx" .= numCtx
        , "toolLimit" .= toolLimit
        ]

newApp ::
    FilePath -> Set Text -> Maybe Manager -> Maybe Text -> [FilePath] -> IO App
newApp workDir globalDeps mHttpMgr mAiToken localPkgs = do
    absWork <- canonicalizePath workDir
    localAbs <- mapM canonicalizePath localPkgs
    tmpBase <- getCanonicalTemporaryDirectory
    tmpDir <- createTempDirectory tmpBase "sabela-server"
    debug <- isJust <$> lookupEnv "SABELA_DEBUG"
    (mApiKey, mSavedModel, mSavedProvider, mNumCtx, mToolLimit) <-
        resolveConfig absWork
    envModel <- lookupEnv "ANTHROPIC_MODEL"
    let numCtx0 = fromMaybe defaultNumCtx mNumCtx
        toolLimit0 = fromMaybe defaultToolLimit mToolLimit
        provider = fromMaybe "anthropic" mSavedProvider
        isOllama = provider == ollamaProviderId
        defaultModel
            | isOllama = T.unpack defaultOllamaModel
            | otherwise = "claude-sonnet-4-20250514"
        modelPref
            | isOllama = fmap T.unpack mSavedModel
            | otherwise = envModel <|> fmap T.unpack mSavedModel
        apiModel = fromMaybe defaultModel modelPref
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
    mAIStore <- case mHttpMgr of
        Just mgr
            | isOllama || isJust mApiKey -> do
                let cfg =
                        AnthropicConfig
                            { acApiKey = maybe "" T.pack mApiKey
                            , acModel = T.pack apiModel
                            , acBaseUrl = T.pack "https://api.anthropic.com"
                            }
                store <- newAIStore cfg mgr
                setAIProvider store (buildProvider mgr provider cfg numCtx0)
                pure (Just store)
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
        <*> newIORef numCtx0
        <*> newIORef toolLimit0

{- | Resolve API key + saved model + saved provider. Env ANTHROPIC_API_KEY wins
for the key; provider is absent in pre-provider config files (⇒ anthropic).
-}

-- | A JSON value as an @Int@ (via aeson's own decoder); 'Nothing' if not integral.
jsonInt :: Value -> Maybe Int
jsonInt v = case fromJSON v of
    Success i -> Just i
    Error _ -> Nothing

resolveConfig ::
    FilePath ->
    IO (Maybe String, Maybe Text, Maybe Text, Maybe Int, Maybe Int)
resolveConfig workDir = do
    mEnv <- lookupEnv "ANTHROPIC_API_KEY"
    (fileKey, fileModel, fileProvider, fileNumCtx, fileToolLimit) <-
        readConfigFile workDir
    pure (mEnv <|> fileKey, fileModel, fileProvider, fileNumCtx, fileToolLimit)

readConfigFile ::
    FilePath ->
    IO (Maybe String, Maybe Text, Maybe Text, Maybe Int, Maybe Int)
readConfigFile workDir = do
    let configFile = workDir </> ".sabela" </> "config.json"
    exists <- doesFileExist configFile
    if not exists
        then pure (Nothing, Nothing, Nothing, Nothing, Nothing)
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
                        provider = case KM.lookup (Key.fromText "provider") obj of
                            Just (String s) | not (T.null s) -> Just s
                            _ -> Nothing
                        intField k = KM.lookup (Key.fromText k) obj >>= jsonInt
                     in pure
                            ( key
                            , model
                            , provider
                            , intField "numCtx"
                            , intField "toolLimit"
                            )
                _ -> pure (Nothing, Nothing, Nothing, Nothing, Nothing)
