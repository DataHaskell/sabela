{-# LANGUAGE OverloadedStrings #-}

module Sabela.State (
    App (..),
    newApp,
    getAIStore,
    setAIStore,
    configureAI,

    -- * Re-exports for convenience
    module Sabela.State.Environment,
    module Sabela.State.EventBus,
    module Sabela.State.NotebookStore,
    module Sabela.State.SessionManager,
    module Sabela.State.DependencyTracker,
    module Sabela.State.WidgetStore,
    module Sabela.State.BridgeStore,
) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Data.Aeson (Value (..), eitherDecodeStrict, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)
import Sabela.AI.Store (AIStore, newAIStore)
import Sabela.Anthropic.Types (AnthropicConfig (..))
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
    , appAI :: MVar (Maybe AIStore)
    , appHttpMgr :: Maybe Manager
    }

-- | Read the current AI store (if configured).
getAIStore :: App -> IO (Maybe AIStore)
getAIStore = readMVar . appAI

-- | Set the AI store.
setAIStore :: App -> Maybe AIStore -> IO ()
setAIStore app val = modifyMVar_ (appAI app) (const (pure val))

{- | Configure AI with an API key at runtime.
Writes the key to <workdir>/.sabela/config.json and initializes the AIStore.
-}
configureAI :: App -> Text -> IO (Either Text ())
configureAI app apiKey = case appHttpMgr app of
    Nothing -> pure (Left "No HTTP manager available")
    Just mgr -> do
        let model = envAnthropicModel (appEnv app)
            cfg =
                AnthropicConfig
                    { acApiKey = apiKey
                    , acModel = model
                    , acBaseUrl = T.pack "https://api.anthropic.com"
                    }
        store <- newAIStore cfg mgr
        setAIStore app (Just store)
        -- Persist to config file
        let configDir = envWorkDir (appEnv app) </> ".sabela"
            configFile = configDir </> "config.json"
            json = encode (object ["anthropicKey" .= apiKey])
        createDirectoryIfMissing True configDir
        BS.writeFile configFile (BS.toStrict json)
        pure (Right ())

newApp :: FilePath -> Set Text -> Maybe Manager -> IO App
newApp workDir globalDeps mHttpMgr = do
    absWork <- canonicalizePath workDir
    tmpBase <- getCanonicalTemporaryDirectory
    tmpDir <- createTempDirectory tmpBase "sabela-server"
    debug <- isJust <$> lookupEnv "SABELA_DEBUG"
    mApiKey <- resolveApiKey absWork
    apiModel <-
        fromMaybe "claude-sonnet-4-20250514"
            <$> lookupEnv "ANTHROPIC_MODEL"
    let env =
            Environment
                { envWorkDir = absWork
                , envTmpDir = tmpDir
                , envGlobalDeps = globalDeps
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
    App env
        <$> newNotebookStore
        <*> newEventBus
        <*> newSessionManager
        <*> newDependencyTracker
        <*> newWidgetStore
        <*> newBridgeStore
        <*> pure aiVar
        <*> pure mHttpMgr

-- | Resolve API key: env var takes priority, then config file on disk.
resolveApiKey :: FilePath -> IO (Maybe String)
resolveApiKey workDir = do
    mEnv <- lookupEnv "ANTHROPIC_API_KEY"
    case mEnv of
        Just key -> pure (Just key)
        Nothing -> readKeyFromConfig workDir

readKeyFromConfig :: FilePath -> IO (Maybe String)
readKeyFromConfig workDir = do
    let configFile = workDir </> ".sabela" </> "config.json"
    exists <- doesFileExist configFile
    if not exists
        then pure Nothing
        else do
            bs <- BS.readFile configFile
            case eitherDecodeStrict bs of
                Left _ -> pure Nothing
                Right (Object obj) ->
                    case KM.lookup (Key.fromText "anthropicKey") obj of
                        Just (String key) -> pure (Just (T.unpack key))
                        _ -> pure Nothing
                Right _ -> pure Nothing

