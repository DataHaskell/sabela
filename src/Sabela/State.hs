{-# LANGUAGE OverloadedStrings #-}

{- | The composition root: build the 'App' record from the work directory and
runtime config. The record itself, the AI-config subsystem, and the config file
live in the @Sabela.State.*@ submodules this re-exports, so importers keep
seeing one 'Sabela.State' surface.
-}
module Sabela.State (
    newApp,

    -- * Re-exports for convenience
    module Sabela.State.App,
    module Sabela.State.AIConfig,
    module Sabela.State.Config,
    module Sabela.State.Environment,
    module Sabela.State.EventBus,
    module Sabela.State.NotebookStore,
    module Sabela.State.SessionManager,
    module Sabela.State.DependencyTracker,
    module Sabela.State.WidgetStore,
    module Sabela.State.BridgeStore,
) where

import Control.Applicative ((<|>))
import Control.Concurrent.MVar (newMVar)
import Data.IORef (newIORef)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)
import System.Directory (canonicalizePath)
import System.Environment (lookupEnv)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

import Sabela.AI.Store (AIStore, newAIStore, setAIProvider)
import Sabela.Anthropic.Types (AnthropicConfig (..))
import Sabela.State.AIConfig
import Sabela.State.App
import Sabela.State.BridgeStore
import Sabela.State.Config
import Sabela.State.DependencyTracker
import Sabela.State.Environment
import Sabela.State.EventBus
import Sabela.State.NotebookStore
import Sabela.State.SessionManager
import Sabela.State.WidgetStore

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
    mAIStore <- initialAIStore mHttpMgr provider isOllama mApiKey apiModel numCtx0
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
        <*> newIORef Nothing
        <*> newIORef numCtx0
        <*> newIORef toolLimit0

{- | The AI store to start with: only when there is an HTTP manager AND the
provider is usable (Ollama needs no key; Anthropic needs one). 'Nothing'
otherwise — the settings modal configures it later via 'updateAIConfig'.
-}
initialAIStore ::
    Maybe Manager ->
    Text ->
    Bool ->
    Maybe String ->
    String ->
    Int ->
    IO (Maybe AIStore)
initialAIStore mHttpMgr provider isOllama mApiKey apiModel numCtx = case mHttpMgr of
    Just mgr
        | isOllama || isJust mApiKey -> do
            let cfg =
                    AnthropicConfig
                        { acApiKey = maybe "" T.pack mApiKey
                        , acModel = T.pack apiModel
                        , acBaseUrl = "https://api.anthropic.com"
                        }
            store <- newAIStore cfg mgr
            setAIProvider store (buildProvider mgr provider cfg numCtx)
            pure (Just store)
    _ -> pure Nothing
