{-# LANGUAGE OverloadedStrings #-}

module Sabela.State.AIConfig (
    AIConfigUpdate (..),
    configureAI,
    updateAIConfig,
    getAINumCtx,
    getAIToolLimit,
    persistConfig,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BS
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Sabela.AI.Store (
    getAIConfig,
    getAIProvider,
    newAIStore,
    setAIFullConfig,
    setAIProvider,
 )
import Sabela.Anthropic.Types (AnthropicConfig (..))
import Sabela.State.App (App (..), getAIStore, setAIStore)
import Sabela.State.Config (
    buildProvider,
    configJson,
    defaultOllamaModel,
    ollamaProviderId,
    providerNameOf,
 )
import Sabela.State.Environment (Environment (..))

data AIConfigUpdate = AIConfigUpdate
    { aicuApiKey :: Maybe Text
    , aicuModel :: Maybe Text
    , aicuProvider :: Maybe Text
    , aicuNumCtx :: Maybe Int
    , aicuToolLimit :: Maybe Int
    }

getAINumCtx :: App -> IO Int
getAINumCtx = readIORef . appAINumCtx

getAIToolLimit :: App -> IO Int
getAIToolLimit = readIORef . appAIToolLimit

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

persistConfig :: App -> Text -> Text -> Text -> Int -> Int -> IO ()
persistConfig app key model provider numCtx toolLimit = do
    let configDir = envWorkDir (appEnv app) </> ".sabela"
        configFile = configDir </> "config.json"
    createDirectoryIfMissing True configDir
    BS.writeFile
        configFile
        (encode (configJson key model provider numCtx toolLimit))
