{-# LANGUAGE OverloadedStrings #-}

{- | Provider selection through 'updateAIConfig': choosing Ollama builds an
Ollama-backed provider (no API key), switching back rebuilds Anthropic, and
first-time Anthropic still requires a key. Uses a temp workdir so the persisted
@.sabela/config.json@ write is isolated.
-}
module Test.ProviderSelectSpec (spec) where

import qualified Data.Set as Set
import Data.Text (Text)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Sabela.AI.Store (getAIProvider)
import Sabela.LLM.Provider (mpName)
import Sabela.Server (newApp)
import Sabela.State (AIConfigUpdate (..), App, getAIStore, updateAIConfig)

withApp :: (App -> IO a) -> IO a
withApp k = withSystemTempDirectory "sabela-provsel" $ \dir -> do
    mgr <- newManager defaultManagerSettings
    app <- newApp dir Set.empty (Just mgr) Nothing []
    k app

activeProvider :: App -> IO (Maybe Text)
activeProvider app = do
    mStore <- getAIStore app
    traverse (fmap mpName . getAIProvider) mStore

pick :: Maybe Text -> Maybe Text -> Maybe Text -> AIConfigUpdate
pick k m p =
    AIConfigUpdate
        { aicuApiKey = k
        , aicuModel = m
        , aicuProvider = p
        , aicuNumCtx = Nothing
        , aicuToolLimit = Nothing
        }

spec :: Spec
spec = describe "provider selection via updateAIConfig" $ do
    it "selecting ollama builds an Ollama-backed provider with no API key" $
        withApp $ \app -> do
            r <- updateAIConfig app (pick Nothing (Just "gpt-oss:20b") (Just "ollama"))
            r `shouldBe` Right ()
            p <- activeProvider app
            p `shouldBe` Just "ollama:gpt-oss:20b"

    it "switching back to anthropic rebuilds an Anthropic provider" $
        withApp $ \app -> do
            _ <- updateAIConfig app (pick Nothing (Just "gpt-oss:20b") (Just "ollama"))
            _ <-
                updateAIConfig app (pick (Just "sk-x") (Just "claude-y") (Just "anthropic"))
            p <- activeProvider app
            p `shouldBe` Just "anthropic"

    it "requires an api key for first-time anthropic setup" $
        withApp $ \app -> do
            r <- updateAIConfig app (pick Nothing Nothing (Just "anthropic"))
            r `shouldBe` Left "apiKey is required for first-time setup"

    it "persists the provider across a restart (new App, same workdir)" $
        withSystemTempDirectory "sabela-provpersist" $ \dir -> do
            mgr <- newManager defaultManagerSettings
            app1 <- newApp dir Set.empty (Just mgr) Nothing []
            _ <- updateAIConfig app1 (pick Nothing (Just "gpt-oss:20b") (Just "ollama"))
            app2 <- newApp dir Set.empty (Just mgr) Nothing []
            p <- activeProvider app2
            p `shouldBe` Just "ollama:gpt-oss:20b"
