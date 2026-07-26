{-# LANGUAGE OverloadedStrings #-}

module Test.AwaitIdleAgreementSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set
import Data.Text (Text)
import Test.Hspec

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Sabela.AI.Capabilities.Kernel (execAwaitIdle, haskellKernelOccupied)
import Sabela.AI.Capabilities.KernelHealth (busyEvidenceNow)
import Sabela.AI.KernelVocab (BusyVerdict (..), busyVerdict)
import Sabela.AI.Store (AIStore (..), newAIStore)
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.AI.WriteRegistry (registerWrite)
import Sabela.Anthropic.Types (AnthropicConfig (..))
import Sabela.Server (newApp)
import Sabela.State (App)

fixture :: IO (App, AIStore)
fixture = do
    mgr <- newManager defaultManagerSettings
    app <- newApp "." Set.empty (Just mgr) Nothing []
    store <-
        newAIStore
            AnthropicConfig
                { acApiKey = ""
                , acModel = "placeholder"
                , acBaseUrl = "https://api.anthropic.com"
                }
            mgr
    pure (app, store)

waitedTag :: Value -> Maybe Text
waitedTag (Object o) = case KM.lookup (Key.fromText "waited") o of
    Just (String s) -> Just s
    _ -> Nothing
waitedTag _ = Nothing

spec :: Spec
spec = describe "await_idle agrees with the admission bounce" $ do
    it "with no session and no running write, it is idle" $ do
        (app, store) <- fixture
        outcome <- execAwaitIdle app store
        waitedTag (toolOutcomeValue outcome) `shouldBe` Just "idle"

    it "idle-then-busy: never claims idle while a write is still running" $ do
        (app, store) <- fixture
        _ <- registerWrite (aiWriteReg store) "cell-0" 0
        outcome <- execAwaitIdle app store
        waitedTag (toolOutcomeValue outcome) `shouldSatisfy` (/= Just "idle")

    it "post-condition: an idle answer admits the very next write (G8.2)" $ do
        (app, store) <- fixture
        outcome <- execAwaitIdle app store
        waitedTag (toolOutcomeValue outcome) `shouldBe` Just "idle"
        verdict <-
            busyVerdict
                <$> busyEvidenceNow app store (haskellKernelOccupied app)
        verdict `shouldBe` AdmitNow

    it "await is stricter than the occupancy window, never the reverse" $ do
        (app, store) <- fixture
        _ <- registerWrite (aiWriteReg store) "cell-0" 0
        waited <- waitedTag . toolOutcomeValue <$> execAwaitIdle app store
        waited `shouldSatisfy` (/= Just "idle")
