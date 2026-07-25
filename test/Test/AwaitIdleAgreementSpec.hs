{-# LANGUAGE OverloadedStrings #-}

{- | @await_idle@ and the admission bounce must share one notion of idle.
live_test8: @await_idle@ answered @{"waited":"idle"}@ and the very next
@insert_cell@ bounced with "cell 0 has been executing for 73356ms" — the
documented barrier was a no-op, because await sampled @sbBusy@ while
admission consulted the write registry, which the cascade does not clear
between cells.
-}
module Test.AwaitIdleAgreementSpec (spec) where

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson (Value (..))
import qualified Data.Set as Set
import Data.Text (Text)
import Test.Hspec

import Sabela.AI.Capabilities.Kernel (execAwaitIdle)
import Sabela.AI.Store (AIStore (..), newAIStore)
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.AI.WriteRegistry (registerWrite)
import Sabela.Anthropic.Types (AnthropicConfig (..))
import Sabela.Server (newApp)
import Sabela.State (App)
import Network.HTTP.Client (defaultManagerSettings, newManager)

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

    it "never claims idle while a registered write is still running" $ do
        (app, store) <- fixture
        -- Exactly the live_test8 state: the run-lock is free (no session at
        -- all here) but a write is still in flight, so the next insert bounces.
        _ <- registerWrite (aiWriteReg store) "cell-0" 0
        outcome <- execAwaitIdle app store
        waitedTag (toolOutcomeValue outcome) `shouldSatisfy` (/= Just "idle")
