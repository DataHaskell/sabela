{-# LANGUAGE OverloadedStrings #-}

{- | @await_idle@ and the admission bounce must share one notion of idle.
live_test8: @await_idle@ answered @{"waited":"idle"}@ and the very next
@insert_cell@ bounced with "cell 0 has been executing for 73356ms" — the
documented barrier was a no-op, because await sampled @sbBusy@ while
admission consulted the write registry, which the cascade does not clear
between cells.
-}
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
        -- Exactly the live_test8 state: the run-lock is free (no session at
        -- all here) but a write is still in flight, so the next insert bounces.
        _ <- registerWrite (aiWriteReg store) "cell-0" 0
        outcome <- execAwaitIdle app store
        waitedTag (toolOutcomeValue outcome) `shouldSatisfy` (/= Just "idle")

    it "post-condition: an idle answer admits the very next write (G8.2)" $ do
        (app, store) <- fixture
        outcome <- execAwaitIdle app store
        waitedTag (toolOutcomeValue outcome) `shouldBe` Just "idle"
        -- The barrier is only real if the write that follows is admitted by
        -- the SAME evidence await consulted; live_test8's was not.
        verdict <-
            busyVerdict
                <$> busyEvidenceNow app store (haskellKernelOccupied app)
        verdict `shouldBe` AdmitNow

    {- The asymmetry is deliberate and one-directional: await is STRICTER
    than the occupancy window, because the running-write bounce lives in the
    write gate rather than in 'busyVerdict'. Only await-says-idle-then-busy
    is a defect; await refusing idle while admission would allow is safe. -}
    it "await is stricter than the occupancy window, never the reverse" $ do
        (app, store) <- fixture
        _ <- registerWrite (aiWriteReg store) "cell-0" 0
        waited <- waitedTag . toolOutcomeValue <$> execAwaitIdle app store
        waited `shouldSatisfy` (/= Just "idle")
