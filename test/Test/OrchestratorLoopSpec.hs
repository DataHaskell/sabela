{-# LANGUAGE OverloadedStrings #-}

{- | Drives 'agenticLoop' against a scripted fake 'ModelProvider' — no network,
no live model. Proves the rewired loop actually calls the port, maps a
'Completion' onto the turn phase + history, and fails cleanly on a provider
error. (Tool-dispatch + live streaming are covered by the end-to-end smoke.)
-}
module Test.OrchestratorLoopSpec (spec) where

import Control.Concurrent.STM (TChan, atomically, readTVarIO, tryReadTChan)
import Data.IORef (readIORef)
import qualified Data.Set as Set
import Data.Text (Text)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Test.Hspec

import Sabela.AI.Orchestrator.Loop (agenticLoop)
import Sabela.AI.Store
import Sabela.AI.Types (Turn (..), TurnPhase (..), newTurn)
import Sabela.Anthropic.Types (AnthropicConfig (..))
import Sabela.Handlers (ReactiveNotebook (..))
import Sabela.LLM.Completion (Completion (..), StopCondition (..))
import Sabela.LLM.Message (ContentPart (..), Message (..), Role (..))
import Sabela.LLM.Provider (ModelProvider (..), ProviderCaps (..))
import Sabela.Model (NotebookEvent (..))
import Sabela.Server (newApp)
import Sabela.State (App (..))
import Sabela.State.EventBus (subscribeBroadcast)

-- | A provider that always returns the same scripted result.
constProvider :: Either Text Completion -> ModelProvider
constProvider r =
    ModelProvider
        { mpName = "fake"
        , mpCaps = ProviderCaps False False False
        , mpComplete = \_ _ _ -> pure r
        }

inertRn :: ReactiveNotebook
inertRn =
    ReactiveNotebook
        { rnCellEdit = \_ _ -> pure ()
        , rnRunCell = \_ -> pure ()
        , rnRunCellForced = \_ -> pure ()
        , rnRunAll = pure ()
        , rnReset = pure ()
        , rnRestartKernel = pure ()
        , rnWidgetCell = \_ -> pure ()
        }

setup :: ModelProvider -> IO (App, AIStore, Turn)
setup provider = do
    mgr <- newManager defaultManagerSettings
    app <- newApp "." Set.empty (Just mgr) Nothing []
    let cfg = AnthropicConfig "" "placeholder" "https://api.anthropic.com"
    store <- newAIStore cfg mgr
    setAIProvider store provider
    turn <- newTurn (aiNextTurnId store)
    setCurrentTurn store turn
    appendMessage store (Message User [TextPart "hi"])
    pure (app, store, turn)

spec :: Spec
spec = describe "agenticLoop (fake ModelProvider)" $ do
    it "completes a text turn: appends the assistant reply, one iteration" $ do
        (app, store, turn) <-
            setup (constProvider (Right (Completion [TextPart "done"] Done mempty)))
        agenticLoop app store inertRn turn
        phase <- readTVarIO (turnPhase turn)
        case phase of
            TurnComplete _ -> pure ()
            other -> expectationFailure ("expected TurnComplete, got " <> show other)
        iters <- readIORef (turnIterations turn)
        iters `shouldBe` 1
        msgs <- getMessages store
        map msgRole msgs `shouldBe` [User, Assistant]
        msgParts (last msgs) `shouldBe` [TextPart "done"]

    it "fails the turn when the provider errors" $ do
        (app, store, turn) <- setup (constProvider (Left "boom"))
        agenticLoop app store inertRn turn
        phase <- readTVarIO (turnPhase turn)
        case phase of
            TurnFailed e -> e `shouldBe` "boom"
            other -> expectationFailure ("expected TurnFailed, got " <> show other)

    it "broadcasts the assistant text for a non-streaming provider (Ollama-style)" $ do
        (app, store, turn) <-
            setup (constProvider (Right (Completion [TextPart "hi there"] Done mempty)))
        chan <- subscribeBroadcast (appEvents app)
        agenticLoop app store inertRn turn
        evs <- drainChan chan
        [t | EvChatTextDelta _ t <- evs] `shouldContain` ["hi there"]

drainChan :: TChan a -> IO [a]
drainChan ch = go []
  where
    go acc = do
        m <- atomically (tryReadTChan ch)
        case m of
            Just x -> go (x : acc)
            Nothing -> pure (reverse acc)
