{-# LANGUAGE OverloadedStrings #-}

module Test.StateQuerySpec (stateQuerySpec) where

import Control.Monad (forM_)
import Data.Aeson (Value, object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.HistoryGuard (guardDiscover, newSearchLedger)
import Siza.Agent.ToolRoute (isStateQuery, stateQueryTools)
import Siza.Agent.Tools (renderOutcome)

answerFor :: Text -> Int -> Value
answerFor "list_cells" n =
    object ["cells" .= [object ["id" .= (0 :: Int), "source" .= tick n]]]
answerFor _ n = object ["state" .= ("idle" :: Text), "elapsedMs" .= n]

tick :: Int -> Text
tick n = "print " <> T.pack (show n)

runRepeats :: Text -> Int -> IO ([Text], Int)
runRepeats tool n = do
    ledger <- newSearchLedger
    calls <- newIORef (0 :: Int)
    let inner tc = do
            modifyIORef' calls (+ 1)
            i <- readIORef calls
            pure (Right (ToolOk (answerFor (tcName tc) i)))
    outs <-
        mapM
            (const (renderOutcome <$> guardDiscover ledger inner (ToolCall tool (object []))))
            [1 .. n]
    (,) outs <$> readIORef calls

stateQuerySpec :: Spec
stateQuerySpec = describe "state queries are exempt from dedup (G8.8)" $ do
    it "deduped-state-query: N identical calls return N full payloads" $
        forM_ stateQueryTools $ \tool -> do
            (outs, calls) <- runRepeats tool 4
            calls `shouldBe` 4
            length outs `shouldBe` 4

    it "no answer is ever elided to a back-reference" $
        forM_ stateQueryTools $ \tool -> do
            (outs, _) <- runRepeats tool 4
            forM_ outs $ \o ->
                o `shouldNotSatisfy` T.isInfixOf "same as your last"

    it "each answer carries its own changing payload, not the first one" $ do
        (outs, _) <- runRepeats "kernel_status" 3
        outs `shouldSatisfy` \os -> length os == 3
        last outs `shouldNotSatisfy` T.isInfixOf "\"elapsedMs\":1,"

    it "the category is closed over the state tools the plan names" $ do
        forM_ ["kernel_status", "await_idle", "list_cells", "list_bindings"] $
            \t -> isStateQuery t `shouldBe` True
        isStateQuery "insert_cell" `shouldBe` False
        isStateQuery "discover" `shouldBe` False
