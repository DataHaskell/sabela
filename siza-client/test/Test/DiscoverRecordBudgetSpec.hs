{-# LANGUAGE OverloadedStrings #-}

{- | R3.9: the envelope budget holds at EVERY emitting seam, including the
ledger-record annotations (worldChange note, steer text) applied after the
tool's own bound — the run-20260720-195038 topMonth-on 2631b breach class.
-}
module Test.DiscoverRecordBudgetSpec (discoverRecordBudgetSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Data.IORef (atomicModifyIORef')
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.Envelope (envelopeCharBudget, envelopeChars)
import Siza.Agent.Discover.History (ledgerWorldChanged)
import Siza.Agent.Discover.HistoryGuard (guardDiscover, newSearchLedger)

-- | A found envelope tuned to sit just under the budget before annotation.
nearBudgetEnvelope :: Value
nearBudgetEnvelope =
    object
        [ "query" .= ("maxBy" :: Text)
        , "state" .= ("found" :: Text)
        , "shown" .= (23 :: Int)
        , "omitted" .= (0 :: Int)
        , "hits" .= [hit i | i <- [1 .. 23 :: Int]]
        ]
  where
    hit i =
        object
            [ "name" .= ("candidate" <> T.pack (show i))
            , "module" .= ("Data.Long.Module.Path" :: Text)
            , "package" .= ("some-package" :: Text)
            , "install" .= ("installed" :: Text)
            ]

discoverRecordBudgetSpec :: Spec
discoverRecordBudgetSpec =
    describe "post-record envelope budget (R3.9, 2631b breach class)" $ do
        it "the fixture sits under the budget before annotation" $
            envelopeChars nearBudgetEnvelope
                `shouldSatisfy` \n ->
                    n <= envelopeCharBudget && n > envelopeCharBudget - 400
        it "a pending world-change note never pushes the answer over budget" $ do
            ref <- newSearchLedger
            let inner _ = pure (Right (ToolOk nearBudgetEnvelope))
                callQ q = ToolCall "discover" (object ["query" .= q])
            -- A prior recorded answer, so the world change legally announces
            -- (R10-T4: a first-of-session change has nothing to stale).
            _ <- guardDiscover ref inner (callQ ("seedq" :: Text))
            atomicModifyIORef' ref (\l -> (ledgerWorldChanged l, ()))
            r <- guardDiscover ref inner (callQ ("maxBy" :: Text))
            case r of
                Right (ToolOk v) -> do
                    envelopeChars v `shouldSatisfy` (<= envelopeCharBudget)
                    -- The annotation survives; the bound sheds hits instead.
                    hasKey "worldChange" v `shouldBe` True
                other -> expectationFailure (show other)
  where
    hasKey k (Object o) = KM.member k o
    hasKey _ _ = False
