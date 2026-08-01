{-# LANGUAGE OverloadedStrings #-}

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
import Test.DiscoverFixtures (hitsOf)

{- | The widest answer that still fits, found rather than hard-coded: a
fixture pinned to one hit count stops testing the breach the moment the
budget moves.
-}
nearBudgetEnvelope :: Value
nearBudgetEnvelope = last (takeWhile fits (map envelopeOf [1 ..]))
  where
    fits v = envelopeChars v <= envelopeCharBudget

envelopeOf :: Int -> Value
envelopeOf k =
    object
        [ "query" .= ("maxBy" :: Text)
        , "state" .= ("found" :: Text)
        , "shown" .= k
        , "omitted" .= (0 :: Int)
        , "hits" .= [hit i | i <- [1 .. k]]
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
        it "the fixture is the widest answer that still fits" $ do
            envelopeChars nearBudgetEnvelope
                `shouldSatisfy` (<= envelopeCharBudget)
            let wider = envelopeOf (1 + length (hitsOf nearBudgetEnvelope))
            envelopeChars wider `shouldSatisfy` (> envelopeCharBudget)
        it "a pending world-change note never pushes the answer over budget" $ do
            ref <- newSearchLedger
            let inner _ = pure (Right (ToolOk nearBudgetEnvelope))
                callQ q = ToolCall "discover" (object ["query" .= q])
            _ <- guardDiscover ref inner (callQ ("seedq" :: Text))
            atomicModifyIORef' ref (\l -> (ledgerWorldChanged l, ()))
            r <- guardDiscover ref inner (callQ ("maxBy" :: Text))
            case r of
                Right (ToolOk v) -> do
                    envelopeChars v `shouldSatisfy` (<= envelopeCharBudget)
                    hasKey "worldChange" v `shouldBe` True
                other -> expectationFailure (show other)
  where
    hasKey k (Object o) = KM.member k o
    hasKey _ _ = False
