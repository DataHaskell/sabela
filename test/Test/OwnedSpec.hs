{-# LANGUAGE OverloadedStrings #-}

{- | Ownership of the cells an agent turn wrote: what gets owned, when the accept
gate stops vs re-enters, and when a concurrent edit makes ownership stale.
-}
module Test.OwnedSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Test.Hspec

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Health (Health, healthOfResult)
import Sabela.AI.Owned (
    MutationEvent (..),
    StopDecision (..),
    bestFailing,
    ownershipStale,
    recordMutation,
    stopDecision,
 )
import Sabela.AI.Types (ExecutionResult (..))

cleanH :: Health
cleanH = healthOfResult (Right (ExecutionResult [] Nothing [] []))

redH :: Health
redH = healthOfResult (Right (ExecutionResult [] (Just "boom") [] []))

spec :: Spec
spec = describe "Sabela.AI.Owned" $ do
    describe "recordMutation" $ do
        it "owns a cell a notebook-acting tool wrote" $
            Map.keys (recordMutation (MutationEvent InsertCell 1 "x = 1" cleanH) Map.empty)
                `shouldBe` [1]
        it "records nothing for a read/query tool" $
            recordMutation (MutationEvent ReadCell 1 "x = 1" cleanH) Map.empty
                `shouldBe` Map.empty
        it "the latest mutation of a cell wins" $ do
            let m0 = recordMutation (MutationEvent InsertCell 1 "old" redH) Map.empty
                m1 = recordMutation (MutationEvent ReplaceCellSource 1 "new" cleanH) m0
            stopDecision m1 `shouldBe` Stop

    describe "stopDecision" $ do
        it "stops when every owned cell is clean" $
            stopDecision
                (recordMutation (MutationEvent InsertCell 1 "x=1" cleanH) Map.empty)
                `shouldBe` Stop
        it "re-enters the red owned cells" $ do
            let m =
                    recordMutation (MutationEvent InsertCell 2 "bad" redH) $
                        recordMutation (MutationEvent InsertCell 1 "x=1" cleanH) Map.empty
            stopDecision m `shouldBe` Reenter [2]

    describe "ownershipStale" $ do
        it "is stale when the current source differs from what was committed" $
            ownershipStale
                1
                "x = 2"
                (recordMutation (MutationEvent InsertCell 1 "x = 1" cleanH) Map.empty)
                `shouldBe` True
        it "is not stale when the source is unchanged" $
            ownershipStale
                1
                "x = 1"
                (recordMutation (MutationEvent InsertCell 1 "x = 1" cleanH) Map.empty)
                `shouldBe` False
        it "is not stale for a cell the turn does not own" $
            ownershipStale 9 "anything" Map.empty `shouldBe` False

    describe "bestFailing" $ do
        it "is Nothing when all owned cells are clean" $
            bestFailing (recordMutation (MutationEvent InsertCell 1 "x=1" cleanH) Map.empty)
                `shouldBe` Nothing
        it "surfaces a red owned cell" $
            bestFailing (recordMutation (MutationEvent InsertCell 1 "bad" redH) Map.empty)
                `shouldSatisfy` isJust
