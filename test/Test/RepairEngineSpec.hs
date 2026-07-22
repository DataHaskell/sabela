{-# LANGUAGE OverloadedStrings #-}

{- | The shared repair-search core both the product and eval paths adapt to:
first-verified backtracking ('firstJustM') and hole-fit candidate generation.
-}
module Test.RepairEngineSpec (spec) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Test.Hspec

import Sabela.AI.Capabilities.Edit.Run (parseRepairBudget, repairTierOrder)
import Sabela.AI.HoleRepair (holeFitRewrites)
import Sabela.AI.Repair (firstJustM, interleave)

-- | Run 'firstJustM' over @xs@, returning its result and the candidates visited.
traced :: (Int -> Maybe String) -> [Int] -> IO (Maybe (Int, String), [Int])
traced f xs = do
    seen <- newIORef []
    r <- firstJustM (\x -> modifyIORef' seen (++ [x]) >> pure (f x)) xs
    (,) r <$> readIORef seen

spec :: Spec
spec = describe "Sabela.AI.Repair (shared repair core)" $ do
    describe "firstJustM" $ do
        it "returns the first candidate that hits, with its value, and stops there" $
            traced keep [1, 2, 3, 4]
                `shouldReturn` (Just (3, "hit-3"), [1, 2, 3])
        it "is Nothing when no candidate passes (visiting all)" $
            traced (const Nothing) [1, 2, 3]
                `shouldReturn` (Nothing, [1, 2, 3])
        it "is Nothing on an empty candidate list" $
            traced keep [] `shouldReturn` (Nothing, [])

    describe "interleave — candidate diversity under a small execution cap" $ do
        -- The cap must sample across GROUPS (names, tiers) before trying a
        -- second variant of the same one: three executions should cover three
        -- different problems, not three spellings of one.
        it "round-robins across groups" $
            interleave [["a1", "a2", "a3"], ["b1"], ["c1", "c2"]]
                `shouldBe` ["a1", "b1", "c1", "a2", "c2", "a3"]
        it "is flat concat for a single group" $
            interleave [["a1", "a2"]] `shouldBe` ["a1", "a2"]
        it "drops empty groups" $
            interleave [[], ["b1"], []] `shouldBe` ["b1"]

    describe "parseRepairBudget — the cascade's wall-clock allowance" $ do
        it "defaults when unset" $
            parseRepairBudget Nothing `shouldBe` 150
        it "reads an explicit seconds value" $
            parseRepairBudget (Just "60") `shouldBe` 60
        it "falls back to the default on junk" $
            parseRepairBudget (Just "fast") `shouldBe` 150

    describe "repairTierOrder (the list the cascade driver iterates)"
        $ it
            "runs the compile-only speculative tier before the session-committing resolvers"
        $ repairTierOrder
            `shouldBe` [ "firstFix"
                       , "moduleDep"
                       , "speculative"
                       , "resolvers"
                       , "restart"
                       ]

    describe "holeFitRewrites" $ do
        it "substitutes each plain fit for the wrong name, dropping no-ops" $
            holeFitRewrites
                "getCol"
                "Valid hole fits include\n  columnAsList :: a\n  toColumn :: a"
                "total = sum (getCol df)"
                `shouldBe` [ "total = sum (columnAsList df)"
                           , "total = sum (toColumn df)"
                           ]
        it "is empty when no fit changes the source" $
            holeFitRewrites "getCol" "no fits here" "total = 1" `shouldBe` []
  where
    keep n = if n == 3 then Just ("hit-" ++ show n) else Nothing
