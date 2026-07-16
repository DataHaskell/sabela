{-# LANGUAGE OverloadedStrings #-}

{- | The shared repair-search core both the product and eval paths adapt to:
first-verified backtracking ('firstJustM') and hole-fit candidate generation.
-}
module Test.RepairEngineSpec (spec) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Test.Hspec

import Sabela.AI.HoleRepair (holeFitRewrites)
import Sabela.AI.Repair (firstJustM)

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
