{-# LANGUAGE OverloadedStrings #-}

module Test.UsageMergeSpec (spec) where

import qualified Data.Maybe
import Test.Hspec

import Sabela.Anthropic.Types (Usage (..))

-- Mirror of Orchestrator.mergeUsage for unit testing. Keep in sync.
mergeUsage :: Usage -> Usage -> Usage
mergeUsage a b =
    Usage
        { uInputTokens = uInputTokens a + uInputTokens b
        , uOutputTokens = uOutputTokens a + uOutputTokens b
        , uCacheCreationInputTokens =
            addMaybeInt (uCacheCreationInputTokens a) (uCacheCreationInputTokens b)
        , uCacheReadInputTokens =
            addMaybeInt (uCacheReadInputTokens a) (uCacheReadInputTokens b)
        }
  where
    addMaybeInt :: Maybe Int -> Maybe Int -> Maybe Int
    addMaybeInt Nothing Nothing = Nothing
    addMaybeInt x y = Just (Data.Maybe.fromMaybe 0 x + Data.Maybe.fromMaybe 0 y)

spec :: Spec
spec = do
    describe "mergeUsage" $ do
        it "adds token counts componentwise" $ do
            let a = Usage 100 50 (Just 10) (Just 20)
                b = Usage 200 75 (Just 30) (Just 40)
                r = mergeUsage a b
            uInputTokens r `shouldBe` 300
            uOutputTokens r `shouldBe` 125
            uCacheCreationInputTokens r `shouldBe` Just 40
            uCacheReadInputTokens r `shouldBe` Just 60

        it "keeps cache fields as Nothing when both sides never report them" $ do
            let a = Usage 10 5 Nothing Nothing
                b = Usage 20 8 Nothing Nothing
                r = mergeUsage a b
            uCacheCreationInputTokens r `shouldBe` Nothing
            uCacheReadInputTokens r `shouldBe` Nothing

        it "treats a missing cache field as 0 once the other side reports" $ do
            let a = Usage 10 5 Nothing Nothing
                b = Usage 20 8 (Just 7) (Just 3)
                r = mergeUsage a b
            uCacheCreationInputTokens r `shouldBe` Just 7
            uCacheReadInputTokens r `shouldBe` Just 3

        it "is associative on a 3-iteration turn" $ do
            let u1 = Usage 1000 100 (Just 50) Nothing
                u2 = Usage 500 60 Nothing (Just 900)
                u3 = Usage 300 40 Nothing (Just 800)
                left = mergeUsage (mergeUsage u1 u2) u3
                right = mergeUsage u1 (mergeUsage u2 u3)
            uInputTokens left `shouldBe` uInputTokens right
            uOutputTokens left `shouldBe` uOutputTokens right
            uCacheCreationInputTokens left `shouldBe` uCacheCreationInputTokens right
            uCacheReadInputTokens left `shouldBe` uCacheReadInputTokens right
