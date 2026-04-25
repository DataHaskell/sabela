{-# LANGUAGE OverloadedStrings #-}

module Test.AiHandlesSpec (spec) where

import qualified Data.Maybe
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Handles (
    HandleId (..),
    LargeResult (..),
    cleanOutput,
    clearHandles,
    grepLines,
    headLines,
    lookupHandle,
    newHandleStore,
    sliceLines,
    storeLargeResult,
    tailLines,
 )

spec :: Spec
spec = do
    describe "storeLargeResult" $ do
        it "inlines small payloads" $ do
            store <- newHandleStore
            r <- storeLargeResult store "short"
            case r of
                Left cleaned -> cleaned `shouldBe` "short"
                Right _ -> expectationFailure "small payload should inline"

        it "stores large payloads under a handle" $ do
            store <- newHandleStore
            let big = T.unlines (map (T.pack . show) [1 :: Int .. 100])
            r <- storeLargeResult store big
            case r of
                Left _ -> expectationFailure "large payload should get a handle"
                Right (HandleId hid, summary, nLines, _) -> do
                    hid `shouldSatisfy` T.isPrefixOf "lr_"
                    nLines `shouldBe` 100
                    summary `shouldSatisfy` T.isInfixOf "more lines"

        it "clearHandles drops stored entries" $ do
            store <- newHandleStore
            let big = T.unlines (map (T.pack . show) [1 :: Int .. 100])
            Right (hid, _, _, _) <- storeLargeResult store big
            mLr <- lookupHandle store hid
            isJust mLr `shouldBe` True
            clearHandles store
            mLr2 <- lookupHandle store hid
            isJust mLr2 `shouldBe` False

    describe "drill-down ops" $ do
        let lr =
                LargeResult
                    { lrLines = map (T.pack . show) [1 :: Int .. 10]
                    , lrTotalLines = 10
                    , lrTotalBytes = 30
                    }

        it "headLines returns prefix" $
            headLines 3 lr `shouldBe` ["1", "2", "3"]

        it "tailLines returns suffix" $
            tailLines 3 lr `shouldBe` ["8", "9", "10"]

        it "sliceLines is 1-based inclusive" $
            sliceLines 4 6 lr `shouldBe` ["4", "5", "6"]

        it "grepLines returns matching line numbers" $
            grepLines "7" lr `shouldBe` [(7, "7")]

    describe "cleanOutput" $ do
        it "strips ANSI color sequences" $
            cleanOutput "\ESC[31mred\ESC[0m text" `shouldBe` "red text"

        it "collapses consecutive duplicate lines" $ do
            let raw = T.intercalate "\n" ["a", "b", "b", "b", "c"]
            cleanOutput raw `shouldSatisfy` T.isInfixOf "\xd7"
  where
    isJust = Data.Maybe.isJust
