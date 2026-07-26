{-# LANGUAGE OverloadedStrings #-}

module Test.RtsGhcOptionsSpec (spec) where

import Data.Char (isDigit)
import Data.List (isInfixOf, isPrefixOf)
import Sabela.Session.Process (rtsGhcOptions)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

nurseryTotalMb :: Int
nurseryTotalMb = 512

nurseryFloorMb :: Int
nurseryFloorMb = 16

areaMbOf :: String -> Int
areaMbOf rendered =
    case [w | w <- words rendered, "-A" `isPrefixOf` w] of
        (w : _) -> read (takeWhile isDigit (drop 2 w))
        [] -> error ("no -A flag in: " ++ rendered)

capCases :: [Int]
capCases = [1, 2, 4, 8, 14, 64]

spec :: Spec
spec = describe "rtsGhcOptions" $ do
    describe "nursery total budget" $ do
        it "keeps the total allocation area bounded at every cap count" $
            mapM_ boundedTotal capCases

        it "divides the budget evenly until the floor engages" $ do
            areaMbOf (rtsGhcOptions 1 Nothing) `shouldBe` 512
            areaMbOf (rtsGhcOptions 4 Nothing) `shouldBe` 128
            areaMbOf (rtsGhcOptions 8 Nothing) `shouldBe` 64
            areaMbOf (rtsGhcOptions 14 Nothing) `shouldBe` 36

        it "floors -A rather than dividing to nothing on a huge box" $
            areaMbOf (rtsGhcOptions 64 Nothing) `shouldBe` nurseryFloorMb

        it "treats a nonsensical cap count as a single capability" $
            rtsGhcOptions 0 Nothing `shouldBe` rtsGhcOptions 1 Nothing

    describe "capabilities" $
        it "always pins -N to an explicit count, never a bare -N" $
            mapM_ explicitCaps capCases

    describe "heap cap" $ do
        it "caps at the conservative default -M8g when the heap is unset" $
            rtsGhcOptions 8 Nothing `shouldSatisfy` isInfixOf "-M8g"

        it "honours an explicit heap override" $
            rtsGhcOptions 8 (Just "4G") `shouldSatisfy` isInfixOf "-M4G"

        it "opts out of -M entirely on the \"0\" sentinel" $
            rtsGhcOptions 8 (Just "0") `shouldSatisfy` not . isInfixOf "-M"

    describe "the whole rendering" $
        it "pins the flags a freshly-detected 8-capability box gets" $
            rtsGhcOptions 8 Nothing
                `shouldBe` "+RTS -N8 -A64m -n4m -H1G -M8g -RTS"

boundedTotal :: Int -> IO ()
boundedTotal caps =
    (caps * areaMbOf (rtsGhcOptions caps Nothing))
        `shouldSatisfy` (<= max nurseryTotalMb (caps * nurseryFloorMb))

explicitCaps :: Int -> IO ()
explicitCaps caps = do
    rtsGhcOptions caps Nothing `shouldSatisfy` isInfixOf ("-N" ++ show caps)
    words (rtsGhcOptions caps Nothing) `shouldSatisfy` notElem "-N"
