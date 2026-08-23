module Test.NotebookChartSpec (spec) where

import Data.List (isInfixOf)
import Sabela.Notebook.Chart
import Sabela.Notebook.Picture
import Test.Hspec

-- | How many times a tag opens in the rendered body.
tagCount :: String -> Picture -> Int
tagCount tag = length . filter (tag `isInfixOf`) . chunks . svgBody
  where
    chunks s = case break (== '<') s of
        (_, []) -> []
        (_, _ : rest) -> let (el, more) = break (== '>') rest in el : chunks more

spec :: Spec
spec = do
    describe "barChart names the thing it draws and takes the data in hand" $ do
        let bars = [("Q1", 12), ("Q2", 18), ("Q3", 9)]
        it "draws nothing for no bars" $
            svgBody (barChart []) `shouldBe` ""
        it "draws one rectangle per bar" $
            tagCount "rect" (barChart bars) `shouldBe` 3
        it "labels every bar" $
            mapM_
                (\l -> svgBody (barChart bars) `shouldSatisfy` isInfixOf l)
                ["Q1", "Q2", "Q3"]
        it "scales bar heights to the values" $ do
            let [h1, h2, h3] = barHeights (barChart bars)
            h2 `shouldSatisfy` (> h1)
            h1 `shouldSatisfy` (> h3)
        it "survives an all-zero column without dividing by zero" $
            tagCount "rect" (barChart [("a", 0), ("b", 0)]) `shouldBe` 2
        it "survives negative values" $
            tagCount "rect" (barChart [("a", -3), ("b", 4)]) `shouldBe` 2

    describe "the chart family is canvas-free by default" $ do
        let pts = [(0, 1), (1, 4), (2, 2)]
        it "lineChart matches lineChartOn defaultCanvas" $
            svgBody (lineChart pts) `shouldBe` svgBody (lineChartOn defaultCanvas pts)
        it "barChart matches barChartOn defaultCanvas" $
            svgBody (barChart [("a", 1)]) `shouldBe` svgBody (barChartOn defaultCanvas [("a", 1)])
        it "scatterChart draws one circle per point" $
            tagCount "circle" (scatterChart pts) `shouldBe` 3
        it "an explicit canvas changes the geometry" $
            svgBody (barChartOn (Canvas 800 200) [("a", 1)])
                `shouldNotBe` svgBody (barChartOn defaultCanvas [("a", 1)])

    describe "lineChart scales points into the canvas" $ do
        it "is empty for fewer than two points" $
            svgBody (lineChart [(1, 1)]) `shouldBe` ""
        it "insets the extremes by the margin" $
            svgBody (lineChartOn (Canvas 100 100) [(0, 0), (10, 10)])
                `shouldSatisfy` isInfixOf "34,66 66,34"

    describe "histogram bins a sample" $ do
        let xs = [1, 1, 2, 2, 2, 3, 8, 9]
        it "draws one rectangle per non-empty bin" $
            tagCount "rect" (histogram 4 xs) `shouldSatisfy` (<= 4)
        it "draws nothing for an empty sample" $
            svgBody (histogram 5 []) `shouldBe` ""
        it "counts every observation exactly once" $
            sum (binCounts 4 xs) `shouldBe` length xs
        it "puts equal values in one bin" $
            binCounts 4 [5, 5, 5, 5] `shouldBe` [4, 0, 0, 0]

-- | Rectangle heights, in the order drawn.
barHeights :: Picture -> [Double]
barHeights = map (read . takeWhile (/= '"')) . drop 1 . split "height=\"" . svgBody
  where
    split sep s = case breakOn sep s of
        (start, Nothing) -> [start]
        (start, Just rest) -> start : split sep rest
    breakOn sep = go ""
      where
        go acc [] = (reverse acc, Nothing)
        go acc r@(c : cs)
            | take (length sep) r == sep = (reverse acc, Just (drop (length sep) r))
            | otherwise = go (c : acc) cs
