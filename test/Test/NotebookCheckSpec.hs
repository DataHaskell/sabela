{-# LANGUAGE OverloadedStrings #-}

module Test.NotebookCheckSpec (spec) where

import Test.Hspec

import Sabela.Notebook.Check (
    chartAgrees,
    svgMarkCount,
    tableAgrees,
    tableShape,
 )

chartSvg :: String
chartSvg =
    "<svg xmlns=\"http://www.w3.org/2000/svg\">\
    \<path d=\"M0,0 L1,1\"/><circle cx=\"1\" cy=\"1\"/><circle cx=\"2\" cy=\"4\"/>\
    \</svg>"

emptyFrame :: String
emptyFrame = "<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>"

pts :: [(Double, Double)]
pts = [(0, 0), (1, 1), (2, 4)]

spec :: Spec
spec = describe "structural artifact checks (C2 task 5)" $ do
    describe "chartAgrees" $ do
        it "passes when the census covers every datum" $
            chartAgrees chartSvg pts `shouldBe` True

        it "fails a render that draws nothing, however well it parses" $
            chartAgrees emptyFrame pts `shouldBe` False

        it "fails when the data outnumber the marks" $
            chartAgrees chartSvg (pts ++ [(3, 9), (4, 16)]) `shouldBe` False

        it "cannot be weakened by passing no data" $
            chartAgrees chartSvg ([] :: [(Double, Double)]) `shouldBe` False

        it "fails anything that is not an SVG at all" $
            chartAgrees "just some text" pts `shouldBe` False

        it "counts marks, never the container" $ do
            svgMarkCount chartSvg `shouldBe` 3
            svgMarkCount emptyFrame `shouldBe` 0

    describe "tableAgrees" $ do
        let rendered =
                unlines
                    [ "name  count  mean"
                    , "----  -----  ----"
                    , "a     1      2.0"
                    , "b     3      4.0"
                    ]
        it "passes when both counts agree with the caller's recount" $
            tableAgrees rendered 3 2 `shouldBe` True

        it "fails when the column count disagrees" $
            tableAgrees rendered 4 2 `shouldBe` False

        it "fails when the row count disagrees" $
            tableAgrees rendered 3 3 `shouldBe` False

        it "cannot be weakened to a zero-shape assertion" $ do
            tableAgrees rendered 0 0 `shouldBe` False
            tableAgrees "" 0 0 `shouldBe` False

        it "reads the shape past the separator rule" $
            tableShape rendered `shouldBe` (3, 2)
