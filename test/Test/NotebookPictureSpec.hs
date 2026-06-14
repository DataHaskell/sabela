{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}

{- | Tests for the sticky @Sabela.Notebook.Picture@ DSL: monoid structure, the
@svgBody@ homomorphism, shape rendering, and inner-wins styling.
-}
module Test.NotebookPictureSpec (spec) where

import Data.List (isInfixOf)
import Sabela.Notebook.Picture
import Test.Hspec

spec :: Spec
spec = do
    describe "Picture is a monoid" $ do
        let a = circle (1, 2) 3
            b = rectangle (0, 0) 4 5
            c = line (0, 0) (1, 1)
        it "mempty is the empty drawing" $ svgBody (mempty :: Picture) `shouldBe` ""
        it "mempty is a left identity" $ svgBody (mempty <> a) `shouldBe` svgBody a
        it "mempty is a right identity" $ svgBody (a <> mempty) `shouldBe` svgBody a
        it "is associative" $
            svgBody ((a <> b) <> c) `shouldBe` svgBody (a <> (b <> c))

    describe "svgBody is a monoid homomorphism" $ do
        let a = fill red (circle (1, 1) 2)
            b = rectangle (3, 3) 4 4
        it "drawing of (a <> b) is drawing a then drawing b" $
            svgBody (a <> b) `shouldBe` (svgBody a ++ svgBody b)
        it "drawing of mempty is the empty string" $
            svgBody (mempty :: Picture) `shouldBe` ""

    describe "shapes render to SVG" $ do
        it "circle" $
            svgBody (circle (10, 20) 5) `shouldBe` "<circle cx=\"10\" cy=\"20\" r=\"5\"/>"
        it "rectangle" $
            svgBody (rectangle (0, 0) 30 40)
                `shouldBe` "<rect x=\"0\" y=\"0\" width=\"30\" height=\"40\"/>"
        it "whole numbers drop the trailing .0" $
            svgBody (circle (1.5, 2) 3) `shouldBe` "<circle cx=\"1.5\" cy=\"2\" r=\"3\"/>"

    describe "styling wraps in <g> so inner wins (SVG cascade)" $ do
        it "fill adds a fill attribute on a group" $
            svgBody (fill red (circle (0, 0) 1))
                `shouldBe` "<g fill=\"red\"><circle cx=\"0\" cy=\"0\" r=\"1\"/></g>"
        it "a fill inside a fill nests, inner closest to the shape" $ do
            let s = svgBody (fill blue (fill red (circle (0, 0) 1)))
            s `shouldSatisfy` isInfixOf "<g fill=\"blue\"><g fill=\"red\">"
        it "rgb mixes a colour" $
            svgBody (fill (rgb 255 140 0) (circle (0, 0) 1))
                `shouldSatisfy` isInfixOf "fill=\"rgb(255,140,0)\""

    describe "transforms wrap in <g transform>" $ do
        it "translate" $
            svgBody (translate (10, 20) (circle (0, 0) 1))
                `shouldSatisfy` isInfixOf "<g transform=\"translate(10,20)\">"
        it "rotate" $
            svgBody (rotate 45 (circle (0, 0) 1))
                `shouldSatisfy` isInfixOf "transform=\"rotate(45)\""

    describe "renderSvg wraps the body once" $ do
        let out = renderSvg (Canvas 100 200) (circle (1, 1) 1)
        it "opens an <svg> with the canvas size" $
            out `shouldSatisfy` isInfixOf "width=\"100\" height=\"200\""
        it "closes the <svg>" $ out `shouldSatisfy` isInfixOf "</svg>"
        it "contains the body" $
            out `shouldSatisfy` isInfixOf (svgBody (circle (1, 1) 1))

    describe "fromSvg" $
        it "drops a raw SVG fragment in verbatim" $
            svgBody (fromSvg "<custom/>") `shouldBe` "<custom/>"

    describe "lineChart" $ do
        it "is empty for fewer than two points" $
            svgBody (lineChart (Canvas 100 100) [(1, 1)]) `shouldBe` ""
        it "auto-scales points into the canvas (with a 30px margin)" $
            svgBody (lineChart (Canvas 100 100) [(0, 0), (10, 10)])
                `shouldSatisfy` isInfixOf "30,70 70,30"
