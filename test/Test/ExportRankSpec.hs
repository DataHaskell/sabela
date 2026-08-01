{-# LANGUAGE OverloadedStrings #-}

{- | Module-card export ordering.

Every budget downstream takes the head of the export list, so an alphabetical
head is the whole failure: live_test70 asked @Chart.Primitive@ for \"point\"
three times and got @blank, blankChart1, box, boxes@ each time, while
@GlyphChart :: Style -> [Point Double] -> Chart@ — the scatterplot constructor
it needed — sat unreachable further down.
-}
module Test.ExportRankSpec (spec) where

import qualified Data.Set as Set
import Data.Text (Text)
import Test.Hspec

import Sabela.AI.Search.Export (
    ExportRow (..),
    exportBand,
    isReExport,
    producesType,
    rankExportRows,
    resultOf,
 )
import Sabela.AI.Search.Need (parseNeed)

-- A slice of chart-svg's real Chart.Primitive, alphabetical as @:browse@ gives it.
chartPrimitive :: [ExportRow]
chartPrimitive =
    [ ExportRow "blank" "Rect Double -> ChartTree" ""
    , ExportRow "blankChart1" "Rect Double -> Chart" ""
    , ExportRow "box" "ChartData -> Maybe (Rect Double)" ""
    , ExportRow "boxes" "[Chart] -> Maybe (Rect Double)" ""
    , ExportRow "colourStyle" "(Colour -> Colour) -> Style -> Style" ""
    , ExportRow "filterChartTree" "(Chart -> Bool) -> ChartTree -> ChartTree" ""
    , ExportRow "flipPlace" "Place -> Place" ""
    , ExportRow "frameChart" "Style -> Double -> ChartTree -> ChartTree" ""
    , ExportRow "GlyphChart" "Style -> [Point Double] -> Chart" ""
    , ExportRow "GlyphData" "[Point Double] -> ChartData" ""
    , ExportRow "glyphData'" "Lens' ChartData (Maybe [Point Double])" ""
    , ExportRow "glyphize" "Style -> ChartTree -> ChartTree" ""
    ]

-- The Chart facade re-exports NumHask, which @:browse@ qualifies by origin.
chartFacade :: [ExportRow]
chartFacade =
    [ ExportRow "(NumHask.Algebra.Additive.+)" "a -> a -> a" ""
    , ExportRow "NumHask.Algebra.Additive.zero" "a" ""
    , ExportRow "NumHask.Algebra.Metric.magnitude" "a -> Mag a" ""
    , ExportRow "Chart" "Style -> ChartData -> Chart" ""
    , ExportRow "glyphChart" "Style -> [Point Double] -> Chart" ""
    ]

names :: Text -> [ExportRow] -> [Text]
names q = map erName . rankExportRows (parseNeed Set.empty q) "Chart.Primitive"

spec :: Spec
spec = do
    relevanceSpec
    producerSpec
    provenanceSpec
    bandSpec

{- | A truncated card is a head, and for a type-shaped query the head the caller
needs is what /makes/ the type. Live round 075227 asked for a type and got the
defining module's card with the consumers kept and the producer cut.
-}
producerSpec :: Spec
producerSpec = describe "producers of a queried type" $ do
    -- Equal-length names, so the tie the credit breaks is the credit's alone.
    let rows t =
            [ ExportRow "apply" "Int -> Frame -> Frame" ""
            , ExportRow "drain" (t <> " -> Frame") ""
            , ExportRow "build" ("[Text] -> Frame -> " <> t) ""
            , ExportRow "cover" ("(Frame -> " <> t <> ") -> Frame") ""
            ]
        ranked t = map erName (rankExportRows (parseNeed Set.empty t) "Mod" (rows t))

    it "leads with the producer, for any type name" $
        map (head . ranked) ["Grouped", "Widgetry", "Tally"]
            `shouldBe` ["build", "build", "build"]

    it "keeps the consumer ahead of rows the query does not touch" $
        takeWhile (/= "apply") (ranked "Grouped") `shouldContain` ["drain"]

    it "an arrow under a bracket is an argument, not the result" $ do
        producesType "Grouped" (ExportRow "cover" "(Frame -> Grouped) -> Frame" "")
            `shouldBe` False
        producesType "Grouped" (ExportRow "build" "Frame -> Grouped" "")
            `shouldBe` True

    it "reads the result through a context and a qualified name" $ do
        resultOf "Ord a => [a] -> Set.Set a" `shouldBe` " Set.Set a"
        producesType "Grouped" (ExportRow "g" "pkg-1.0:M.Inner.Grouped" "")
            `shouldBe` True

    -- The credit is a tie-layer for type-shaped needs, never a sort key of its
    -- own: a lower-case query is a name, and ties keep input order.
    it "a lower-case query earns no producer credit" $
        head
            ( map
                erName
                (rankExportRows (parseNeed Set.empty "grouped") "Mod" (rows "Grouped"))
            )
            `shouldBe` "drain"

relevanceSpec :: Spec
relevanceSpec = describe "export relevance" $ do
    -- The live_test70 regression. "point" appears in no export's NAME, only in
    -- the types; the old ranker required two or more query tokens before it
    -- would look at a type at all, so a one-word query could never match one.
    it "a one-word query matches on the type, not just the name" $ do
        let top = take 3 (names "point" chartPrimitive)
        top `shouldContain` ["GlyphChart"]
        top `shouldContain` ["GlyphData"]

    it "does not return the alphabetical head when something matches" $
        head (names "point" chartPrimitive) `shouldNotBe` "blank"

    it "a name match still outranks a type match" $
        head (names "glyphize" chartPrimitive) `shouldBe` "glyphize"

    it "prefers the plain name over its lens variant" $ do
        let ranked = names "glyph" chartPrimitive
        takeWhile (/= "glyphData'") ranked `shouldContain` ["glyphize"]

    it "keeps input order when nothing matches" $
        take 2 (names "wobble" chartPrimitive) `shouldBe` ["blank", "blankChart1"]

provenanceSpec :: Spec
provenanceSpec = describe "provenance" $ do
    it "reads the defining module from a qualified browse name" $ do
        isReExport "Chart" "(NumHask.Algebra.Additive.+)" `shouldBe` True
        isReExport "Chart" "Chart.Primitive.glyphChart" `shouldBe` False
        isReExport "Chart" "glyphChart" `shouldBe` False

    -- Chart re-exports NumHask, so an 840-export card led with +, zero and
    -- magnitude. The module's own API belongs in front of what it passes through.
    it "the module's own API outranks what it re-exports" $ do
        let ranked =
                map
                    erName
                    (rankExportRows (parseNeed Set.empty "chart") "Chart" chartFacade)
        take 2 ranked `shouldContain` ["Chart"]
        last ranked `shouldSatisfy` (`elem` map erName (take 3 chartFacade))

bandSpec :: Spec
bandSpec = describe "export bands" $
    it "orders values, then type-level names, then operators and internals" $ do
        exportBand "glyphize" `shouldBe` 0
        exportBand "GlyphChart" `shouldBe` 1
        exportBand "(NumHask.Algebra.Additive.+)" `shouldBe` 2
        exportBand "_internal" `shouldBe` 3
