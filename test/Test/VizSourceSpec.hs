{-# LANGUAGE OverloadedStrings #-}

{- | The canonical cell a chart spec prints as, and the ownership rule that
tells the builder's cells from hand-written ones.
-}
module Test.VizSourceSpec (spec) where

import Data.Aeson (decode, eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.Api.Viz
import Sabela.Api.Viz.FromVegaLite (fromVegaLite)
import Sabela.Api.Viz.Source
import Sabela.Model (
    Cell (..),
    CellType (..),
    Notebook (..),
    OutputItem (..),
    textToMime,
 )
import Sabela.Server.Viz (vizSourceFor)
import qualified Sabela.SessionTypes as ST

{- | The Vega-Lite the dataframe library emits for 'barSpec', as a chart
output carries it.
-}
libraryVegaLite :: Text
libraryVegaLite =
    "{\"$schema\":\"https://vega.github.io/schema/vega-lite/v5.json\",\"data\":{\"values\":[{\"city\":\"a\",\"product\":\"tea\",\"revenue\":1}]},"
        <> "\"encoding\":{\"color\":{\"field\":\"product\",\"type\":\"nominal\"},\"x\":{\"field\":\"city\",\"type\":\"nominal\"},"
        <> "\"y\":{\"aggregate\":\"sum\",\"field\":\"revenue\",\"type\":\"quantitative\"}},\"height\":400,"
        <> "\"mark\":{\"tooltip\":true,\"type\":\"bar\"},\"title\":\"Revenue by city\",\"width\":600}"

chartOutput :: Text
chartOutput =
    "<div class=\"sabela-viz\" data-cell-id=\"4\"><script type=\"application/json\" class=\"sabela-viz-spec\">"
        <> "{\"v\":2,\"vegaLite\":"
        <> libraryVegaLite
        <> "}</script><div class=\"sabela-viz-view\"></div></div>"

enc :: Text -> Enc
enc f = Enc f Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

barSpec :: VizSpec
barSpec =
    VizSpec
        { vsBase =
            VizLayer
                (Just "bar")
                [ ("x", [enc "city"])
                , ("y", [(enc "revenue"){encAggregate = Just "sum"}])
                , ("color", [enc "product"])
                ]
        , vsLayers = []
        , vsTitle = Just "Revenue by city"
        , vsWidth = Nothing
        , vsHeight = Nothing
        , vsSelect = Nothing
        }

barSource :: Text
barSource =
    T.unlines
        [ "import qualified DataFrame.Typed as DT"
        , "import qualified DataFrame.Display.Web.Chart.Typed as C"
        , "import DataFrame ((|>))"
        , ""
        , "C.chart orders"
        , "  |> C.mark C.Bar"
        , "  |> C.enc C.X (DT.col @\"city\")"
        , "  |> C.enc C.Y (DT.col @\"revenue\")"
        , "  |> C.aggregateOn C.Y C.Sum"
        , "  |> C.enc C.Color (DT.col @\"product\")"
        , "  |> C.title \"Revenue by city\""
        ]

cellWith :: Int -> Text -> [OutputItem] -> Cell
cellWith cid src outs = Cell cid CodeCell ST.Haskell src outs Nothing False

notebookOf :: [Cell] -> Notebook
notebookOf = Notebook "t.md"

spec :: Spec
spec = do
    describe "specToSource" $ do
        it "prints the canonical cell in a fixed order" $
            specToSource "orders" barSpec `shouldBe` barSource

        it "spells bins, scales, layers and a forced type as builder steps" $ do
            let s =
                    barSpec
                        { vsBase =
                            VizLayer
                                (Just "point")
                                [ ("x", [(enc "units"){encScale = Just "log", encType = Just "quantitative"}])
                                , ("tooltip", [enc "city"])
                                ]
                        , vsLayers =
                            [ VizLayer
                                (Just "bar")
                                [ ("x", [(enc "revenue"){encBin = Just 10}])
                                , ("y", [(enc "*"){encAggregate = Just "count"}])
                                ]
                            ]
                        , vsTitle = Nothing
                        , vsHeight = Just 250
                        }
                src = specToSource "df" s
            T.isInfixOf
                "C.encAs C.X (DT.col @\"units\") C.Quantitative |> C.logScale C.X"
                src
                `shouldBe` True
            T.isInfixOf "C.enc C.Tooltip (DT.col @\"city\")" src `shouldBe` True
            T.isInfixOf
                "C.chart df |> C.mark C.Bar |> C.enc C.X (DT.col @\"revenue\") |> C.binX |> C.aggregateOn C.Y C.Count"
                src
                `shouldBe` True
            T.isInfixOf "  |> C.size 600 250" src `shouldBe` True
            T.isPrefixOf "C.layer\n  [ C.chart df" (T.unlines (drop 4 (T.lines src)))
                `shouldBe` True

        it "reads its own data expression back" $
            dataExprOf barSource `shouldBe` Just "orders"

    describe "wire" $ do
        it "round-trips through json" $
            decode (encode barSpec) `shouldBe` Just barSpec

        it "reads the Vega-Lite the dataframe library writes" $
            specFromOutput chartOutput `shouldBe` Just barSpec

        it "reads a count, a bin, a layer and a size back" $ do
            let vl =
                    "{\"data\":{\"values\":[]},\"height\":250,\"width\":500,\"layer\":[{\"encoding\":{\"x\":{\"bin\":true,\"field\":\"revenue\",\"type\":\"quantitative\"},"
                        <> "\"y\":{\"aggregate\":\"count\",\"type\":\"quantitative\"}},\"mark\":{\"tooltip\":true,\"type\":\"bar\"}},"
                        <> "{\"encoding\":{\"x\":{\"field\":\"city\",\"type\":\"nominal\"}},\"mark\":{\"tooltip\":true,\"type\":\"line\"}}]}"
                Just v = decode (LBS.pack (T.unpack vl))
                Just s = fromVegaLite v
            vlMark (vsBase s) `shouldBe` Just "bar"
            lookup "x" (vlChannels (vsBase s))
                `shouldBe` Just [(enc "revenue"){encBin = Just 10}]
            lookup "y" (vlChannels (vsBase s))
                `shouldBe` Just [(enc "*"){encAggregate = Just "count"}]
            map vlMark (vsLayers s) `shouldBe` [Just "line"]
            (vsWidth s, vsHeight s) `shouldBe` (Just 500, Just 250)

        it "reads an escaped field reference back as the column name" $ do
            let vl =
                    "{\"data\":{\"values\":[]},\"encoding\":{\"x\":{\"field\":\"variety\",\"type\":\"nominal\"},"
                        <> "\"y\":{\"aggregate\":\"sum\",\"field\":\"sepal\\\\.length\",\"type\":\"quantitative\"}},\"mark\":{\"type\":\"bar\"}}"
                Just v = decode (LBS.pack (T.unpack vl))
                Just s = fromVegaLite v
            lookup "y" (vlChannels (vsBase s))
                `shouldBe` Just [(enc "sepal.length"){encAggregate = Just "sum"}]

        it "names the field and the allowed words when a value does not fit" $ do
            let Left err =
                    eitherDecode "{\"mark\":\"histogram\",\"x\":{\"field\":\"a\"}}" ::
                        Either String VizSpec
            err `shouldContain` "mark: expected one of bar, line"
            err `shouldContain` "histogram"
            let Left err2 =
                    eitherDecode "{\"mark\":\"bar\",\"x\":{\"field\":\"a\",\"agg\":\"sum\"}}" ::
                        Either String VizSpec
            err2 `shouldContain` "unknown key(s) agg"

    describe "ownership" $ do
        let output = chartOutput
            outs = [OutputItem (textToMime "text/html") output]
            req = VizSourceRequest barSpec Nothing (Just 4)

        it "recovers the spec a chart output carries" $
            specFromOutput output `shouldBe` Just barSpec

        it "calls an untouched canonical cell canonical" $
            vsOwnership (vizSourceFor (notebookOf [cellWith 4 barSource outs]) req)
                `shouldBe` OwnCanonical

        it "ignores whitespace at the ends of lines" $
            vsOwnership
                ( vizSourceFor
                    (notebookOf [cellWith 4 (T.replace "\n" "  \n" barSource) outs])
                    req
                )
                `shouldBe` OwnCanonical

        it "calls a hand-edited cell edited" $ do
            let r =
                    vizSourceFor (notebookOf [cellWith 4 (barSource <> "\n-- tweaked\n") outs]) req
            vsOwnership r `shouldBe` OwnEdited
            vsMessage r `shouldContain'` "edited by hand"
            vsSource r `shouldBe` barSource

        it "treats a missing cell as new and keeps the data expression given" $ do
            let r =
                    vizSourceFor (notebookOf []) (VizSourceRequest barSpec (Just "sales") Nothing)
            vsOwnership r `shouldBe` OwnNew
            vsDataExpr r `shouldBe` "sales"
            T.isInfixOf "C.chart sales" (vsSource r) `shouldBe` True
  where
    shouldContain' t needle = T.isInfixOf needle t `shouldBe` True
