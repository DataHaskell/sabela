{-# LANGUAGE OverloadedStrings #-}

{- | The chart language as the model first meets it: one card, short enough
to sit in every prompt, that shows the shape of a chart cell and names the
whole vocabulary. The same words are the @make_chart@ tool's schema.
-}
module Sabela.AI.VizCard (vizGrammarCard) where

import Data.Text (Text)
import qualified Data.Text as T

vizGrammarCard :: Text
vizGrammarCard =
    T.unlines
        [ "## Charts: dataframe's own typed chart builder"
        , ""
        , "Prefer the make_chart tool: it writes the canonical cell below, which"
        , "the user can also edit in the Design panel. A chart cell is ordinary"
        , "dataframe code over a typed frame, so it runs unchanged outside Sabela;"
        , "every column, aggregate and scale is checked by the compiler."
        , ""
        , "```haskell"
        , "import qualified DataFrame.Typed as DT"
        , "import qualified DataFrame.Typed.IO.CSV as TCSV"
        , "import qualified DataFrame.Display.Web.Chart.Typed as C"
        , "import DataFrame ((|>))"
        , ""
        , "$(DT.deriveSchemaFromCsvFile \"Orders\" \"./data/orders.csv\")   -- type Orders = '[ '(\"city\", Text), … ]"
        , "orders <- TCSV.readCsv @Orders \"./data/orders.csv\""
        , ""
        , "C.chart orders"
        , "  |> C.mark C.Bar"
        , "  |> C.enc C.X (DT.col @\"city\")"
        , "  |> C.enc C.Y (DT.col @\"revenue\")"
        , "  |> C.aggregateOn C.Y C.Sum"
        , "  |> C.enc C.Color (DT.col @\"product\")"
        , "  |> C.title \"Revenue by city\""
        , "```"
        , ""
        , "- data: a TypedDataFrame. From a file: `$(DT.deriveSchemaFromCsvFile \"Name\" path)`"
        , "  (or ...FromParquetFile) then `name <- TCSV.readCsv @Name path`. From an"
        , "  untyped frame df already loaded: `$(DT.deriveSchema \"Df\" df)` then"
        , "  `tdf <- freezeOrThrow @Df df` (import DataFrame.Typed.Freeze). The notebook needs"
        , "  `-- cabal: build-depends: dataframe, text` and"
        , "  `-- cabal: default-extensions: DataKinds, TemplateHaskell, TypeApplications, OverloadedStrings`."
        , "  Columns: `DT.columnNames tdf`, `D.columnNames df`, or peek_data for a file."
        , "- marks: C.Bar C.Line C.Point C.Area C.Tick C.Arc C.Boxplot C.Rule."
        , "- a pie is C.Arc with the category on C.Color and the value on C.Theta (summed);"
        , "  never C.X or C.Y on an arc: each x value would draw its own pie."
        , "- channels: C.X C.Y C.Color C.Size C.Shape C.Opacity C.Theta C.Row C.Column C.Tooltip,"
        , "  each `C.enc <channel> (DT.col @\"name\")`, or `C.encAs ... C.Temporal` to force a type."
        , "- per channel: `C.aggregateOn C.Y C.Sum` (Count Sum Mean Median Min Max;"
        , "  Count needs no column), `C.binX` / `C.binY` for histograms, `C.logScale C.Y`,"
        , "  `C.includeZero C.Y False`."
        , "- options: `C.title`, `C.size w h`, `C.layer [chart1, chart2]` to overlay marks"
        , "  that share data, `C.regression x y`, `C.density x`."
        , "The chart is drawn by Vega-Lite in the notebook; `C.toHtml` or `C.showChart`"
        , "give the same chart outside it. Use granite only for what this cannot say."
        ]
