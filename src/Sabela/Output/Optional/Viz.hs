{-# LANGUAGE OverloadedStrings #-}

{- | Charts in a session that has the dataframe package: the library's own
web chart (typed or untyped) displays itself through the notebook's chart
runtime, carrying its Vega-Lite, and a frame can be asked for its columns.
Offered after the dataframe block; a dataframe too old to have the chart
builder loses only this.
-}
module Sabela.Output.Optional.Viz (
    vizChartDefs,
) where

import Data.Text (Text)
import qualified Data.Text as T

vizChartDefs :: Text
vizChartDefs =
    T.unlines
        [ ":{"
        , "import qualified Data.Aeson as SabelaAeson"
        , "import qualified Data.ByteString.Lazy.Char8 as SabelaBLC"
        , "import qualified DataFrame.Display.Web.Chart as SabelaChart"
        , "import qualified DataFrame.Display.Web.Chart.Typed as SabelaChartT"
        , ":}"
        , ":{"
        , "_sabelaJsonDoc :: SabelaAeson.Value -> SabelaBase.String"
        , "_sabelaJsonDoc v ="
        , "    SabelaBase.concatMap"
        , "        (\\c -> if c SabelaBase.== '<' then \"\\\\u003c\" else [c])"
        , "        (SabelaBLC.unpack (SabelaAeson.encode v))"
        , "_sabelaChartMarkup :: SabelaBase.String -> SabelaAeson.Value -> SabelaBase.String"
        , "_sabelaChartMarkup cid vl ="
        , "    \"<div class=\\\"sabela-viz\\\" data-cell-id=\\\"\" SabelaBase.++ cid"
        , "        SabelaBase.++ \"\\\"><script type=\\\"application/json\\\" class=\\\"sabela-viz-spec\\\">{\\\"v\\\":2,\\\"vegaLite\\\":\""
        , "        SabelaBase.++ _sabelaJsonDoc vl"
        , "        SabelaBase.++ \"}</script><div class=\\\"sabela-viz-view\\\"></div></div>\""
        , "_sabelaShowChart :: SabelaAeson.Value -> SabelaBase.IO ()"
        , "_sabelaShowChart vl ="
        , "    SabelaIORef.readIORef _sabelaCellIdRef SabelaBase.>>= \\cid ->"
        , "        display (html (_sabelaVizHtml (_sabelaChartMarkup cid vl)))"
        , "instance Display SabelaChart.Chart where"
        , "    display c = _sabelaShowChart (SabelaChart.toVegaSpec c)"
        , "instance Display (SabelaChartT.Chart cols) where"
        , "    display c = _sabelaShowChart (SabelaChartT.toVegaSpec c)"
        , "class SabelaFrameLike f where"
        , "    _sabelaFrameOf :: f -> SabelaDF.DataFrame"
        , "instance SabelaFrameLike SabelaDF.DataFrame where"
        , "    _sabelaFrameOf = SabelaBase.id"
        , "instance SabelaFrameLike (SabelaDFTyped.TypedDataFrame cols) where"
        , "    _sabelaFrameOf = SabelaDFTyped.thaw"
        , "_sabelaColumnsJson :: (SabelaFrameLike f) => f -> SabelaBase.String"
        , "_sabelaColumnsJson f ="
        , "    let df = _sabelaFrameOf f"
        , "     in _sabelaJsonDoc"
        , "            ( SabelaAeson.toJSON"
        , "                [ [SabelaDFText.unpack n, t]"
        , "                | (n, t) <- SabelaBase.zip (SabelaDF.columnNames df) (_sabelaDFTypes df)"
        , "                ]"
        , "            )"
        , ":}"
        ]
