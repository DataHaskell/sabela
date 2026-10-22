{-# LANGUAGE OverloadedStrings #-}

{- | The chart tool's definition. Its schema is the chart language itself,
flat at the top so a small model's call is checked key by key before any
code is written, with the nested encodings validated by the same decoder
the builder uses.
-}
module Sabela.AI.Capabilities.Tools.Viz (vizTools, makeChartSchema) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as Key
import Data.Text (Text)

import Sabela.AI.Capabilities.ToolName (ToolName (..), mkTool)
import Sabela.Anthropic.Types (ToolDef)
import Sabela.Api.Viz (aggregates, fieldTypes, marks, scales, sorts, stacks)

vizTools :: [ToolDef]
vizTools =
    [ mkTool
        MakeChart
        "Make a chart from data already in the notebook, as a real Haskell cell you and the user can both edit. Pick a mark and map columns to x, y, color and the rest; aggregate with sum/mean/count, bin for histograms. Prefer this over writing chart code by hand; the cell it writes is plain dataframe code (DataFrame.Display.Web.Chart.Typed over a TypedDataFrame)."
        makeChartSchema
    ]

str :: Text -> [(Text, Value)] -> Value
str desc extra =
    object
        ( ["type" .= ("string" :: Text), "description" .= desc]
            ++ [Key.fromText k .= v | (k, v) <- extra]
        )

enumOf :: Text -> [Text] -> Value
enumOf desc vals = object ["type" .= ("string" :: Text), "enum" .= vals, "description" .= desc]

encoding :: Text -> Value
encoding desc =
    object
        [ "type" .= ("object" :: Text)
        , "description" .= desc
        , "properties"
            .= object
                [ "field" .= str "Column name, or \"*\" with aggregate count to count rows." []
                , "aggregate" .= enumOf "Fold the column per group." aggregates
                , "bin"
                    .= object
                        [ "type" .= ("integer" :: Text)
                        , "description" .= ("Bin a number into at most this many buckets." :: Text)
                        ]
                , "type" .= enumOf "Override the inferred field type." fieldTypes
                , "sort" .= enumOf "Order of the field's values." sorts
                , "scale" .= enumOf "Axis scale." scales
                , "title" .= str "Axis or legend title." []
                , "stack" .= enumOf "How bars or areas stack." stacks
                , "zero"
                    .= object
                        [ "type" .= ("boolean" :: Text)
                        , "description"
                            .= ( "Anchor the axis at zero (default for bars; false fits scatter axes to the data)." ::
                                    Text
                               )
                        ]
                ]
        , "required" .= (["field"] :: [Text])
        ]

layerProps :: [(Text, Value)]
layerProps =
    [ ("mark", enumOf "What to draw." marks)
    , ("x", encoding "Horizontal position.")
    , ("y", encoding "Vertical position.")
    , ("color", encoding "Colour, by a category or a number.")
    , ("size", encoding "Mark size, by a number.")
    , ("shape", encoding "Point shape, by a category.")
    , ("opacity", encoding "Mark opacity, by a number.")
    ,
        ( "theta"
        , encoding
            "Slice size for an arc (pie) mark; put the category on color, not x, or every x value draws its own pie."
        )
    , ("row", encoding "Facet into rows by a category.")
    , ("column", encoding "Facet into columns by a category.")
    ,
        ( "tooltip"
        , object
            [ "type" .= ("array" :: Text)
            , "items" .= encoding "A field shown on hover."
            , "description" .= ("Fields shown on hover." :: Text)
            ]
        )
    ]

makeChartSchema :: Value
makeChartSchema =
    object
        [ "type" .= ("object" :: Text)
        , "properties"
            .= object
                ( [ "data"
                        .= str
                            "Haskell expression for the rows: a binding from list_bindings that is a DataFrame, a [record] deriving Tabular, [(String, Double)] pairs, or V.fromRows header rows."
                            []
                  , "layers"
                        .= object
                            [ "type" .= ("array" :: Text)
                            , "description"
                                .= ( "Further marks drawn over the same data; each inherits the base channels it does not set." ::
                                        Text
                                   )
                            , "items"
                                .= object
                                    [ "type" .= ("object" :: Text)
                                    , "properties" .= object [Key.fromText k .= v | (k, v) <- layerProps]
                                    ]
                            ]
                  , "title" .= str "Chart title." []
                  , "width" .= object ["type" .= ("integer" :: Text)]
                  , "height" .= object ["type" .= ("integer" :: Text)]
                  , "select"
                        .= enumOf
                            "Expose a brush (interval) or clicked points as a widget value named select_name."
                            ["interval", "point"]
                  , "select_name" .= str "Widget name other cells read with widgetGet." []
                  , "cell_id"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description"
                                .= ("Rewrite this chart cell instead of inserting a new one." :: Text)
                            ]
                  , "after_cell_id"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("Insert after this cell instead of at the end." :: Text)
                            ]
                  ]
                    ++ [Key.fromText k .= v | (k, v) <- layerProps]
                )
        , "required" .= (["data", "mark"] :: [Text])
        ]
