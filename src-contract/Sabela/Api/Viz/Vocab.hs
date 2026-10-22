{-# LANGUAGE OverloadedStrings #-}

{- | The words the chart spec is made of, and the one order channels print
in; shared by the decoder, the tool schema, the code printer and the reader.
-}
module Sabela.Api.Viz.Vocab (
    channels,
    marks,
    aggregates,
    fieldTypes,
    scales,
    sorts,
    stacks,
    selections,
    orderChannels,
) where

import Data.Text (Text)

channels
    , marks
    , aggregates
    , fieldTypes
    , scales
    , sorts
    , stacks
    , selections ::
        [Text]
channels =
    [ "x"
    , "y"
    , "color"
    , "size"
    , "shape"
    , "opacity"
    , "tooltip"
    , "row"
    , "column"
    , "theta"
    ]
marks = ["bar", "line", "point", "area", "tick", "arc", "boxplot", "rule"]
aggregates = ["sum", "mean", "median", "min", "max", "count", "distinct"]
fieldTypes = ["quantitative", "nominal", "ordinal", "temporal"]
scales = ["linear", "log", "sqrt"]
sorts = ["ascending", "descending"]
stacks = ["stacked", "normalized", "unstacked"]
selections = ["interval", "point"]

-- | Channels in the one order a cell prints them, whatever order they came in.
orderChannels :: [(Text, a)] -> [(Text, a)]
orderChannels chans =
    [c | name <- order, c@(n, _) <- chans, n == name]
        ++ [c | c@(n, _) <- chans, n `notElem` order]
  where
    order =
        [ "x"
        , "y"
        , "color"
        , "size"
        , "shape"
        , "opacity"
        , "theta"
        , "row"
        , "column"
        , "tooltip"
        ]
