{-# LANGUAGE OverloadedStrings #-}

{- | The canonical cell for a chart spec, and the ownership rule built on it:
a cell is the builder's to rewrite exactly when its text is what the builder
would write for the spec its output carries. The cell is ordinary
@dataframe@ code (DataFrame.Typed plus the library's own web chart builder),
so it runs unchanged outside Sabela. Nothing here parses Haskell; the
template is fixed, so equality is the whole test.
-}
module Sabela.Api.Viz.Source (
    specToSource,
    dataExprOf,
    normaliseSource,
    ownership,
    specFromOutput,
    vizImports,
) where

import Data.Aeson (Value (..), decodeStrict')
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAsciiLower)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Sabela.Api.Viz
import Sabela.Api.Viz.FromVegaLite (fromVegaLite)

vizImports :: [Text]
vizImports =
    [ "import qualified DataFrame.Typed as DT"
    , "import qualified DataFrame.Display.Web.Chart.Typed as C"
    , "import DataFrame ((|>))"
    ]

{- | The cell text for a spec over a data expression, with steps in a fixed
order so the same spec always prints the same way:

> C.chart orders
>   |> C.mark C.Bar
>   |> C.enc C.X (DT.col @"city")
>   |> C.enc C.Y (DT.col @"revenue")
>   |> C.aggregateOn C.Y C.Sum
>   |> C.title "Revenue by city"
-}
specToSource :: Text -> VizSpec -> Text
specToSource dataExpr s = T.unlines (vizImports ++ [""] ++ body)
  where
    d = T.strip dataExpr
    body
        | null (vsLayers s) = chain 0 (layerSteps d (vsBase s) ++ chartSteps s)
        | otherwise =
            ["C.layer"]
                ++ zipWith
                    ( \i l ->
                        (if i == (0 :: Int) then "  [ " else "  , ")
                            <> T.intercalate " |> " (layerSteps d l)
                    )
                    [0 ..]
                    (vsBase s : vsLayers s)
                ++ ["  ]"]
                ++ map ("  |> " <>) (chartSteps s)
    chain _ (first : rest) = first : map ("  |> " <>) rest
    chain _ [] = []

-- | A layer: the data, its mark, then its channels in a fixed order.
layerSteps :: Text -> VizLayer -> [Text]
layerSteps d l =
    ("C.chart " <> d)
        : catMaybes [("C.mark C." <>) . capitalise <$> vlMark l]
        ++ concatMap channelSteps (orderChannels (vlChannels l))

{- | A channel's steps: the encoding, then its aggregate, bin and scale as the
builder spells them. Options the builder has no word for are left out.
-}
channelSteps :: (Text, [Enc]) -> [Text]
channelSteps (_, []) = []
channelSteps (ch, e : _) =
    catMaybes
        [ if encField e == "*" then Nothing else Just encStep
        , (\a -> "C.aggregateOn " <> chan <> " C." <> capitalise a) <$> encAggregate e
        , case encBin e of
            Just _ | ch == "x" -> Just "C.binX"
            Just _ | ch == "y" -> Just "C.binY"
            _ -> Nothing
        , case encScale e of
            Just "log" -> Just ("C.logScale " <> chan)
            _ -> Nothing
        , (\z -> "C.includeZero " <> chan <> (if z then " True" else " False"))
            <$> encZero e
        ]
  where
    chan = "C." <> channelCon ch
    column = "(DT.col @" <> lit (encField e) <> ")"
    encStep = case encType e of
        Just ty -> "C.encAs " <> chan <> " " <> column <> " C." <> capitalise ty
        Nothing -> "C.enc " <> chan <> " " <> column

channelCon :: Text -> Text
channelCon ch = case ch of
    "x" -> "X"
    "y" -> "Y"
    "color" -> "Color"
    "size" -> "Size"
    "shape" -> "Shape"
    "opacity" -> "Opacity"
    "theta" -> "Theta"
    "row" -> "Row"
    "column" -> "Column"
    "tooltip" -> "Tooltip"
    other -> capitalise other

-- | Title and size, after the layers.
chartSteps :: VizSpec -> [Text]
chartSteps s =
    catMaybes
        [ ("C.title " <>) . lit <$> vsTitle s
        , case (vsWidth s, vsHeight s) of
            (Nothing, Nothing) -> Nothing
            (w, h) ->
                Just ("C.size " <> tshow (fromMaybe 600 w) <> " " <> tshow (fromMaybe 400 h))
        ]

capitalise :: Text -> Text
capitalise t = case T.uncons t of
    Just (c, rest) -> T.cons (toUpperAscii c) rest
    Nothing -> t
  where
    toUpperAscii c = if isAsciiLower c then toEnum (fromEnum c - 32) else c

lit :: Text -> Text
lit = T.pack . show . T.unpack

tshow :: (Show a) => a -> Text
tshow = T.pack . show

-- | The data expression of a canonical cell: what follows the first @C.chart@.
dataExprOf :: Text -> Maybe Text
dataExprOf src =
    case mapMaybe fromLine (T.lines src) of
        (e : _) | not (T.null e) -> Just e
        _ -> Nothing
  where
    fromLine l = do
        let (_, after) = T.breakOn "C.chart " l
        rest <- T.stripPrefix "C.chart " after
        pure (T.strip (fst (T.breakOn " |>" rest)))

-- | Text with trailing spaces and surrounding blank lines removed.
normaliseSource :: Text -> Text
normaliseSource = T.strip . T.unlines . map T.stripEnd . T.lines . T.replace "\r\n" "\n"

{- | Canonical when the cell reads exactly as the spec prints; edited
otherwise. The spec compared is the one the cell's output carries, since a
hand edit that changed the chart changed that too.
-}
ownership :: VizSpec -> Text -> VizOwnership
ownership spec cellSource = case dataExprOf cellSource of
    Nothing -> OwnEdited
    Just d
        | normaliseSource cellSource == normaliseSource (specToSource d spec) ->
            OwnCanonical
        | otherwise -> OwnEdited

-- | The spec a chart output carries: the Vega-Lite in its JSON block, read back.
specFromOutput :: Text -> Maybe VizSpec
specFromOutput html = do
    let (_, after) = T.breakOn marker html
    rest <- T.stripPrefix marker after
    let json = fst (T.breakOn "</script>" rest)
    Object o <- decodeStrict' (TE.encodeUtf8 json)
    vl <- KM.lookup "vegaLite" o
    fromVegaLite vl
  where
    marker = "class=\"sabela-viz-spec\">"
