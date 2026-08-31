{- | Charts named after what they draw, each taking the data you already have.

Every chart comes in two forms: the plain name draws on 'defaultCanvas', and
the @…On@ form takes a 'Canvas' when you want a particular size. Show one with
'Sabela.Notebook.Picture.displayPicture'.

> displayPicture (barChart [("Q1", 12), ("Q2", 18), ("Q3", 9)])
-}
module Sabela.Notebook.Chart (
    barChart,
    barChartOn,
    lineChart,
    lineChartOn,
    scatterChart,
    scatterChartOn,
    histogram,
    histogramOn,
    binCounts,
) where

import Sabela.Notebook.Picture

-- | Space left around the plotting area for axes and labels.
margin :: Double
margin = 34

barColour :: Color
barColour = rgb 74 158 255

-- | Axis rules along the left edge and the given baseline.
axesAt :: Canvas -> Double -> Picture
axesAt (Canvas w h) baseline =
    line (margin, margin) (margin, h - margin)
        <> line (margin, baseline) (w - margin, baseline)

{- | A bar per labelled value, on the default canvas.

> barChart [("Q1", 12), ("Q2", 18), ("Q3", 9)]
-}
barChart :: [(String, Double)] -> Picture
barChart = barChartOn defaultCanvas

-- | 'barChart' at a size you choose.
barChartOn :: Canvas -> [(String, Double)] -> Picture
barChartOn _ [] = mempty
barChartOn canvas@(Canvas w h) bars =
    axesAt canvas baseline <> group (zipWith bar [0 ..] bars)
  where
    slotWidth = (w - 2 * margin) / fromIntegral (length bars)
    barWidth = slotWidth * 0.7
    (yOf, baseline) = verticalScale canvas (map snd bars)
    bar :: Int -> (String, Double) -> Picture
    bar i (name, v) =
        fill barColour (rectangle (x, top) barWidth height)
            <> text (x + barWidth / 2, h - margin + 14) name
      where
        x = margin + fromIntegral i * slotWidth + (slotWidth - barWidth) / 2
        top = min baseline (yOf v)
        height = abs (yOf v - baseline)

{- | Maps values to canvas rows, and where zero sits. A flat column still gets a
span so nothing divides by zero.
-}
verticalScale :: Canvas -> [Double] -> (Double -> Double, Double)
verticalScale (Canvas _ h) vs = (yOf, yOf 0)
  where
    lo = minimum (0 : vs)
    hi = maximum (0 : vs)
    range = if hi == lo then 1 else hi - lo
    yOf v = (h - margin) - (v - lo) / range * (h - 2 * margin)

{- | Points joined in order, on the default canvas.

> lineChart [(0, 1), (1, 4), (2, 2)]
-}
lineChart :: [(Double, Double)] -> Picture
lineChart = lineChartOn defaultCanvas

-- | 'lineChart' at a size you choose.
lineChartOn :: Canvas -> [(Double, Double)] -> Picture
lineChartOn canvas pts
    | length pts < 2 = mempty
    | otherwise =
        frameOf canvas pts <> strokeWidth 2 (polyline (map (toPixels canvas pts) pts))

{- | One dot per point, on the default canvas.

> scatterChart [(0, 1), (1, 4), (2, 2)]
-}
scatterChart :: [(Double, Double)] -> Picture
scatterChart = scatterChartOn defaultCanvas

-- | 'scatterChart' at a size you choose.
scatterChartOn :: Canvas -> [(Double, Double)] -> Picture
scatterChartOn _ [] = mempty
scatterChartOn canvas pts =
    frameOf canvas pts
        <> group [fill barColour (circle (toPixels canvas pts p) 3) | p <- pts]

-- | Left and bottom rules around a set of points.
frameOf :: Canvas -> [(Double, Double)] -> Picture
frameOf canvas@(Canvas _ h) _ = axesAt canvas (h - margin)

-- | Places a data point on the canvas, scaled to the extent of the whole set.
toPixels :: Canvas -> [(Double, Double)] -> (Double, Double) -> (Double, Double)
toPixels (Canvas w h) pts (x, y) =
    ( margin + (x - minimum xs) / spanOf xs * (w - 2 * margin)
    , (h - margin) - (y - minimum ys) / spanOf ys * (h - 2 * margin)
    )
  where
    xs = map fst pts
    ys = map snd pts
    spanOf vs = let d = maximum vs - minimum vs in if d == 0 then 1 else d

{- | How many observations fall in each of @n@ equal-width bins. Every
observation lands in exactly one bin, and a sample with no spread lands
wholly in the first.

> binCounts 4 [1, 1, 2, 2, 2, 3, 8, 9]
-}
binCounts :: Int -> [Double] -> [Int]
binCounts n xs
    | n <= 0 || null xs = []
    | otherwise = [length (filter ((== i) . binOf) xs) | i <- [0 .. n - 1]]
  where
    lo = minimum xs
    hi = maximum xs
    width = if hi == lo then 1 else (hi - lo) / fromIntegral n
    binOf x = min (n - 1) (floor ((x - lo) / width))

{- | The distribution of a sample in @n@ bins, on the default canvas.

> histogram 10 measurements
-}
histogram :: Int -> [Double] -> Picture
histogram = histogramOn defaultCanvas

-- | 'histogram' at a size you choose.
histogramOn :: Canvas -> Int -> [Double] -> Picture
histogramOn canvas n xs
    | null counts = mempty
    | otherwise =
        barChartOn canvas [(binLabel i, fromIntegral c) | (i, c) <- zip [0 ..] counts]
  where
    counts = binCounts n xs
    lo = minimum xs
    hi = maximum xs
    width = if hi == lo then 1 else (hi - lo) / fromIntegral n
    binLabel :: Int -> String
    binLabel i = shortly (lo + fromIntegral i * width)

-- | A number short enough to sit under a bar.
shortly :: Double -> String
shortly v
    | v == fromIntegral r = show r
    | otherwise = show (fromIntegral (round (v * 10) :: Integer) / 10 :: Double)
  where
    r = round v :: Integer
