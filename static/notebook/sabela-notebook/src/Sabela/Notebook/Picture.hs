{- |
Module      : Sabela.Notebook.Picture
Description : Build pictures by snapping simple shapes together.

A __picture__ is a drawing made of simple shapes — circles, rectangles, lines,
text — that you combine, colour, and move around. Building a picture never draws
anything; it just /describes/ a drawing. You see it with 'picture'.

= The two ideas

1. __Make shapes__ with 'circle', 'rectangle', 'line', 'polygon', 'text'.
2. __Combine and dress them up__: put one on top of another with @'<>'@ (or
   'group'), recolour with 'fill'\/'stroke', and move with 'translate'\/'scale'\/
   'rotate'.

= A first picture

>>> picture (fill red (circle (150, 150) 60))      -- a red circle, shown in the cell

Stack shapes with @'<>'@ (later ones draw on top):

>>> picture (fill blue (rectangle (50,50) 200 100) <> fill white (circle (150,100) 30))

Move a shape without changing how it was built:

>>> picture (translate (100, 0) (fill green (circle (60, 60) 40)))

= Colours

Use a named colour ('red', 'green', 'blue', 'black', 'white', …) or mix your own
with 'rgb'.

>>> picture (fill (rgb 255 140 0) (circle (150,150) 70))   -- orange

= Coordinates

The canvas is like a sheet of graph paper. @(0, 0)@ is the __top-left__ corner;
@x@ grows to the right and @y@ grows __downward__ (the usual computer-screen
convention). The default canvas is 300×300; use 'pictureOn' for a different size.
-}
module Sabela.Notebook.Picture (
    -- * The picture type
    Picture,

    -- * Shapes
    circle,
    rectangle,
    line,
    polyline,
    polygon,
    path,
    text,

    -- * Colour
    Color,
    rgb,
    red,
    green,
    blue,
    black,
    white,
    orange,
    purple,
    yellow,

    -- * Dressing shapes up
    fill,
    stroke,
    strokeWidth,
    opacity,

    -- * Moving shapes
    translate,
    scale,
    rotate,

    -- * Combining
    group,

    -- * Charts (handy for data streams)
    lineChart,

    -- * Showing a picture
    Canvas (..),
    defaultCanvas,
    picture,
    pictureOn,
    renderSvg,
    svgBody,

    -- * Escape hatch
    fromSvg,
) where

import Data.List (intercalate)
import Sabela.Notebook.Picture.Internal (
    Canvas (..),
    Picture (..),
    Point,
    Shape (..),
    num,
 )
import Sabela.Notebook.Picture.Svg (renderSvg, svgBody)

-- | A circle, given its centre and radius.
circle :: Point -> Double -> Picture
circle c r = Prim (Circle c r)

-- | A rectangle, given its top-left corner, width, and height.
rectangle :: Point -> Double -> Double -> Picture
rectangle tl w h = Prim (Rectangle tl w h)

-- | A straight line between two points.
line :: Point -> Point -> Picture
line a b = Prim (Line a b)

-- | A connected run of line segments through the given points (not filled).
polyline :: [Point] -> Picture
polyline = Prim . Polyline

-- | A closed shape through the given corners.
polygon :: [Point] -> Picture
polygon = Prim . Polygon

{- | An arbitrary shape from a raw SVG path string (the @d=@ attribute), for when
you need something the named shapes don't cover.
-}
path :: String -> Picture
path = Prim . PathD

-- | Text drawn starting at a point.
text :: Point -> String -> Picture
text p s = Prim (Label p s)

{- | A raw SVG fragment, dropped in verbatim. Useful for embedding a chart
produced elsewhere (e.g. a Granite plot) into a picture.
-}
fromSvg :: String -> Picture
fromSvg = Raw

-- | A colour. Make one with 'rgb' or use a named colour like 'red'.
newtype Color = Color {colorText :: String}

{- | Mix a colour from red, green, and blue parts, each @0–255@.

>>> rgb 255 140 0   -- orange
-}
rgb :: Int -> Int -> Int -> Color
rgb r g b = Color ("rgb(" ++ intercalate "," (map show [r, g, b]) ++ ")")

red, green, blue, black, white, orange, purple, yellow :: Color
red = Color "red"
green = Color "green"
blue = Color "blue"
black = Color "black"
white = Color "white"
orange = Color "orange"
purple = Color "purple"
yellow = Color "gold"

{- | Fill a picture with a colour. If you fill an already-filled picture, the
__inner__ fill wins — so you can set an overall colour and override parts of it.

>>> picture (fill blue (circle (80,150) 40 <> fill red (circle (220,150) 40)))
-- left circle blue, right circle red
-}
fill :: Color -> Picture -> Picture
fill c = Attr [("fill", colorText c)]

-- | Draw a picture's outline in a colour.
stroke :: Color -> Picture -> Picture
stroke c = Attr [("stroke", colorText c)]

-- | Set how thick the outline ('stroke') is.
strokeWidth :: Double -> Picture -> Picture
strokeWidth w = Attr [("stroke-width", num w)]

-- | Make a picture see-through: @1@ is solid, @0@ is invisible.
opacity :: Double -> Picture -> Picture
opacity o = Attr [("opacity", num o)]

-- | Slide a picture across by @(dx, dy)@.
translate :: Point -> Picture -> Picture
translate (dx, dy) = Attr [("transform", "translate(" ++ num dx ++ "," ++ num dy ++ ")")]

-- | Grow or shrink a picture by @(sx, sy)@ (1 = same size).
scale :: Point -> Picture -> Picture
scale (sx, sy) = Attr [("transform", "scale(" ++ num sx ++ "," ++ num sy ++ ")")]

-- | Turn a picture clockwise by an angle in __degrees__, about the origin.
rotate :: Double -> Picture -> Picture
rotate deg = Attr [("transform", "rotate(" ++ num deg ++ ")")]

-- | Combine many pictures into one, drawn in order (later ones on top).
group :: [Picture] -> Picture
group = mconcat

{- | Draw a simple line chart of @(x, y)@ points, auto-scaled to fill the canvas
(with a margin) and with left\/bottom axes. Great for plotting a running total
from a data stream — e.g. @lineChart defaultCanvas (occurrencesOf (scanlE (+) 0 e))@.

Returns the empty picture for fewer than two points.
-}
lineChart :: Canvas -> [(Double, Double)] -> Picture
lineChart (Canvas w h) pts
    | length pts < 2 = mempty
    | otherwise = axes <> strokeWidth 2 (polyline scaled)
  where
    m = 30
    xs = map fst pts
    ys = map snd pts
    spanX = let d = maximum xs - minimum xs in if d == 0 then 1 else d
    spanY = let d = maximum ys - minimum ys in if d == 0 then 1 else d
    sx x = m + (x - minimum xs) / spanX * (w - 2 * m)
    sy y = (h - m) - (y - minimum ys) / spanY * (h - 2 * m)
    scaled = [(sx x, sy y) | (x, y) <- pts]
    axes = line (m, m) (m, h - m) <> line (m, h - m) (w - m, h - m)

-- | The default 300×300 canvas used by 'picture'.
defaultCanvas :: Canvas
defaultCanvas = Canvas 300 300

{- | Show a picture in the notebook on the default 300×300 canvas.

>>> picture (fill red (circle (150,150) 80))
-}
picture :: Picture -> IO ()
picture = pictureOn defaultCanvas

-- | Show a picture on a canvas of your chosen size.
pictureOn :: Canvas -> Picture -> IO ()
pictureOn cv p = do
    putStrLn "<!-- MIME:image/svg+xml -->"
    putStrLn (renderSvg cv p)
