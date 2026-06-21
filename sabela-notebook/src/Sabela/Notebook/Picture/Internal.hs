{- |
Module      : Sabela.Notebook.Picture.Internal
Description : Core picture/shape/canvas types shared by the facade and renderer.

The data types live here (with their constructors exposed) so both the public
'Sabela.Notebook.Picture' facade and the 'Sabela.Notebook.Picture.Svg' renderer
can import them without a module cycle. End users go through the facade, which
re-exports the abstract names; this module is internal.
-}
module Sabela.Notebook.Picture.Internal (
    Point,
    Picture (..),
    Shape (..),
    Canvas (..),
    num,
) where

{- | A point on the canvas, @(x, y)@ — @x@ rightward, @y@ downward, from the
top-left corner.
-}
type Point = (Double, Double)

{- | A drawing. Build one from shapes and combine pictures with @'<>'@; the empty
picture is 'mempty'. A picture is just a description — nothing is drawn until you
call @picture@.
-}
data Picture
    = Blank
    | Prim Shape
    | Over Picture Picture
    | Attr [(String, String)] Picture
    | Raw String

-- | Combine two pictures by drawing the second on top of the first.
instance Semigroup Picture where
    Blank <> q = q
    p <> Blank = p
    p <> q = Over p q

-- | The empty picture draws nothing.
instance Monoid Picture where
    mempty = Blank

data Shape
    = Circle Point Double
    | Rectangle Point Double Double
    | Line Point Point
    | Polyline [Point]
    | Polygon [Point]
    | PathD String
    | Label Point String

-- | A drawing area of the given width and height (in canvas units).
data Canvas = Canvas
    { canvasWidth :: Double
    , canvasHeight :: Double
    }

-- | Format a number for SVG: whole numbers lose their trailing @.0@.
num :: Double -> String
num x
    | x == fromIntegral r = show r
    | otherwise = show x
  where
    r = round x :: Integer
