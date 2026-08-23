module Sabela.Notebook.Picture.Types (
    Point,
    Picture (..),
    Shape (..),
    Canvas (..),
    num,
) where

type Point = (Double, Double)

data Picture
    = Blank
    | Prim Shape
    | Over Picture Picture
    | Attr [(String, String)] Picture
    | Raw String

instance Semigroup Picture where
    Blank <> q = q
    p <> Blank = p
    p <> q = Over p q

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

data Canvas = Canvas
    { canvasWidth :: Double
    , canvasHeight :: Double
    }

num :: Double -> String
num x
    | x == fromIntegral r = show r
    | otherwise = show x
  where
    r = round x :: Integer
