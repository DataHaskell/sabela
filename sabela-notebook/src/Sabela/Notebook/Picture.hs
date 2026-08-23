module Sabela.Notebook.Picture (
    Picture,
    Point,
    circle,
    rectangle,
    line,
    polyline,
    polygon,
    path,
    text,
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
    fill,
    stroke,
    strokeWidth,
    opacity,
    translate,
    scale,
    rotate,
    group,
    Canvas (..),
    defaultCanvas,
    displayPicture,
    displayPictureOn,
    renderSvg,
    svgBody,
    fromSvg,
) where

import Data.List (intercalate)
import Sabela.Notebook.Markup (unSvg)
import Sabela.Notebook.Picture.Svg (renderSvg, svgBody)
import Sabela.Notebook.Picture.Types (
    Canvas (..),
    Picture (..),
    Point,
    Shape (..),
    num,
 )

circle :: Point -> Double -> Picture
circle c r = Prim (Circle c r)

rectangle :: Point -> Double -> Double -> Picture
rectangle tl w h = Prim (Rectangle tl w h)

line :: Point -> Point -> Picture
line a b = Prim (Line a b)

polyline :: [Point] -> Picture
polyline = Prim . Polyline

polygon :: [Point] -> Picture
polygon = Prim . Polygon

path :: String -> Picture
path = Prim . PathD

text :: Point -> String -> Picture
text p s = Prim (Label p s)

fromSvg :: String -> Picture
fromSvg = Raw

newtype Color = Color {colorText :: String}

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

fill :: Color -> Picture -> Picture
fill c = Attr [("fill", colorText c)]

stroke :: Color -> Picture -> Picture
stroke c = Attr [("stroke", colorText c)]

strokeWidth :: Double -> Picture -> Picture
strokeWidth w = Attr [("stroke-width", num w)]

opacity :: Double -> Picture -> Picture
opacity o = Attr [("opacity", num o)]

translate :: Point -> Picture -> Picture
translate (dx, dy) = Attr [("transform", "translate(" ++ num dx ++ "," ++ num dy ++ ")")]

scale :: Point -> Picture -> Picture
scale (sx, sy) = Attr [("transform", "scale(" ++ num sx ++ "," ++ num sy ++ ")")]

rotate :: Double -> Picture -> Picture
rotate deg = Attr [("transform", "rotate(" ++ num deg ++ ")")]

group :: [Picture] -> Picture
group = mconcat

defaultCanvas :: Canvas
defaultCanvas = Canvas 300 300

displayPicture :: Picture -> IO ()
displayPicture = displayPictureOn defaultCanvas

displayPictureOn :: Canvas -> Picture -> IO ()
displayPictureOn cv p = do
    putStrLn "<!-- MIME:image/svg+xml -->"
    putStrLn (unSvg (renderSvg cv p))
