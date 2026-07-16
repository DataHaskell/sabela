{- |
Module      : Sabela.Notebook.Picture.Svg
Description : Render a picture to an SVG string.

The structural rendering of a 'Picture' into SVG. Split out of the
'Sabela.Notebook.Picture' facade (which re-exports 'renderSvg' and 'svgBody')
to keep each module under the size cap.
-}
module Sabela.Notebook.Picture.Svg (
    renderSvg,
    svgBody,
) where

import Sabela.Notebook.Markup (Svg (..))
import Sabela.Notebook.Picture.Internal (
    Canvas (..),
    Picture (..),
    Shape (..),
    num,
 )

{- | Render a picture to a standalone SVG string on a given canvas.

The body is built by 'svgBody', which is a /monoid homomorphism/: the drawing of
@a '<>' b@ is exactly the drawing of @a@ followed by the drawing of @b@. The
@\<svg\>@ wrapper is added once around the whole thing.
-}
renderSvg :: Canvas -> Picture -> Svg
renderSvg (Canvas w h) p =
    Svg $
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\""
            ++ num w
            ++ "\" height=\""
            ++ num h
            ++ "\" viewBox=\"0 0 "
            ++ num w
            ++ " "
            ++ num h
            ++ "\">"
            ++ svgBody p
            ++ "</svg>"

{- | The structural rendering of a picture's contents (no @\<svg\>@ wrapper).
Styling and transforms become nested @\<g\>@ groups, so inner styles win via
SVG's normal inheritance.
-}
svgBody :: Picture -> String
svgBody Blank = ""
svgBody (Prim s) = shapeSvg s
svgBody (Over a b) = svgBody a ++ svgBody b
svgBody (Raw s) = s
svgBody (Attr as p) = "<g" ++ concatMap attr as ++ ">" ++ svgBody p ++ "</g>"

attr :: (String, String) -> String
attr (k, v) = " " ++ k ++ "=\"" ++ v ++ "\""

shapeSvg :: Shape -> String
shapeSvg (Circle (x, y) r) =
    "<circle cx=\"" ++ num x ++ "\" cy=\"" ++ num y ++ "\" r=\"" ++ num r ++ "\"/>"
shapeSvg (Rectangle (x, y) w h) =
    "<rect x=\""
        ++ num x
        ++ "\" y=\""
        ++ num y
        ++ "\" width=\""
        ++ num w
        ++ "\" height=\""
        ++ num h
        ++ "\"/>"
shapeSvg (Line (x1, y1) (x2, y2)) =
    "<line x1=\""
        ++ num x1
        ++ "\" y1=\""
        ++ num y1
        ++ "\" x2=\""
        ++ num x2
        ++ "\" y2=\""
        ++ num y2
        ++ "\" stroke=\"black\"/>"
shapeSvg (Polyline ps) =
    "<polyline points=\"" ++ pointList ps ++ "\" fill=\"none\" stroke=\"black\"/>"
shapeSvg (Polygon ps) =
    "<polygon points=\"" ++ pointList ps ++ "\"/>"
shapeSvg (PathD d) = "<path d=\"" ++ d ++ "\"/>"
shapeSvg (Label (x, y) s) =
    "<text x=\"" ++ num x ++ "\" y=\"" ++ num y ++ "\">" ++ s ++ "</text>"

pointList :: [(Double, Double)] -> String
pointList = unwords . map (\(x, y) -> num x ++ "," ++ num y)
