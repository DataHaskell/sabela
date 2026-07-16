{- | Typed render outputs: an SVG and an HTML document, each a newtype over the
raw string, so a rendered artifact cannot be mistaken for arbitrary text. Show a
value with 'Sabela.Notebook.Picture.picture' / 'Sabela.Notebook.Anim.animate',
which display directly; these newtypes exist mainly to keep the wrong path from
type-checking.
-}
module Sabela.Notebook.Markup (
    Svg (..),
    Html (..),
) where

-- | A rendered SVG document (the output of 'Sabela.Notebook.Picture.renderSvg').
newtype Svg = Svg {unSvg :: String}
    deriving (Eq, Show)

-- | A rendered HTML document (the output of 'renderAnimation').
newtype Html = Html {unHtml :: String}
    deriving (Eq, Show)
