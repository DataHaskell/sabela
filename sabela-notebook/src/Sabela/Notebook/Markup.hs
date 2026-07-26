module Sabela.Notebook.Markup (
    Svg (..),
    Html (..),
) where

newtype Svg = Svg {unSvg :: String}
    deriving (Eq, Show)

newtype Html = Html {unHtml :: String}
    deriving (Eq, Show)
