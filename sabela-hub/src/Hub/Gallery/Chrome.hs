{-# LANGUAGE OverloadedStrings #-}

module Hub.Gallery.Chrome (
    GalleryChrome (..),
    htmlEscape,
    page,
    ogTags,
    footer,
    link,
    h3,
    p,
    meta,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Hub.Gallery.Style (styleBlock)

data GalleryChrome = GalleryChrome
    { gcOrigin :: Text
    , gcContact :: Maybe Text
    , gcRepoUrl :: Text
    }

htmlEscape :: Text -> Text
htmlEscape =
    T.replace "\"" "&quot;"
        . T.replace ">" "&gt;"
        . T.replace "<" "&lt;"
        . T.replace "&" "&amp;"

page :: GalleryChrome -> Text -> Text -> Text -> Text
page chrome title relPath body =
    "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
        <> "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">"
        <> "<title>"
        <> htmlEscape title
        <> "</title>"
        <> ogTags chrome title relPath
        <> styleBlock
        <> "</head><body><header class=\"top\">"
        <> "<a class=\"brand\" href=\"/gallery\"><span class=\"lam\">λ</span> Sabela</a>"
        <> "<nav class=\"hnav\"><a href=\"https://github.com/DataHaskell/sabela\">Source</a>"
        <> "<a class=\"signin\" href=\"/_hub/login\">Sign in</a></nav></header>"
        <> body
        <> footer chrome
        <> "</body></html>"

ogTags :: GalleryChrome -> Text -> Text -> Text
ogTags chrome title relPath =
    meta' "og:title" (htmlEscape title)
        <> meta' "og:description" "Curated reactive Haskell notebooks."
        <> meta' "og:url" (gcOrigin chrome <> relPath)
        <> meta' "og:image" (gcOrigin chrome <> "/gallery/og.png")
        <> "<meta name=\"twitter:card\" content=\"summary_large_image\">"
  where
    meta' k v =
        "<meta property=\"" <> k <> "\" content=\"" <> v <> "\">"

footer :: GalleryChrome -> Text
footer chrome =
    "<footer class=\"cta\"><p>Sabela is open source. Run any notebook locally: "
        <> "<a href=\""
        <> gcRepoUrl chrome
        <> "?utm_source=gallery\">"
        <> htmlEscape (gcRepoUrl chrome)
        <> "</a></p>"
        <> maybe "" contactLine (gcContact chrome)
        <> "</footer>"
  where
    contactLine c =
        "<p>Want a hosted account? <a href=\"mailto:"
            <> c
            <> "?subject=Gallery%20access%20request\">Request access</a></p>"

link :: Text -> Text -> Text
link href inner = "<a href=\"" <> href <> "\">" <> inner <> "</a>"

h3 :: Text -> Text
h3 t = "<h3>" <> htmlEscape t <> "</h3>"

p :: Text -> Text
p t = "<p class=\"desc\">" <> htmlEscape t <> "</p>"

meta :: Text -> Text
meta t = "<p class=\"meta\">" <> htmlEscape t <> "</p>"
