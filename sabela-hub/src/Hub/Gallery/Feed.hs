{-# LANGUAGE OverloadedStrings #-}

{- | Pure RSS + sitemap over the resolved gallery feed — the cheapest retention
channel for an RSS-driven ecosystem (Planet Haskell, Haskell Weekly). Entries
are in @index@ order; titles are 'htmlEscape'd (XML shares the @& < >@ escapes);
links are absolute against the configured origin.
-}
module Hub.Gallery.Feed (
    renderFeed,
    renderSitemap,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Hub.Gallery.Render (
    GalleryChrome (..),
    htmlEscape,
    itemRelPath,
    itemTitle,
 )
import Hub.Gallery.Store (GalleryItem)

absUrl :: GalleryChrome -> GalleryItem -> Text
absUrl chrome i = gcOrigin chrome <> itemRelPath i

renderFeed :: GalleryChrome -> [GalleryItem] -> Text
renderFeed chrome items =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        <> "<rss version=\"2.0\"><channel>"
        <> "<title>Sabela Community Gallery</title>"
        <> "<link>"
        <> gcOrigin chrome
        <> "/gallery</link>"
        <> "<description>Curated reactive Haskell notebooks.</description>"
        <> T.concat (map entry items)
        <> "</channel></rss>"
  where
    entry i =
        let url = absUrl chrome i
         in "<item><title>"
                <> htmlEscape (itemTitle i)
                <> "</title><link>"
                <> url
                <> "</link><guid isPermaLink=\"true\">"
                <> url
                <> "</guid></item>"

renderSitemap :: GalleryChrome -> [GalleryItem] -> Text
renderSitemap chrome items =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        <> "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"
        <> loc (gcOrigin chrome <> "/gallery")
        <> T.concat [loc (absUrl chrome i) | i <- items]
        <> "</urlset>"
  where
    loc u = "<url><loc>" <> u <> "</loc></url>"
