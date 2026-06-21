{-# LANGUAGE OverloadedStrings #-}

{- | Pure HTML for the public gallery pages (feed, collection landing, and the
opaque-origin sandboxed reader). Kept separate from the store so the store stays
testable without HTML, mirroring the "Hub.Pages" leaf discipline. Every
user/admin string is 'htmlEscape'd; every slug is re-checked with 'validSlug'
before it reaches an attribute.
-}
module Hub.Gallery.Render (
    GalleryChrome (..),
    htmlEscape,
    itemTitle,
    itemRelPath,
    renderGallery,
    renderCollectionPage,
    renderCollectionReader,
) where

import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Text as T

import Hub.Gallery.Chrome (
    GalleryChrome (..),
    footer,
    h3,
    htmlEscape,
    link,
    meta,
    ogTags,
    p,
    page,
 )
import Hub.Gallery.Store (CollectionView (..), GalleryItem (..))
import Hub.Share (Share (..), validSlug)
import Hub.Types (exportModeText)

itemTitle :: GalleryItem -> Text
itemTitle (GItemShare s _ _) = shareTitle s
itemTitle (GItemCollection cv) = cvTitle cv

-- | The relative URL a top-level entry links to (slug re-validated).
itemRelPath :: GalleryItem -> Text
itemRelPath (GItemShare s _ _)
    | validSlug (shareSlug s) = "/s/" <> shareSlug s
    | otherwise = "#"
itemRelPath (GItemCollection cv)
    | validSlug (cvCid cv) = "/c/" <> cvCid cv
    | otherwise = "#"

itemTags :: GalleryItem -> [Text]
itemTags (GItemShare _ ts _) = ts
itemTags (GItemCollection cv) = cvTags cv

-- Gallery feed page

renderGallery :: GalleryChrome -> Maybe Text -> [GalleryItem] -> Text
renderGallery chrome activeFilter items =
    page chrome "Sabela Community Gallery" "/gallery" body
  where
    known = sort (nub (concatMap itemTags items))
    valid = activeFilter >>= \t -> if t `elem` known then Just t else Nothing
    shown = maybe items (\t -> filter ((t `elem`) . itemTags) items) valid
    body =
        masthead
            <> chipRow valid known
            <> "<main class=\"feed\">"
            <> feed shown
            <> "</main>"

masthead :: Text
masthead =
    "<section class=\"masthead\"><span class=\"kick\">// reactive Haskell notebooks</span>"
        <> "<h1>A gallery of Sabela notebooks.</h1>"
        <> "<p>Work people have published. Read it in your browser, or run it yourself.</p>"
        <> "</section>"

feed :: [GalleryItem] -> Text
feed [] = "<p class=\"empty\">Nothing featured yet.</p>"
feed (x : xs) =
    cell 1 "spotlight" x <> T.concat (zipWith (`cell` "") [2 ..] xs)

chipRow :: Maybe Text -> [Text] -> Text
chipRow _ [] = ""
chipRow active tags =
    "<nav class=\"chips\">"
        <> T.concat [chip (Just t == active) t | t <- tags]
        <> "</nav>"
  where
    chip on t =
        "<a class=\"chip"
            <> (if on then " on" else "")
            <> "\" href=\"/gallery?tag="
            <> t
            <> "\">"
            <> htmlEscape t
            <> "</a>"

-- Cards (rendered as notebook "cells" with a catalog counter)

cell :: Int -> Text -> GalleryItem -> Text
cell n cls (GItemShare s tags author) =
    cellWrap
        cls
        n
        "Notebook"
        (relShare s)
        (shareTitle s)
        body
        (shareActions (shareSlug s))
  where
    body =
        byline author
            <> meta (exportModeText (shareMode s) <> " · " <> shareCreatedAt s)
            <> tagChips tags
cell n cls (GItemCollection cv) =
    cellWrap
        (T.unwords [cls, "coll"])
        n
        kind
        (relCollection cv)
        (cvTitle cv)
        body
        open
  where
    body = p (cvDescription cv) <> tagChips (cvTags cv)
    kind = "Collection / " <> T.pack (show (length (cvMembers cv)))
    open = "<span class=\"open\">view ▸</span>"

{- | A card is an @article@ (not one big @a@) so its footer can hold a Download
link and a Fork @form@ — interactive elements can't nest inside an @a@.
-}
cellWrap :: Text -> Int -> Text -> Text -> Text -> Text -> Text -> Text
cellWrap cls n kind href title body actions =
    "<article class=\"cell "
        <> cls
        <> "\"><div class=\"crow\"><span class=\"num\">No. "
        <> pad n
        <> "</span><span class=\"kind\">"
        <> kind
        <> "</span></div><a class=\"ctitle\" href=\""
        <> href
        <> "\">"
        <> h3 title
        <> "</a>"
        <> body
        <> "<div class=\"cfoot\">"
        <> actions
        <> "</div></article>"
  where
    pad x = (if x < 10 then "0" else "") <> T.pack (show x)

{- | Download (anyone) + Fork (a POST form; the hub gates auth/Origin) for a
share card. A non-hex slug yields no actions.
-}
shareActions :: Text -> Text
shareActions slug
    | not (validSlug slug) = ""
    | otherwise =
        "<a class=\"act\" href=\"/_hub/source/"
            <> slug
            <> "\">Download .md</a>"
            <> "<form class=\"act\" method=\"post\" action=\"/_hub/fork/"
            <> slug
            <> "\"><button>Fork ▸</button></form>"

relShare :: Share -> Text
relShare s = if validSlug (shareSlug s) then "/s/" <> shareSlug s else "#"

relCollection :: CollectionView -> Text
relCollection cv = if validSlug (cvCid cv) then "/c/" <> cvCid cv else "#"

-- | An author byline ("by <name>"), shown when a curator attributed the work.
byline :: Maybe Text -> Text
byline Nothing = ""
byline (Just a) = "<p class=\"byline\">by " <> htmlEscape a <> "</p>"

tagChips :: [Text] -> Text
tagChips [] = ""
tagChips ts =
    "<p class=\"cardtags\">"
        <> T.concat ["<span class=\"tg\">#" <> htmlEscape t <> "</span>" | t <- ts]
        <> "</p>"

-- Collection landing + reader

renderCollectionPage :: GalleryChrome -> CollectionView -> Text
renderCollectionPage chrome cv =
    page chrome (cvTitle cv) (relCollection cv) body
  where
    body =
        "<section class=\"masthead\"><h1>"
            <> htmlEscape (cvTitle cv)
            <> "</h1><p>"
            <> htmlEscape (cvDescription cv)
            <> "</p></section><main class=\"feed\">"
            <> T.concat (zipWith memberCard [0 ..] (cvMembers cv))
            <> "</main>"
    memberCard i s =
        cellWrap
            ""
            (i + 1)
            "Notebook"
            (readerPath cv i)
            (shareTitle s)
            (meta (exportModeText (shareMode s)))
            "<span class=\"open\">read ▸</span>"

renderCollectionReader :: GalleryChrome -> CollectionView -> Int -> Text
renderCollectionReader chrome cv n =
    page chrome (cvTitle cv) (relCollection cv) body
  where
    members = cvMembers cv
    total = length members
    slug = case drop n members of
        (s : _) -> shareSlug s
        [] -> ""
    frame
        | validSlug slug =
            "<iframe class=\"reader\" sandbox=\"allow-scripts allow-popups allow-forms\" src=\"/s/"
                <> slug
                <> "\"></iframe>"
        | otherwise = "<p>Notebook unavailable.</p>"
    prev
        | n > 0 = link (readerPath cv (n - 1)) "‹ prev"
        | otherwise = ""
    next
        | n < total - 1 = link (readerPath cv (n + 1)) "next ›"
        | otherwise = ""
    body =
        "<nav class=\"reader-nav\">"
            <> prev
            <> "<span> "
            <> htmlEscape (cvTitle cv)
            <> " </span>"
            <> next
            <> "</nav>"
            <> frame

readerPath :: CollectionView -> Int -> Text
readerPath cv i = relCollection cv <> "/" <> T.pack (show i)
