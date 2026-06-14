{-# LANGUAGE OverloadedStrings #-}

{- | Public gallery HTTP handlers: the @\/gallery@ feed (with @?tag=@), the
@\/c\/<cid>@ collection landing + @\/c\/<cid>\/<n>@ reader, and the
@feed.xml@ / @sitemap.xml@ surfaces. All resolve the curated index against the
live 'ShareStore' (soft refs) and render via the pure "Hub.Gallery.Render".
-}
module Hub.Gallery.Public (
    chromeFrom,
    galleryHeaders,
    serveGallery,
    serveCollection,
    serveCollectionReader,
    serveFeed,
    serveSitemap,
    serveSource,
) where

import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types
import Network.Wai
import Text.Read (readMaybe)

import Data.Char (isAsciiLower, isDigit)
import Data.List (find)
import Hub.Gallery (
    GalleryStore,
    cvMembers,
    listGallery,
    publicSlugs,
    resolveCollection,
    validTag,
 )
import Hub.Gallery.Feed (renderFeed, renderSitemap)
import Hub.Gallery.Render (
    GalleryChrome (..),
    renderCollectionPage,
    renderCollectionReader,
    renderGallery,
 )
import Hub.Share (
    Share (..),
    ShareStore,
    listAllShares,
    lookupShareSource,
    validSlug,
 )
import Hub.Types (HubConfig (..))

-- | Build the render chrome (origin / contact / repo) from config.
chromeFrom :: HubConfig -> GalleryChrome
chromeFrom cfg =
    GalleryChrome
        { gcOrigin = originOf (hcGoogleRedirectUri cfg)
        , gcContact = hcAdminContact cfg
        , gcRepoUrl = "https://github.com/DataHaskell/sabela"
        }
  where
    originOf uri = T.intercalate "/" (take 3 (T.splitOn "/" uri))

{- | Anti-framing + html headers for the gallery's own pages, so a third-party
site can't frame the gallery (only the share→reader same-origin framing is
intended).
-}
galleryHeaders :: [Header]
galleryHeaders =
    [ ("Content-Type", "text/html; charset=utf-8")
    , ("X-Content-Type-Options", "nosniff")
    , ("X-Frame-Options", "SAMEORIGIN")
    , ("Content-Security-Policy", "frame-ancestors 'self'")
    ]

html :: Status -> [Header] -> Text -> Response
html st hdrs t = responseLBS st hdrs (BL.fromStrict (TE.encodeUtf8 t))

htmlOk :: Text -> Response
htmlOk = html status200 galleryHeaders

notFound :: Response
notFound = html status404 galleryHeaders "<h1>Not found</h1>"

serveGallery ::
    HubConfig ->
    GalleryStore ->
    ShareStore ->
    Request ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
serveGallery cfg gallery shares req respond = do
    items <- listGallery gallery shares
    let raw = case lookup "tag" (queryString req) of
            Just (Just bs) -> Just (TE.decodeUtf8 bs)
            _ -> Nothing
        active = raw >>= \t -> if validTag t then Just t else Nothing
    respond $ htmlOk (renderGallery (chromeFrom cfg) active items)

serveCollection ::
    HubConfig ->
    GalleryStore ->
    ShareStore ->
    Text ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
serveCollection cfg gallery shares cid respond = do
    mcv <- resolveCollection gallery shares cid
    respond $ case mcv of
        Just cv -> htmlOk (renderCollectionPage (chromeFrom cfg) cv)
        Nothing -> notFound

serveCollectionReader ::
    HubConfig ->
    GalleryStore ->
    ShareStore ->
    Text ->
    Text ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
serveCollectionReader cfg gallery shares cid nTxt respond = do
    mcv <- resolveCollection gallery shares cid
    respond $ case (mcv, readMaybe (T.unpack nTxt) :: Maybe Int) of
        (Just cv, Just n)
            | n >= 0 && n < length (cvMembers cv) ->
                htmlOk (renderCollectionReader (chromeFrom cfg) cv n)
        _ -> notFound

serveFeed ::
    HubConfig ->
    GalleryStore ->
    ShareStore ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
serveFeed cfg gallery shares respond = do
    items <- listGallery gallery shares
    respond $
        html
            status200
            [("Content-Type", "application/rss+xml; charset=utf-8")]
            (renderFeed (chromeFrom cfg) items)

serveSitemap ::
    HubConfig ->
    GalleryStore ->
    ShareStore ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
serveSitemap cfg gallery shares respond = do
    items <- listGallery gallery shares
    respond $
        html
            status200
            [("Content-Type", "application/xml; charset=utf-8")]
            (renderSitemap (chromeFrom cfg) items)

{- | Download: serve a public notebook's source markdown as an attachment.
Unauthenticated, so the **public-slug** check (featured or a collection member)
is the only gate (F2) — a non-public or unknown slug 404s.
-}
serveSource ::
    GalleryStore ->
    ShareStore ->
    Text ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
serveSource gallery shares slug respond = do
    pub <- publicSlugs gallery
    if not (validSlug slug) || slug `notElem` pub
        then respond notFound
        else do
            msrc <- lookupShareSource shares slug
            title <-
                maybe "" shareTitle . find ((== slug) . shareSlug) <$> listAllShares shares
            let disp = "attachment; filename=\"" <> downloadName title <> "\""
            respond $ case msrc of
                Just src ->
                    responseLBS
                        status200
                        [ ("Content-Type", "text/markdown; charset=utf-8")
                        , ("Content-Disposition", TE.encodeUtf8 disp)
                        , ("X-Content-Type-Options", "nosniff")
                        ]
                        (BL.fromStrict src)
                Nothing -> notFound

{- | A download filename from a notebook title: lowercased, non-alphanumerics
collapsed to hyphens, capped, @.md@ appended (falls back to @sabela-notebook@).
-}
downloadName :: Text -> Text
downloadName title =
    let mapped =
            T.map (\c -> if isAsciiLower c || isDigit c then c else '-') (T.toLower title)
        collapsed = T.intercalate "-" (filter (not . T.null) (T.splitOn "-" mapped))
        capped = T.dropWhileEnd (== '-') (T.take 60 collapsed)
     in (if T.null capped then "sabela-notebook" else capped) <> ".md"
