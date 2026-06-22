{-# LANGUAGE OverloadedStrings #-}

{- | Seed a demo\/curated gallery from repo example notebooks: write each
share's @index.html@ + @meta@ + @source.md@, plus the gallery curation files
(@index@ \/ @attribution@ \/ @tags@) and the Learn You a Haskell collection.
Port of the former @sabela-hub\/scripts\/seed-gallery.py@, extended with a
@collection@ writer.

Run from the repo root (the dir holding @examples\/@ and @sabela-hub\/@):

> sabela-hub seed-gallery [DATA_ROOT] [REPO_ROOT]
-}
module Hub.Gallery.Seed (
    Curated (..),
    RenderSpec (..),
    Collection (..),
    curation,
    lyahChapters,
    lyahCollection,
    seedGallery,
) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

import Hub.Banner (spliceBanner)
import Hub.Gallery.Lyah (LyahChapter (..), lyahChapterTable)
import Hub.Gallery.SeedAssets (rewriteAssets)
import Hub.Gallery.SeedRender (brandDashboard, page, renderBody)
import Hub.Meta (writeMetaLine)
import Hub.Runner (spliceRunner)

-- | How a share's static @index.html@ is produced from its Markdown.
data RenderSpec
    = -- | Render the Markdown directly (code blocks shown).
      FromMarkdown
    | -- | Use a pre-built dashboard export (path relative to @sabela-hub\/@).
      FromDashboard FilePath
    | -- | Rewrite @\/api\/asset@ refs (base, positional model list) then render.
      FromAssets Text [Text]

-- | A curated share: where its source lives and how to render it.
data Curated = Curated
    { cSlug :: Text
    , cFile :: FilePath
    , cTitle :: Text
    , cAuthor :: Text
    , cTags :: [Text]
    , cRender :: RenderSpec
    }

-- | A featured collection: a title, description, and ordered member slugs.
data Collection = Collection
    { colCid :: Text
    , colTitle :: Text
    , colDescription :: Text
    , colTags :: [Text]
    , colMembers :: [Text]
    }

owner :: Text
owner = "curators@sabela.dev"

createdAt :: Text
createdAt = "2026-06-12T00:00:00Z"

{- | ASCII-only: gallery @meta@\/@attribution@ files are read with the hub
container's C locale, which cannot decode a UTF-8 @\269@ (Lipova\269a).
-}
lyahAuthor :: Text
lyahAuthor = "Miran Lipovaca (CC BY-NC-SA 3.0)"

-- | The five originally-curated single-notebook shares (unchanged behaviour).
curation :: [Curated]
curation =
    [ Curated
        "c56a0001"
        "examples/CSG.md"
        "What is Constructive Solid Geometry?"
        "Joe Warren"
        ["geometry", "graphics", "3d"]
        ( FromAssets
            "https://raw.githubusercontent.com/joe-warren/sabela/0b87c7bc183323cc38db5e5a2f20a500473a425a/waterfall/"
            [ "563306"
            , "271686"
            , "795000"
            , "796533"
            , "492778"
            , "747853"
            , "396714"
            , "469117"
            , "559254"
            ]
        )
    , Curated
        "b1ef0001"
        "examples/bluefin.md"
        "A tour of Bluefin"
        "Tom Ellis"
        ["effects", "tutorial"]
        (FromDashboard "scripts/dashboards/bluefin.html")
    , Curated
        "ca1f0001"
        "examples/CaliforniaHousing.md"
        "California Housing: From Exploration to Linear Regression"
        "DataHaskell"
        ["regression", "dataframe", "hasktorch"]
        (FromDashboard "scripts/dashboards/california.html")
    , Curated
        "f12a0001"
        "examples/frp-tutorial.md"
        "Functional Reactive Programming in Sabela"
        "DataHaskell"
        ["frp", "animation", "tutorial"]
        (FromDashboard "scripts/dashboards/frp.html")
    , Curated
        "c0de0001"
        "examples/tutorial-python-integration.md"
        "Haskell and Python in one notebook"
        "DataHaskell"
        ["python", "interop", "matplotlib"]
        (FromDashboard "scripts/dashboards/python.html")
    ]

{- | The 14 Learn You a Haskell chapters, in reading order. Slugs are
@1ea40001@…@1ea4000e@; sources are the converter's output under
@examples\/lyah\/@.
-}
lyahChapters :: [Curated]
lyahChapters = map fromChapter lyahChapterTable
  where
    fromChapter ch =
        Curated
            (T.pack ("1ea4" <> pad4 (lcNum ch)))
            ("examples/lyah" </> numbered ch)
            (lcTitle ch)
            lyahAuthor
            (lcTags ch)
            FromMarkdown
    numbered ch = pad2 (lcNum ch) <> "-" <> T.unpack (lcSlug ch) <> ".md"
    pad2 n = let s = show n in replicate (2 - length s) '0' <> s
    pad4 n = let s = show n in replicate (4 - length s) '0' <> s

-- | The collection grouping the 14 chapters at @/c/1ea40000@.
lyahCollection :: Collection
lyahCollection =
    Collection
        { colCid = "1ea40000"
        , colTitle = "Learn You a Haskell for Great Good!"
        , colDescription =
            "Miran Lipovaca's classic introduction to Haskell, ported to runnable "
                <> "Sabela notebooks. Adapted under CC BY-NC-SA 3.0."
        , colTags = ["haskell", "tutorial", "book"]
        , colMembers = map cSlug lyahChapters
        }

-- ---------------------------------------------------------------------------
-- Seeding
-- ---------------------------------------------------------------------------

seedGallery :: FilePath -> FilePath -> IO ()
seedGallery repoRoot dataRoot = do
    let shares = dataRoot </> "shares"
        gallery = dataRoot </> "gallery"
        allShares = curation ++ lyahChapters
    createDirectoryIfMissing True (gallery </> "collections")
    forM_ allShares (seedShare repoRoot shares)

    -- Top-level feed: the five originals + the LYAH collection. Chapter shares
    -- exist on disk but surface only through the collection.
    let indexLines =
            map (writeMetaLine "share" . cSlug) curation
                ++ [writeMetaLine "collection" (colCid lyahCollection)]
        attrLines = [writeMetaLine (cSlug c) (cAuthor c) | c <- allShares]
        tagLines =
            [writeMetaLine (cSlug c) (T.intercalate "," (cTags c)) | c <- curation]
                ++ [ writeMetaLine
                        (colCid lyahCollection)
                        (T.intercalate "," (colTags lyahCollection))
                   ]
    writeText (gallery </> "index") (T.unlines indexLines)
    writeText (gallery </> "attribution") (T.unlines attrLines)
    writeText (gallery </> "tags") (T.unlines tagLines)
    writeCollection gallery lyahCollection
    putStrLn ("gallery seeded at " <> dataRoot)

{- | Seed one share. The static @index.html@ always gets the fork banner (as the
live publish path does). A 'FromMarkdown' share — the LYAH chapters — also gets
the WASM MicroHs runner with its source as the data island, so it runs in the
browser; the dashboard\/asset exports are pre-rendered outputs MicroHs can't run,
so they keep the banner only.
-}
seedShare :: FilePath -> FilePath -> Curated -> IO ()
seedShare repoRoot shares c = do
    md <- TIO.readFile (repoRoot </> cFile c)
    let sdir = shares </> T.unpack (cSlug c)
        slug = cSlug c
    writeText (sdir </> "source.md") md
    indexHtml <- renderShare repoRoot c md
    let banner = spliceBanner slug (TE.encodeUtf8 indexHtml)
        final = case cRender c of
            FromMarkdown -> spliceRunner slug md banner
            _ -> banner
    writeBytes (sdir </> "index.html") final
    writeText
        (sdir </> "meta")
        ( T.unlines
            [ writeMetaLine "owner" owner
            , writeMetaLine "mode" "dashboard"
            , writeMetaLine "createdAt" createdAt
            , writeMetaLine "title" (cTitle c)
            ]
        )
    putStrLn ("seeded " <> T.unpack (cSlug c) <> "  " <> T.unpack (cTitle c))

renderShare :: FilePath -> Curated -> Text -> IO Text
renderShare repoRoot c md = case cRender c of
    FromMarkdown -> pure (renderMd md)
    FromAssets base models -> pure (renderMd (rewriteAssets base models md))
    FromDashboard rel -> brandDashboard <$> TIO.readFile (repoRoot </> "sabela-hub" </> rel)
  where
    renderMd m = page (cTitle c) (cAuthor c) (cSlug c) (renderBody m)

writeCollection :: FilePath -> Collection -> IO ()
writeCollection gallery col =
    writeText
        (gallery </> "collections" </> T.unpack (colCid col) </> "meta")
        ( T.unlines
            [ writeMetaLine "title" (colTitle col)
            , writeMetaLine "description" (colDescription col)
            , writeMetaLine "createdAt" createdAt
            , writeMetaLine "members" (T.intercalate "," (colMembers col))
            ]
        )

writeText :: FilePath -> Text -> IO ()
writeText path txt = do
    createDirectoryIfMissing True (takeDirectory path)
    TIO.writeFile path txt

writeBytes :: FilePath -> BS.ByteString -> IO ()
writeBytes path bytes = do
    createDirectoryIfMissing True (takeDirectory path)
    BS.writeFile path bytes
