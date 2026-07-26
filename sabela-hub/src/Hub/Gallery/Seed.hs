{-# LANGUAGE OverloadedStrings #-}

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

data RenderSpec
    = FromMarkdown
    | FromDashboard FilePath
    | FromAssets Text [Text]

data Curated = Curated
    { cSlug :: Text
    , cFile :: FilePath
    , cTitle :: Text
    , cAuthor :: Text
    , cTags :: [Text]
    , cRender :: RenderSpec
    }

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

lyahAuthor :: Text
lyahAuthor = "Miran Lipovaca (CC BY-NC-SA 3.0)"

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

seedGallery :: FilePath -> FilePath -> IO ()
seedGallery repoRoot dataRoot = do
    let shares = dataRoot </> "shares"
        gallery = dataRoot </> "gallery"
        allShares = curation ++ lyahChapters
    createDirectoryIfMissing True (gallery </> "collections")
    forM_ allShares (seedShare repoRoot shares)

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
