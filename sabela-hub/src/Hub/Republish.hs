{-# LANGUAGE OverloadedStrings #-}

{- | Backfill the spliced fragments into already-published share snapshots.
Walks a shares directory (@\<root\>\/\<slug\>\/index.html@) and rewrites each
snapshot: 'republishBanners' through 'spliceBanner', 'republishRunners' through
'spliceRunner' (reading the share's stored @source.md@). Both splices are
idempotent and byte-identical outside the inserted fragment, so re-running is
safe; it changes nothing on the second pass.
-}
module Hub.Republish (
    republishBanners,
    republishRunners,
) where

import Control.Monad (filterM, forM, when)
import qualified Data.ByteString as BS
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))

import Hub.Banner (spliceBanner)
import Hub.Runner (spliceRunner)
import Hub.Share (validSlug)

{- | Splice the banner into every share under @sharesDir@. Returns @(slug,
changed)@ per processed share — @changed@ is 'False' for a share that already
carried the banner. Non-slug directories and dirs without an @index.html@ are
skipped.
-}
republishBanners :: FilePath -> IO [(Text, Bool)]
republishBanners = republishWith (\_ slug -> pure (spliceBanner slug))

{- | Splice the WASM runner (+ source island) into every share under
@sharesDir@, reading each share's stored @source.md@ (empty when absent). Returns
@(slug, changed)@ per processed share; @changed@ is 'False' for a share that
already carried the runner.
-}
republishRunners :: FilePath -> IO [(Text, Bool)]
republishRunners = republishWith readSource
  where
    readSource dir slug =
        spliceRunner slug <$> readSourceMd (sourcePath dir slug)
    sourcePath dir slug = dir </> T.unpack slug </> "source.md"

{- | Shared directory walk: for each valid-slug share with an @index.html@,
build a per-share rewrite from @(sharesDir, slug)@, apply it, and write back
only when the bytes changed.
-}
republishWith ::
    (FilePath -> Text -> IO (BS.ByteString -> BS.ByteString)) ->
    FilePath ->
    IO [(Text, Bool)]
republishWith mkRewrite sharesDir = do
    names <- listDirectory sharesDir
    let slugs = filter validSlug (map T.pack names)
    haveHtml <- filterM (doesFileExist . indexPath) slugs
    fmap catMaybes . forM haveHtml $ \slug -> do
        let f = indexPath slug
        rewrite <- mkRewrite sharesDir slug
        before <- BS.readFile f
        let after = rewrite before
            changed = after /= before
        when changed $ BS.writeFile f after
        pure (Just (slug, changed))
  where
    indexPath slug = sharesDir </> T.unpack slug </> "index.html"

-- | The stored source markdown text, or empty when the share has none.
readSourceMd :: FilePath -> IO Text
readSourceMd f = do
    e <- doesFileExist f
    if e then TE.decodeUtf8Lenient <$> BS.readFile f else pure ""
