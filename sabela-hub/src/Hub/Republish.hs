{-# LANGUAGE OverloadedStrings #-}

{- | Backfill the fork banner into already-published share snapshots. Walks a
shares directory (@\<root\>\/\<slug\>\/index.html@) and rewrites each snapshot
through 'spliceBanner', which is idempotent and byte-identical outside the
inserted banner. Re-running is safe; it changes nothing on the second pass.
-}
module Hub.Republish (
    republishBanners,
) where

import Control.Monad (filterM, forM, when)
import qualified Data.ByteString as BS
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))

import Hub.Banner (spliceBanner)
import Hub.Share (validSlug)

{- | Splice the banner into every share under @sharesDir@. Returns @(slug,
changed)@ per processed share — @changed@ is 'False' for a share that already
carried the banner. Non-slug directories and dirs without an @index.html@ are
skipped.
-}
republishBanners :: FilePath -> IO [(Text, Bool)]
republishBanners sharesDir = do
    names <- listDirectory sharesDir
    let slugs = filter validSlug (map T.pack names)
    haveHtml <- filterM (doesFileExist . indexPath) slugs
    fmap catMaybes . forM haveHtml $ \slug -> do
        let f = indexPath slug
        before <- BS.readFile f
        let after = spliceBanner slug before
            changed = after /= before
        when changed $ BS.writeFile f after
        pure (Just (slug, changed))
  where
    indexPath slug = sharesDir </> T.unpack slug </> "index.html"
