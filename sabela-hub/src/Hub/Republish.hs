{-# LANGUAGE OverloadedStrings #-}

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

republishBanners :: FilePath -> IO [(Text, Bool)]
republishBanners = republishWith (\_ slug -> pure (spliceBanner slug))

republishRunners :: FilePath -> IO [(Text, Bool)]
republishRunners = republishWith readSource
  where
    readSource dir slug =
        spliceRunner slug <$> readSourceMd (sourcePath dir slug)
    sourcePath dir slug = dir </> T.unpack slug </> "source.md"

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

readSourceMd :: FilePath -> IO Text
readSourceMd f = do
    e <- doesFileExist f
    if e then TE.decodeUtf8Lenient <$> BS.readFile f else pure ""
