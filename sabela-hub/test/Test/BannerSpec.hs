{-# LANGUAGE OverloadedStrings #-}

{- | The fork-banner splice: it inserts exactly one banner after @\<body\>@,
interpolates the slug into the fork action, is idempotent, leaves every byte
outside the inserted banner untouched, and no-ops on bodyless fragments. Also
covers the directory backfill ('republishBanners') on a temp shares dir.
-}
module Test.BannerSpec (spec) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (
    createDirectoryIfMissing,
    getTemporaryDirectory,
    removeDirectoryRecursive,
 )
import System.FilePath ((</>))
import Test.Hspec

import Hub.Banner (bannerMarker, spliceBanner)
import Hub.Republish (republishBanners)

page :: BS.ByteString
page = "<html><head>H</head><body>BODY-CONTENT</body></html>"

markerCount :: BS.ByteString -> Int
markerCount = T.count (TE.decodeUtf8 bannerMarker) . TE.decodeUtf8

spec :: Spec
spec = do
    describe "spliceBanner" $ do
        it "inserts exactly one banner, right after <body>" $ do
            let out = spliceBanner "ab12" page
            markerCount out `shouldBe` 1
            ("<html><head>H</head><body>" `BS.isPrefixOf` out) `shouldBe` True

        it "preserves every byte before and after the banner" $ do
            let out = spliceBanner "ab12" page
            ("<html><head>H</head><body>" `BS.isPrefixOf` out) `shouldBe` True
            ("BODY-CONTENT</body></html>" `BS.isSuffixOf` out) `shouldBe` True

        it "interpolates the slug into the fork action" $ do
            let out = TE.decodeUtf8 (spliceBanner "ab12" page)
            ("/_hub/fork/ab12" `T.isInfixOf` out) `shouldBe` True

        it "is idempotent (re-splicing changes nothing)" $ do
            let once = spliceBanner "ab12" page
            spliceBanner "ab12" once `shouldBe` once

        it "is a no-op on HTML that already carries the marker" $ do
            let pre = "<body>" <> bannerMarker <> "X</body>"
            spliceBanner "ab12" pre `shouldBe` pre

        it "is a no-op on a fragment with no <body>" $ do
            spliceBanner "ab12" "<div>x</div>" `shouldBe` "<div>x</div>"

    describe "republishBanners" $
        it "splices each share once and is idempotent across runs" $ do
            base <- getTemporaryDirectory
            let root = base </> "sabela-republish-test"
            _ <- try (removeDirectoryRecursive root) :: IO (Either SomeException ())
            mapM_ (writeShare root) ["aa11", "bb22"]
            -- legacy/non-share dirs are skipped
            createDirectoryIfMissing True (root </> "NOTAHEX")

            first <- republishBanners root
            map snd first `shouldBe` [True, True]
            length first `shouldBe` 2

            again <- republishBanners root
            map snd again `shouldBe` [False, False]

            out <- BS.readFile (root </> "aa11" </> "index.html")
            markerCount out `shouldBe` 1
  where
    writeShare root slug = do
        createDirectoryIfMissing True (root </> slug)
        BS.writeFile (root </> slug </> "index.html") page
