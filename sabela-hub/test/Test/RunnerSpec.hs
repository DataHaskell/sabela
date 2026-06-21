{-# LANGUAGE OverloadedStrings #-}

{- | The WASM-runner splice: it inserts the runtime loader and an inert source
data island after @\<body\>@, interpolates the slug into the bootstrap, is
idempotent, leaves every byte outside the inserted block untouched, no-ops on
bodyless fragments, and HTML-escapes the source so a notebook containing
@\</script\>@ cannot break out of the island. Also covers the directory backfill
('republishRunners') on a temp shares dir.
-}
module Test.RunnerSpec (spec) where

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

import Hub.Republish (republishRunners)
import Hub.Runner (runnerMarker, spliceRunner)

page :: BS.ByteString
page = "<html><head>H</head><body>BODY-CONTENT</body></html>"

source :: T.Text
source = "# Notebook\n\n```haskell\nmain = print 42\n```\n"

markerCount :: BS.ByteString -> Int
markerCount = T.count (TE.decodeUtf8 runnerMarker) . TE.decodeUtf8

spec :: Spec
spec = do
    describe "spliceRunner" $ do
        it "inserts exactly one runner, right after <body>" $ do
            let out = spliceRunner "ab12" source page
            markerCount out `shouldBe` 1
            ("<html><head>H</head><body>" `BS.isPrefixOf` out) `shouldBe` True

        it "preserves every byte before and after the runner" $ do
            let out = spliceRunner "ab12" source page
            ("<html><head>H</head><body>" `BS.isPrefixOf` out) `shouldBe` True
            ("BODY-CONTENT</body></html>" `BS.isSuffixOf` out) `shouldBe` True

        it "embeds the runtime loader and the source data island" $ do
            let out = TE.decodeUtf8 (spliceRunner "ab12" source page)
            ("/_hub/assets/sabela-wasm-run.js" `T.isInfixOf` out) `shouldBe` True
            ("id=\"sabela-nb-source\"" `T.isInfixOf` out) `shouldBe` True
            ("application/notebook+markdown" `T.isInfixOf` out) `shouldBe` True

        it "carries the slug into the bootstrap" $ do
            let out = TE.decodeUtf8 (spliceRunner "ab12" source page)
            ("SABELA_WASM_SLUG=\"ab12\"" `T.isInfixOf` out) `shouldBe` True

        it "escapes the source so </script> cannot break out of the island" $ do
            let evil = "before </script><img src=x onerror=alert(1)> after"
                out = TE.decodeUtf8 (spliceRunner "ab12" evil page)
            -- The raw closing tag never survives into the page.
            ("</script><img" `T.isInfixOf` out) `shouldBe` False
            ("&lt;/script&gt;" `T.isInfixOf` out) `shouldBe` True
            ("onerror=alert(1)" `T.isInfixOf` out) `shouldBe` True
            -- Exactly the three runner scripts close: island + loader + bootstrap.
            T.count "</script>" out `shouldBe` 3

        it "is idempotent (re-splicing changes nothing)" $ do
            let once = spliceRunner "ab12" source page
            spliceRunner "ab12" source once `shouldBe` once

        it "is a no-op on HTML that already carries the marker" $ do
            let pre = "<body>" <> runnerMarker <> "X</body>"
            spliceRunner "ab12" source pre `shouldBe` pre

        it "is a no-op on a fragment with no <body>" $ do
            spliceRunner "ab12" source "<div>x</div>" `shouldBe` "<div>x</div>"

    describe "republishRunners" $
        it "splices each share once and is idempotent across runs" $ do
            base <- getTemporaryDirectory
            let root = base </> "sabela-republish-runners-test"
            _ <- try (removeDirectoryRecursive root) :: IO (Either SomeException ())
            mapM_ (writeShare root) ["aa11", "bb22"]
            createDirectoryIfMissing True (root </> "NOTAHEX")

            first <- republishRunners root
            map snd first `shouldBe` [True, True]
            length first `shouldBe` 2

            again <- republishRunners root
            map snd again `shouldBe` [False, False]

            out <- BS.readFile (root </> "aa11" </> "index.html")
            markerCount out `shouldBe` 1
            (source `T.isInfixOf` TE.decodeUtf8 out) `shouldBe` True
  where
    writeShare root slug = do
        createDirectoryIfMissing True (root </> slug)
        BS.writeFile (root </> slug </> "index.html") page
        BS.writeFile (root </> slug </> "source.md") (TE.encodeUtf8 source)
