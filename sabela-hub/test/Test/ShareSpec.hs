{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Phase 3a acceptance tests for the static share store: publish/lookup
round-trip, owner-scoped listing + deletion, slug path-traversal guard,
secret scrubbing, header hardening, and reload-from-disk.
-}
module Test.ShareSpec (spec) where

import Control.Exception (SomeException, try)
import Data.List (sort)
import Data.Text (Text)
import Hub.Share
import Hub.Types (ExportMode (..))
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec

-- | Run with a fresh store in a wiped temp dir.
withStore :: (ShareStore -> FilePath -> IO a) -> IO a
withStore act = do
    base <- getTemporaryDirectory
    let dir = base </> "sabela-share-test"
    _ <- try (removeDirectoryRecursive dir) :: IO (Either SomeException ())
    store <- newShareStore dir
    act store dir

mkShare :: Text -> Text -> Share
mkShare slug owner =
    Share
        { shareSlug = slug
        , shareOwner = owner
        , shareMode = ExpDashboard
        , shareCreatedAt = "2026-05-27T00:00:00Z"
        }

spec :: Spec
spec = describe "Hub.Share" $ do
    describe "validSlug" $ do
        it "accepts lowercase hex" $ validSlug "abc123def" `shouldBe` True
        it "rejects empty" $ validSlug "" `shouldBe` False
        it "rejects traversal, slashes, uppercase" $ do
            validSlug "../etc" `shouldBe` False
            validSlug "a/b" `shouldBe` False
            validSlug "ABC" `shouldBe` False

    describe "scrubSecrets" $ do
        it "passes clean html" $ scrubSecrets "<h1>hi</h1>" `shouldBe` Nothing
        it "flags an Anthropic key" $
            scrubSecrets "x sk-ant-abc y" `shouldSatisfy` (/= Nothing)
        it "flags a Google secret" $
            scrubSecrets "GOCSPX-zzz" `shouldSatisfy` (/= Nothing)
        it "flags an AWS key" $
            scrubSecrets "AKIAEXAMPLE" `shouldSatisfy` (/= Nothing)
        it "flags a GitHub token" $
            scrubSecrets "ghp_abc123def" `shouldSatisfy` (/= Nothing)
        it "flags a private key block" $
            scrubSecrets "-----BEGIN OPENSSH PRIVATE KEY-----"
                `shouldSatisfy` (/= Nothing)

    describe "shareHeaders" $
        it "include nosniff and frame-ancestors" $ do
            lookup "X-Content-Type-Options" shareHeaders `shouldBe` Just "nosniff"
            lookup "Content-Security-Policy" shareHeaders
                `shouldBe` Just "frame-ancestors 'self'"

    describe "publish / lookup / list / delete" $ do
        it "round-trips the snapshot html" $ withStore $ \store _ -> do
            publishShare store (mkShare "aa11" "alice@x") "<h1>dash</h1>"
            lookupShareHtml store "aa11" `shouldReturn` Just "<h1>dash</h1>"

        it "returns Nothing for unknown or traversal slugs" $ withStore $ \store _ -> do
            lookupShareHtml store "deadbeef" `shouldReturn` Nothing
            lookupShareHtml store "../meta" `shouldReturn` Nothing

        it "lists shares by owner" $ withStore $ \store _ -> do
            publishShare store (mkShare "aa11" "alice@x") "a"
            publishShare store (mkShare "bb22" "alice@x") "b"
            publishShare store (mkShare "cc33" "bob@x") "c"
            owned <- listShares store "alice@x"
            sort (map shareSlug owned) `shouldBe` ["aa11", "bb22"]

        it "delete is owner-checked" $ withStore $ \store _ -> do
            publishShare store (mkShare "aa11" "alice@x") "a"
            deleteShare store "bob@x" "aa11" `shouldReturn` False
            r1 <- lookupShareHtml store "aa11"
            r1 `shouldSatisfy` (/= Nothing)
            deleteShare store "alice@x" "aa11" `shouldReturn` True
            lookupShareHtml store "aa11" `shouldReturn` Nothing

        it "reloads shares from disk on a new store" $ withStore $ \store dir -> do
            publishShare store (mkShare "aa11" "alice@x") "<h1>persisted</h1>"
            store2 <- newShareStore dir
            lookupShareHtml store2 "aa11" `shouldReturn` Just "<h1>persisted</h1>"
            owned <- listShares store2 "alice@x"
            map shareSlug owned `shouldBe` ["aa11"]
