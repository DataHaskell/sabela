{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Phase 3a acceptance tests for the static share store: publish/lookup
round-trip, owner-scoped listing + deletion, slug path-traversal guard,
secret scrubbing, header hardening, reload-from-disk, titles, and the
meta-line write chokepoint.
-}
module Test.ShareSpec (spec) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hub.Meta (parseMeta, writeMetaLine)
import Hub.Share
import Hub.Shares.Api (parsePublishTitle)
import Hub.Types (ExportMode (..))
import System.Directory (
    createDirectoryIfMissing,
    getTemporaryDirectory,
    removeDirectoryRecursive,
 )
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

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
        , shareTitle = "My Notebook"
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
            publishShare store (mkShare "aa11" "alice@x") "<h1>dash</h1>" Nothing
            lookupShareHtml store "aa11" `shouldReturn` Just "<h1>dash</h1>"

        it "returns Nothing for unknown or traversal slugs" $ withStore $ \store _ -> do
            lookupShareHtml store "deadbeef" `shouldReturn` Nothing
            lookupShareHtml store "../meta" `shouldReturn` Nothing

        it "lists shares by owner" $ withStore $ \store _ -> do
            publishShare store (mkShare "aa11" "alice@x") "a" Nothing
            publishShare store (mkShare "bb22" "alice@x") "b" Nothing
            publishShare store (mkShare "cc33" "bob@x") "c" Nothing
            owned <- listShares store "alice@x"
            sort (map shareSlug owned) `shouldBe` ["aa11", "bb22"]

        it "delete is owner-checked" $ withStore $ \store _ -> do
            publishShare store (mkShare "aa11" "alice@x") "a" Nothing
            deleteShare store "bob@x" "aa11" `shouldReturn` False
            r1 <- lookupShareHtml store "aa11"
            r1 `shouldSatisfy` (/= Nothing)
            deleteShare store "alice@x" "aa11" `shouldReturn` True
            lookupShareHtml store "aa11" `shouldReturn` Nothing

        it "reloads shares from disk on a new store" $ withStore $ \store dir -> do
            publishShare store (mkShare "aa11" "alice@x") "<h1>persisted</h1>" Nothing
            store2 <- newShareStore dir
            lookupShareHtml store2 "aa11" `shouldReturn` Just "<h1>persisted</h1>"
            owned <- listShares store2 "alice@x"
            map shareSlug owned `shouldBe` ["aa11"]

    describe "titles (gallery step 1)" $ do
        it "round-trips the title through publish + reload" $
            withStore $ \store dir -> do
                publishShare
                    store
                    (mkShare "aa11" "alice@x"){shareTitle = "Iris, end to end"}
                    "x"
                    Nothing
                store2 <- newShareStore dir
                owned <- listShares store2 "alice@x"
                map shareTitle owned `shouldBe` ["Iris, end to end"]

        it "loads a legacy 3-key meta with the title defaulted, not dropped" $
            withStore $ \_ dir -> do
                createDirectoryIfMissing True (dir </> "ee55")
                BS.writeFile
                    (dir </> "ee55" </> "meta")
                    ( TE.encodeUtf8
                        "owner=old@x\nmode=dashboard\ncreatedAt=2026-01-01T00:00:00Z\n"
                    )
                store2 <- newShareStore dir
                owned <- listShares store2 "old@x"
                map shareTitle owned `shouldBe` ["Untitled"]

        it "a newline-bearing title cannot forge a meta line" $
            withStore $ \store dir -> do
                publishShare
                    store
                    (mkShare "aa11" "alice@x"){shareTitle = "evil\nowner=mallory@x"}
                    "x"
                    Nothing
                store2 <- newShareStore dir
                owned <- listShares store2 "alice@x"
                map shareOwner owned `shouldBe` ["alice@x"]
                listShares store2 "mallory@x" `shouldReturn` []

        it "sanitizeTitle strips newlines and caps the length" $ do
            sanitizeTitle "a\r\nb\nc" `shouldSatisfy` (not . T.any (`elem` ['\r', '\n']))
            T.length (sanitizeTitle (T.replicate 500 "x")) `shouldBe` 200
            T.length (sanitizeTitle (T.replicate 200 "x")) `shouldBe` 200

        it "sanitizeTitle defaults a blank title to Untitled" $ do
            sanitizeTitle "" `shouldBe` "Untitled"
            sanitizeTitle "   " `shouldBe` "Untitled"

        it "an explicitly empty stored title loads as Untitled, not empty" $
            withStore $ \store dir -> do
                publishShare
                    store
                    (mkShare "aa11" "alice@x"){shareTitle = ""}
                    "x"
                    Nothing
                store2 <- newShareStore dir
                owned <- listShares store2 "alice@x"
                map shareTitle owned `shouldBe` ["Untitled"]

    describe "listAllShares" $
        it "returns every owner's shares (the admin view)" $
            withStore $ \store _ -> do
                publishShare store (mkShare "aa11" "alice@x") "a" Nothing
                publishShare store (mkShare "cc33" "bob@x") "c" Nothing
                allShares <- listAllShares store
                sort (map shareSlug allShares) `shouldBe` ["aa11", "cc33"]

    describe "Hub.Meta (the one way a key=value line is written)" $ do
        prop "round-trips any value with newlines collapsed, never forging a key" $
            \(s :: String) ->
                let v = T.pack s
                    parsed = parseMeta (writeMetaLine "title" v)
                 in map fst parsed == ["title"]
                        && not (any (T.any (`elem` ['\r', '\n']) . snd) parsed)

        it "preserves values containing '='" $
            parseMeta (writeMetaLine "title" "a=b=c") `shouldBe` [("title", "a=b=c")]

    describe "parsePublishTitle (publish POST body → title)" $ do
        it "reads and sanitizes the title field" $
            parsePublishTitle "{\"title\":\"Iris, end to end\"}"
                `shouldBe` "Iris, end to end"
        it "defaults a missing or blank title to Untitled" $ do
            parsePublishTitle "{}" `shouldBe` "Untitled"
            parsePublishTitle "{\"title\":\"   \"}" `shouldBe` "Untitled"
            parsePublishTitle "not json" `shouldBe` "Untitled"
        it "strips a newline so a title cannot forge a meta line" $
            parsePublishTitle "{\"title\":\"x\\nowner=mallory\"}"
                `shouldSatisfy` (not . T.any (`elem` ['\r', '\n']))
