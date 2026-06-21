{-# LANGUAGE OverloadedStrings #-}

{- | Pure render tests for the public gallery pages and feed: HTML escaping,
admin-defined feed order, the opaque-origin sandboxed reader, tag chips +
filtering (with no reflection of an unknown tag), og tags, the footer CTA, and
the RSS/sitemap shapes. Substring assertions mirror the @shareHeaders@ style.
-}
module Test.GalleryRenderSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Hub.Gallery (CollectionView (..), GalleryItem (..))
import Hub.Gallery.Feed (renderFeed, renderSitemap)
import Hub.Gallery.Render (
    GalleryChrome (..),
    htmlEscape,
    renderCollectionReader,
    renderGallery,
 )
import Hub.Share (Share (..))
import Hub.Types (ExportMode (..))
import Test.Hspec

chrome :: GalleryChrome
chrome =
    GalleryChrome
        { gcOrigin = "https://hub.example.com"
        , gcContact = Just "admin@example.com"
        , gcRepoUrl = "https://github.com/DataHaskell/sabela"
        }

mkShare :: Text -> Text -> Share
mkShare slug title =
    Share
        { shareSlug = slug
        , shareOwner = "alice@x"
        , shareMode = ExpDashboard
        , shareCreatedAt = "2026-05-27T00:00:00Z"
        , shareTitle = title
        }

shareItem :: Text -> Text -> [Text] -> GalleryItem
shareItem slug title tags = GItemShare (mkShare slug title) tags Nothing

-- | @x@ appears before @y@ in @hay@ (both must be present).
occursBefore :: Text -> Text -> Text -> Bool
occursBefore x y hay =
    case (T.breakOn x hay, T.breakOn y hay) of
        ((bx, mx), (by, my))
            | not (T.null mx) && not (T.null my) -> T.length bx < T.length by
        _ -> False

spec :: Spec
spec = describe "Hub.Gallery.Render / Feed" $ do
    describe "htmlEscape" $
        it "escapes & < > \"" $
            htmlEscape "<a href=\"x\">&" `shouldBe` "&lt;a href=&quot;x&quot;&gt;&amp;"

    describe "renderGallery" $ do
        it "escapes a <script> title and never emits a raw tag" $ do
            let out = renderGallery chrome Nothing [shareItem "aa" "<script>evil</script>" []]
            out `shouldSatisfy` T.isInfixOf "&lt;script&gt;"
            out `shouldSatisfy` (not . T.isInfixOf "<script>")

        it "renders cards in the admin-defined index order, not by date" $ do
            let items = [shareItem "aa" "Zeta" [], shareItem "bb" "Alpha" []]
                out = renderGallery chrome Nothing items
            occursBefore "Zeta" "Alpha" out `shouldBe` True

        it "links each share card to its /s/<slug>" $ do
            let out = renderGallery chrome Nothing [shareItem "abc123" "T" []]
            out `shouldSatisfy` T.isInfixOf "/s/abc123"

        it "emits og:title and a static og:image" $ do
            let out = renderGallery chrome Nothing [shareItem "aa" "Hi" []]
            out `shouldSatisfy` T.isInfixOf "og:title"
            out `shouldSatisfy` T.isInfixOf "og:image"

        it "renders an author byline + Download and Fork actions on a share" $ do
            let out =
                    renderGallery
                        chrome
                        Nothing
                        [GItemShare (mkShare "aa" "CSG") [] (Just "Joe Warren")]
            out `shouldSatisfy` T.isInfixOf "by Joe Warren"
            out `shouldSatisfy` T.isInfixOf "/_hub/source/aa"
            out `shouldSatisfy` T.isInfixOf "/_hub/fork/aa"

        it "shows the contact mailto CTA only when contact is set" $ do
            let withC = renderGallery chrome Nothing [shareItem "aa" "T" []]
                noC = renderGallery chrome{gcContact = Nothing} Nothing [shareItem "aa" "T" []]
            withC `shouldSatisfy` T.isInfixOf "mailto:admin@example.com"
            noC `shouldSatisfy` (not . T.isInfixOf "mailto:")
            noC `shouldSatisfy` T.isInfixOf "github.com/DataHaskell/sabela"

    describe "tag chips + filter (no reflection)" $ do
        let items =
                [ shareItem "aa" "A" ["ml", "viz"]
                , shareItem "bb" "B" ["ml"]
                ]
        it "renders a chip row from the known tag set, alphabetised" $ do
            let out = renderGallery chrome Nothing items
            out `shouldSatisfy` T.isInfixOf "/gallery?tag=ml"
            out `shouldSatisfy` T.isInfixOf "/gallery?tag=viz"
            occursBefore "tag=ml" "tag=viz" out `shouldBe` True

        it "an active filter shows only matching cards" $ do
            let out = renderGallery chrome (Just "viz") items
            out `shouldSatisfy` T.isInfixOf ">A<"
            out `shouldSatisfy` (not . T.isInfixOf ">B<")

        it "an unknown ?tag= value renders the full feed and is never echoed" $ do
            let out = renderGallery chrome (Just "zzz-not-real") items
            out `shouldSatisfy` T.isInfixOf ">A<"
            out `shouldSatisfy` T.isInfixOf ">B<"
            out `shouldSatisfy` (not . T.isInfixOf "zzz-not-real")

    describe "renderCollectionReader" $ do
        let cv =
                CollectionView
                    { cvCid = "c0ffee"
                    , cvTitle = "Tour"
                    , cvDescription = "d"
                    , cvMembers = [mkShare "aa" "One", mkShare "bb" "Two"]
                    , cvTags = []
                    }
        it "frames the member as an opaque-origin sandbox (no allow-same-origin)" $ do
            let out = renderCollectionReader chrome cv 0
            out
                `shouldSatisfy` T.isInfixOf
                    "sandbox=\"allow-scripts allow-popups allow-forms\""
            out `shouldSatisfy` (not . T.isInfixOf "allow-same-origin")
            out `shouldSatisfy` T.isInfixOf "src=\"/s/aa\""

        it "offers a next link but not a prev link on the first member" $ do
            let out = renderCollectionReader chrome cv 0
            out `shouldSatisfy` T.isInfixOf "/c/c0ffee/1"
            out `shouldSatisfy` (not . T.isInfixOf "/c/c0ffee/-1")

    describe "feed + sitemap" $ do
        let items =
                [ shareItem "aa" "First" []
                , GItemCollection
                    CollectionView
                        { cvCid = "c0ffee"
                        , cvTitle = "Second"
                        , cvDescription = "d"
                        , cvMembers = [mkShare "bb" "M"]
                        , cvTags = []
                        }
                ]
        it "renders feed entries in index order with absolute links" $ do
            let out = renderFeed chrome items
            occursBefore "First" "Second" out `shouldBe` True
            out `shouldSatisfy` T.isInfixOf "https://hub.example.com/s/aa"
            out `shouldSatisfy` T.isInfixOf "https://hub.example.com/c/c0ffee"

        it "escapes a <script> title in the feed" $ do
            let out = renderFeed chrome [shareItem "aa" "<script>x</script>" []]
            out `shouldSatisfy` T.isInfixOf "&lt;script&gt;"
            out `shouldSatisfy` (not . T.isInfixOf "<script>")

        it "sitemap lists the gallery and each collection URL" $ do
            let out = renderSitemap chrome items
            out `shouldSatisfy` T.isInfixOf "https://hub.example.com/gallery"
            out `shouldSatisfy` T.isInfixOf "https://hub.example.com/c/c0ffee"
