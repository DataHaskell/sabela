{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Gallery store acceptance tests: soft slug references (dangling drops from
the feed but survives in the admin view), featured/collection CRUD, coarse
member replace-all with lazy-prune, tags (replace-all, validation, soft-prune),
and reload-from-disk.
-}
module Test.GallerySpec (spec) where

import Control.Exception (SomeException, finally, try)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Hub.Gallery
import Hub.OAuth (generateRandomToken)
import Hub.Share (
    Share (..),
    ShareStore,
    deleteShare,
    newShareStore,
    publishShare,
 )
import Hub.Types (ExportMode (..))
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec

-- | Fresh share + gallery stores in per-run temp dirs, cleaned up after.
withStores :: (ShareStore -> GalleryStore -> IO a) -> IO a
withStores act = do
    base <- getTemporaryDirectory
    tok <- generateRandomToken
    let root = base </> "sabela-gallery-test-" <> T.unpack (T.take 8 tok)
    ss <- newShareStore (root </> "shares")
    gs <- newGalleryStore (root </> "gallery")
    act ss gs
        `finally` (try (removeDirectoryRecursive root) :: IO (Either SomeException ()))

mkShare :: Text -> Text -> Share
mkShare slug title =
    Share
        { shareSlug = slug
        , shareOwner = "alice@x"
        , shareMode = ExpDashboard
        , shareCreatedAt = "2026-05-27T00:00:00Z"
        , shareTitle = title
        }

publish :: ShareStore -> Text -> IO ()
publish ss slug = publishShare ss (mkShare slug ("title-" <> slug)) "<h1>x</h1>"

-- | Slugs of the top-level featured shares a feed resolves to, in order.
feedShareSlugs :: [GalleryItem] -> [Text]
feedShareSlugs items = [shareSlug s | GItemShare s _ _ <- items]

collectionCids :: [GalleryItem] -> [Text]
collectionCids items = [cvCid c | GItemCollection c <- items]

spec :: Spec
spec = describe "Hub.Gallery" $ do
    describe "validTag" $ do
        it "accepts lowercase alphanumeric + hyphen, 1..32 chars" $ do
            validTag "data-exploration" `shouldBe` True
            validTag "ghc-9-14" `shouldBe` True
        it "rejects uppercase, spaces, over-length, empty" $ do
            validTag "Data" `shouldBe` False
            validTag "a b" `shouldBe` False
            validTag (T.replicate 33 "a") `shouldBe` False
            validTag "" `shouldBe` False

    describe "featured CRUD + idempotency + overlap" $ do
        it "adds, lists in index order, and is idempotent" $
            withStores $ \ss gs -> do
                mapM_ (publish ss) ["aa", "bb"]
                addFeatured gs "aa"
                addFeatured gs "bb"
                addFeatured gs "aa" -- dupe: no second line
                items <- listGallery gs ss
                feedShareSlugs items `shouldBe` ["aa", "bb"]
        it "removeFeatured drops the index line" $ withStores $ \ss gs -> do
            publish ss "aa"
            addFeatured gs "aa"
            removeFeatured gs "aa"
            items <- listGallery gs ss
            feedShareSlugs items `shouldBe` []
        it "a slug may be featured and also a collection member (overlap)" $
            withStores $ \ss gs -> do
                mapM_ (publish ss) ["aa", "bb"]
                addFeatured gs "aa"
                cid <- createCollection gs "C" "desc"
                _ <- setCollectionMembers gs ss cid ["aa", "bb"]
                items <- listGallery gs ss
                feedShareSlugs items `shouldBe` ["aa"]
                collectionCids items `shouldBe` [cid]

    describe "dangling references (soft slugs)" $ do
        it "drops a deleted featured share from the feed but keeps the index entry" $
            withStores $ \ss gs -> do
                mapM_ (publish ss) ["aa", "bb"]
                addFeatured gs "aa"
                addFeatured gs "bb"
                _ <- deleteShare ss "alice@x" "aa"
                items <- listGallery gs ss
                feedShareSlugs items `shouldBe` ["bb"]
                dangling <- danglingFeatured gs ss
                map fst dangling `shouldBe` ["aa"]
        it "hides a collection whose members all resolved away, keeps the record" $
            withStores $ \ss gs -> do
                publish ss "aa"
                cid <- createCollection gs "C" "d"
                _ <- setCollectionMembers gs ss cid ["aa"]
                _ <- deleteShare ss "alice@x" "aa"
                items <- listGallery gs ss
                collectionCids items `shouldBe` []
                cols <- listCollections gs
                map fst cols `shouldBe` [cid]

    describe "collections" $ do
        it "create / update / delete round-trips" $ withStores $ \ss gs -> do
            cid <- createCollection gs "Title" "Desc"
            updateCollection gs cid "Title2" "Desc2" `shouldReturn` True
            Just cv <- resolveCollection gs ss cid
            (cvTitle cv, cvDescription cv) `shouldBe` ("Title2", "Desc2")
            updateCollection gs "nope" "x" "y" `shouldReturn` False
            deleteCollection gs cid
            resolveCollection gs ss cid `shouldReturn` Nothing
        it "setCollectionMembers keeps order and lazy-prunes a dangling member" $
            withStores $ \ss gs -> do
                mapM_ (publish ss) ["aa", "bb", "cc"]
                cid <- createCollection gs "C" "d"
                _ <- deleteShare ss "alice@x" "bb"
                res <- setCollectionMembers gs ss cid ["cc", "bb", "aa"]
                res `shouldBe` MembersOk
                gs2 <- reopen gs
                Just cv <- resolveCollection gs2 ss cid
                map shareSlug (cvMembers cv) `shouldBe` ["cc", "aa"]
        it "setCollectionMembers rejects a malformed slug whole-list" $
            withStores $ \ss gs -> do
                publish ss "aa"
                cid <- createCollection gs "C" "d"
                res <- setCollectionMembers gs ss cid ["aa", "BAD"]
                res `shouldBe` MembersInvalid ["BAD"]

    describe "tags" $ do
        it "replace-all round-trips and reloads from disk" $
            withStores $ \ss gs -> do
                publish ss "aa"
                addFeatured gs "aa"
                setTags gs "aa" ["ml", "data-exploration"] `shouldReturn` TagsOk
                gs2 <- reopen gs
                sort <$> getTags gs2 "aa" `shouldReturn` ["data-exploration", "ml"]
        it "rejects the whole list on any invalid tag" $ withStores $ \ss gs -> do
            publish ss "aa"
            addFeatured gs "aa"
            setTags gs "aa" ["ok", "BAD"] `shouldReturn` TagsInvalid ["BAD"]
            setTags gs "aa" (replicate 9 "t") `shouldReturn` TagsTooMany
            getTags gs "aa" `shouldReturn` []
        it "404s an id that is not featured or a collection" $
            withStores $ \_ gs ->
                setTags gs "ff" ["x"] `shouldReturn` TagsUnknownId
        it "ignores + prunes a tags line whose id left the index" $
            withStores $ \ss gs -> do
                mapM_ (publish ss) ["aa", "bb"]
                addFeatured gs "aa"
                addFeatured gs "bb"
                setTags gs "aa" ["t1"] `shouldReturn` TagsOk
                removeFeatured gs "aa" -- aa leaves the index
                -- a later write on bb prunes aa's now-orphan tag line
                setTags gs "bb" ["t2"] `shouldReturn` TagsOk
                gs2 <- reopen gs
                getTags gs2 "aa" `shouldReturn` []
        it "a dangling featured entry keeps its tags until removeFeatured" $
            withStores $ \ss gs -> do
                mapM_ (publish ss) ["aa", "bb"]
                addFeatured gs "aa"
                addFeatured gs "bb"
                setTags gs "aa" ["t1"] `shouldReturn` TagsOk
                _ <- deleteShare ss "alice@x" "aa" -- aa dangles, index line stays
                setTags gs "bb" ["t2"] `shouldReturn` TagsOk
                gs2 <- reopen gs
                getTags gs2 "aa" `shouldReturn` ["t1"]
                dangling <- danglingFeatured gs2 ss
                lookup "aa" dangling `shouldBe` Just ["t1"]

-- | Reopen the store from its on-disk dir to assert persistence.
reopen :: GalleryStore -> IO GalleryStore
reopen = newGalleryStore . galleryDir
