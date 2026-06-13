{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | The gallery store's data types and disk layer: the @index@ /
@collections/<cid>/meta@ / @tags@ files, hydrate, and the persist helpers. The
operations on top live in "Hub.Gallery". All writes are serialized through one
'MVar' lock because the @index@ and @tags@ files are shared.
-}
module Hub.Gallery.Store (
    GalleryStore (..),
    IndexEntry (..),
    Collection (..),
    GalleryItem (..),
    CollectionView (..),
    SetMembersResult (..),
    SetTagsResult (..),
    newGalleryStore,
    galleryDir,
    indexIds,
    withWrite,
    shareSet,
    persistIndex,
    persistCollection,
    persistTags,
    persistAttr,
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Monad (forM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    listDirectory,
 )
import System.FilePath ((</>))

import Hub.Meta (parseMeta, writeMetaLine)
import Hub.Share (Share (..), ShareStore, listAllShares, validSlug)
import Hub.Types (isLowerHex)

-- | A top-level @index@ entry: a featured share slug, or a collection cid.
data IndexEntry = IxShare Text | IxCollection Text
    deriving (Eq)

data Collection = Collection
    { colTitle :: Text
    , colDescription :: Text
    , colCreatedAt :: Text
    , colMembers :: [Text]
    }

{- | A resolved feed entry: a featured share with its tags and an optional
author byline (curation metadata), or a collection.
-}
data GalleryItem
    = GItemShare Share [Text] (Maybe Text)
    | GItemCollection CollectionView
    deriving (Eq, Show)

-- | A collection with its members resolved to live shares (drop order kept).
data CollectionView = CollectionView
    { cvCid :: Text
    , cvTitle :: Text
    , cvDescription :: Text
    , cvMembers :: [Share]
    , cvTags :: [Text]
    }
    deriving (Eq, Show)

data SetMembersResult
    = MembersOk
    | MembersInvalid [Text]
    | MembersUnknownCollection
    deriving (Eq, Show)

data SetTagsResult
    = TagsOk
    | TagsInvalid [Text]
    | TagsTooMany
    | TagsUnknownId
    deriving (Eq, Show)

data GalleryStore = GalleryStore
    { gsDir :: FilePath
    , gsIndex :: TVar [IndexEntry]
    , gsCols :: TVar (Map Text Collection)
    , gsTags :: TVar (Map Text [Text])
    , gsAttr :: TVar (Map Text Text)
    , gsLock :: MVar ()
    }

galleryDir :: GalleryStore -> FilePath
galleryDir = gsDir

-- ---------------------------------------------------------------------------
-- Hydrate
-- ---------------------------------------------------------------------------

newGalleryStore :: FilePath -> IO GalleryStore
newGalleryStore dir = do
    createDirectoryIfMissing True (dir </> "collections")
    ix <- loadIndex dir
    cols <- loadCollections dir
    let ids = indexIds ix
    tags <- Map.filterWithKey (\k _ -> k `elem` ids) <$> loadTags dir
    attr <- loadAttr dir
    GalleryStore dir
        <$> newTVarIO ix
        <*> newTVarIO cols
        <*> newTVarIO tags
        <*> newTVarIO attr
        <*> newMVar ()

loadIndex :: FilePath -> IO [IndexEntry]
loadIndex dir = do
    let f = dir </> "index"
    e <- doesFileExist f
    if not e
        then pure []
        else mapMaybe toEntry . parseMeta <$> TIO.readFile f
  where
    toEntry ("share", v) = Just (IxShare v)
    toEntry ("collection", v) = Just (IxCollection v)
    toEntry _ = Nothing

loadCollections :: FilePath -> IO (Map Text Collection)
loadCollections dir = do
    let cdir = dir </> "collections"
    cids <- filter isLowerHex . map T.pack <$> listDirectory cdir
    pairs <- forM cids $ \cid ->
        fmap (cid,) <$> loadCollection cdir cid
    pure (Map.fromList (catMaybes pairs))

loadCollection :: FilePath -> Text -> IO (Maybe Collection)
loadCollection cdir cid = do
    let f = cdir </> T.unpack cid </> "meta"
    e <- doesFileExist f
    if not e
        then pure Nothing
        else do
            kv <- parseMeta <$> TIO.readFile f
            let get k = lookup k kv
                members = maybe [] splitMembers (get "members")
            pure $
                Collection
                    <$> get "title"
                    <*> get "description"
                    <*> get "createdAt"
                    <*> pure members

loadTags :: FilePath -> IO (Map Text [Text])
loadTags dir = do
    let f = dir </> "tags"
    e <- doesFileExist f
    if not e
        then pure Map.empty
        else Map.fromList . map (fmap splitTags) . parseMeta <$> TIO.readFile f

-- | Per-slug author byline (curation metadata, like tags): @<id>=Name@.
loadAttr :: FilePath -> IO (Map Text Text)
loadAttr dir = do
    let f = dir </> "attribution"
    e <- doesFileExist f
    if not e
        then pure Map.empty
        else Map.fromList . parseMeta <$> TIO.readFile f

splitMembers :: Text -> [Text]
splitMembers = filter validSlug . T.splitOn ","

splitTags :: Text -> [Text]
splitTags = filter (not . T.null) . T.splitOn ","

-- ---------------------------------------------------------------------------
-- Persist + shared helpers
-- ---------------------------------------------------------------------------

withWrite :: GalleryStore -> IO a -> IO a
withWrite gs act = withMVar (gsLock gs) (const act)

shareSet :: ShareStore -> IO (Map Text Share)
shareSet ss = Map.fromList . map (\s -> (shareSlug s, s)) <$> listAllShares ss

indexIds :: [IndexEntry] -> [Text]
indexIds ix = [s | IxShare s <- ix] ++ [c | IxCollection c <- ix]

persistIndex :: GalleryStore -> IO ()
persistIndex gs = do
    ix <- readTVarIO (gsIndex gs)
    TIO.writeFile (gsDir gs </> "index") (T.unlines (map entryLine ix))
  where
    entryLine (IxShare s) = writeMetaLine "share" s
    entryLine (IxCollection c) = writeMetaLine "collection" c

persistCollection :: GalleryStore -> Text -> Collection -> IO ()
persistCollection gs cid col = do
    let d = gsDir gs </> "collections" </> T.unpack cid
    createDirectoryIfMissing True d
    TIO.writeFile (d </> "meta") $
        T.unlines
            [ writeMetaLine "title" (colTitle col)
            , writeMetaLine "description" (colDescription col)
            , writeMetaLine "createdAt" (colCreatedAt col)
            , writeMetaLine "members" (T.intercalate "," (colMembers col))
            ]

{- | Persist the tags file, dropping any id no longer in the index (soft
prune) and keeping the in-memory map index-consistent.
-}
persistTags :: GalleryStore -> IO ()
persistTags gs = do
    ix <- readTVarIO (gsIndex gs)
    let ids = indexIds ix
    tags <- atomically $ do
        modifyTVar' (gsTags gs) (Map.filterWithKey (\k _ -> k `elem` ids))
        readTVar (gsTags gs)
    TIO.writeFile (gsDir gs </> "tags") $
        T.unlines
            [ writeMetaLine k (T.intercalate "," v)
            | (k, v) <- Map.toList tags
            , not (null v)
            ]

-- | Persist the per-slug author bylines.
persistAttr :: GalleryStore -> IO ()
persistAttr gs = do
    attr <- readTVarIO (gsAttr gs)
    TIO.writeFile (gsDir gs </> "attribution") $
        T.unlines [writeMetaLine k v | (k, v) <- Map.toList attr, not (T.null v)]
