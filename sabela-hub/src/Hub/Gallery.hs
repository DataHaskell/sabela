{-# LANGUAGE OverloadedStrings #-}

module Hub.Gallery (
    GalleryStore,
    GalleryItem (..),
    CollectionView (..),
    SetMembersResult (..),
    SetTagsResult (..),
    newGalleryStore,
    galleryDir,
    addFeatured,
    removeFeatured,
    createCollection,
    updateCollection,
    deleteCollection,
    setCollectionMembers,
    listCollections,
    resolveCollection,
    setTags,
    getTags,
    getAttribution,
    setAttribution,
    listGallery,
    danglingFeatured,
    featuredSlugs,
    collectionMembership,
    publicSlugs,
    setIndexOrder,
    validTag,
) where

import Control.Concurrent.STM
import Control.Monad (forM)
import Data.Bifunctor (second)
import Data.Char (isAsciiLower, isDigit)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((</>))

import Hub.Gallery.Store
import Hub.OAuth (generateRandomToken)
import Hub.Share (ShareStore, validSlug)

addFeatured :: GalleryStore -> Text -> IO ()
addFeatured gs slug = withWrite gs $ do
    ix <- readTVarIO (gsIndex gs)
    if IxShare slug `elem` ix
        then pure ()
        else do
            atomically $ writeTVar (gsIndex gs) (ix ++ [IxShare slug])
            persistIndex gs

removeFeatured :: GalleryStore -> Text -> IO ()
removeFeatured gs slug = withWrite gs $ do
    atomically $ modifyTVar' (gsIndex gs) (filter (/= IxShare slug))
    persistIndex gs
    persistTags gs

createCollection :: GalleryStore -> Text -> Text -> IO Text
createCollection gs title desc = withWrite gs $ do
    cid <- generateRandomToken
    now <- T.pack . iso8601Show <$> getCurrentTime
    let col = Collection title desc now []
    atomically $ do
        modifyTVar' (gsCols gs) (Map.insert cid col)
        modifyTVar' (gsIndex gs) (++ [IxCollection cid])
    persistCollection gs cid col
    persistIndex gs
    pure cid

updateCollection :: GalleryStore -> Text -> Text -> Text -> IO Bool
updateCollection gs cid title desc = withWrite gs $ do
    cols <- readTVarIO (gsCols gs)
    case Map.lookup cid cols of
        Nothing -> pure False
        Just col -> do
            let col' = col{colTitle = title, colDescription = desc}
            atomically $ modifyTVar' (gsCols gs) (Map.insert cid col')
            persistCollection gs cid col'
            pure True

deleteCollection :: GalleryStore -> Text -> IO ()
deleteCollection gs cid = withWrite gs $ do
    atomically $ do
        modifyTVar' (gsCols gs) (Map.delete cid)
        modifyTVar' (gsIndex gs) (filter (/= IxCollection cid))
    persistIndex gs
    persistTags gs
    removeDirectoryRecursive (galleryDir gs </> "collections" </> T.unpack cid)

setCollectionMembers ::
    GalleryStore -> ShareStore -> Text -> [Text] -> IO SetMembersResult
setCollectionMembers gs ss cid members =
    case filter (not . validSlug) members of
        bad@(_ : _) -> pure (MembersInvalid bad)
        [] -> withWrite gs $ do
            cols <- readTVarIO (gsCols gs)
            case Map.lookup cid cols of
                Nothing -> pure MembersUnknownCollection
                Just col -> do
                    live <- shareSet ss
                    let kept = filter (`Map.member` live) members
                        col' = col{colMembers = kept}
                    atomically $ modifyTVar' (gsCols gs) (Map.insert cid col')
                    persistCollection gs cid col'
                    pure MembersOk

listCollections :: GalleryStore -> IO [(Text, Collection)]
listCollections gs = Map.toList <$> readTVarIO (gsCols gs)

featuredSlugs :: GalleryStore -> IO [Text]
featuredSlugs gs = (\ix -> [s | IxShare s <- ix]) <$> readTVarIO (gsIndex gs)

collectionMembership :: GalleryStore -> IO [(Text, [Text])]
collectionMembership gs = map (second colMembers) <$> listCollections gs

publicSlugs :: GalleryStore -> IO [Text]
publicSlugs gs = do
    feat <- featuredSlugs gs
    mem <- collectionMembership gs
    pure (feat ++ concatMap snd mem)

setIndexOrder :: GalleryStore -> [(Text, Text)] -> IO ()
setIndexOrder gs desired = withWrite gs $ do
    cur <- readTVarIO (gsIndex gs)
    let want = mapMaybe toEntry desired
        kept = [e | e <- want, e `elem` cur]
        rest = [e | e <- cur, e `notElem` kept]
    atomically $ writeTVar (gsIndex gs) (kept ++ rest)
    persistIndex gs
  where
    toEntry ("share", i) = Just (IxShare i)
    toEntry ("collection", i) = Just (IxCollection i)
    toEntry _ = Nothing

resolveCollection ::
    GalleryStore -> ShareStore -> Text -> IO (Maybe CollectionView)
resolveCollection gs ss cid = do
    cols <- readTVarIO (gsCols gs)
    case Map.lookup cid cols of
        Nothing -> pure Nothing
        Just col -> do
            live <- shareSet ss
            tags <- getTags gs cid
            pure $
                Just
                    CollectionView
                        { cvCid = cid
                        , cvTitle = colTitle col
                        , cvDescription = colDescription col
                        , cvMembers = mapMaybe (`Map.lookup` live) (colMembers col)
                        , cvTags = tags
                        }

validTag :: Text -> Bool
validTag t =
    let n = T.length t
     in n >= 1 && n <= 32 && T.all ok t
  where
    ok c = isDigit c || isAsciiLower c || c == '-'

setTags :: GalleryStore -> Text -> [Text] -> IO SetTagsResult
setTags gs tid tags
    | length tags > 8 = pure TagsTooMany
    | bad@(_ : _) <- filter (not . validTag) tags = pure (TagsInvalid bad)
    | otherwise = withWrite gs $ do
        ix <- readTVarIO (gsIndex gs)
        if tid `notElem` indexIds ix
            then pure TagsUnknownId
            else do
                atomically $ modifyTVar' (gsTags gs) (Map.insert tid tags)
                persistTags gs
                pure TagsOk

getTags :: GalleryStore -> Text -> IO [Text]
getTags gs tid = Map.findWithDefault [] tid <$> readTVarIO (gsTags gs)

getAttribution :: GalleryStore -> Text -> IO (Maybe Text)
getAttribution gs slug = Map.lookup slug <$> readTVarIO (gsAttr gs)

setAttribution :: GalleryStore -> Text -> Text -> IO ()
setAttribution gs slug name = withWrite gs $ do
    atomically $
        modifyTVar' (gsAttr gs) $
            if T.null name then Map.delete slug else Map.insert slug name
    persistAttr gs

listGallery :: GalleryStore -> ShareStore -> IO [GalleryItem]
listGallery gs ss = do
    ix <- readTVarIO (gsIndex gs)
    live <- shareSet ss
    catMaybes <$> mapM (resolve live) ix
  where
    resolve live (IxShare slug) =
        case Map.lookup slug live of
            Nothing -> pure Nothing
            Just s ->
                Just <$> (GItemShare s <$> getTags gs slug <*> getAttribution gs slug)
    resolve _ (IxCollection cid) = do
        mcv <- resolveCollection gs ss cid
        pure $ case mcv of
            Just cv | not (null (cvMembers cv)) -> Just (GItemCollection cv)
            _ -> Nothing

danglingFeatured :: GalleryStore -> ShareStore -> IO [(Text, [Text])]
danglingFeatured gs ss = do
    ix <- readTVarIO (gsIndex gs)
    live <- shareSet ss
    forM [slug | IxShare slug <- ix, not (Map.member slug live)] $ \slug ->
        (,) slug <$> getTags gs slug
