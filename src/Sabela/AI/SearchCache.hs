{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.SearchCache (
    SearchSource (..),
    adoptLocalDb,
    freshnessRoot,
    localIndexFreshness,
    localSearchCachePaths,
    parseMetaEpoch,
    searchCacheStatus,
    searchCacheReport,
) where

import Control.Exception (SomeException, try)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Directory (
    canonicalizePath,
    doesDirectoryExist,
    doesFileExist,
    findExecutable,
    getModificationTime,
    listDirectory,
 )
import System.Environment (lookupEnv, setEnv)
import System.FilePath (takeExtension, (</>))
import Text.Read (readMaybe)

data SearchSource = SearchSource
    { sourceLabel :: Text
    , sourceDetail :: Text
    , sourceUsable :: Bool
    }
    deriving (Eq, Show)

-- | Where a repository root keeps its local index and the meta describing it.
localSearchCachePaths :: FilePath -> (FilePath, FilePath)
localSearchCachePaths root =
    ( root </> "data" </> "hoogle-local.hoo"
    , root </> "data" </> "search-cache.meta"
    )

searchCacheStatus :: [FilePath] -> IO [SearchSource]
searchCacheStatus roots = do
    bin <- fromMaybe "hoogle" <$> lookupEnv "SABELA_HOOGLE_BIN"
    found <- findExecutable bin
    mainDb <- lookupEnv "SABELA_HOOGLE_DB"
    localDb <- lookupEnv "SABELA_HOOGLE_LOCAL_DB"
    mainStatus <- dbSource "SABELA_HOOGLE_DB" hoogleDefaultNote mainDb
    localStatus <- dbSource "SABELA_HOOGLE_LOCAL_DB" localUnsetNote localDb
    freshness <- indexFreshnessFor localDb roots
    pure
        ( [ SearchSource
                "hoogle binary"
                (maybe (T.pack bin <> " NOT FOUND on PATH") T.pack found)
                (not (null found))
          , mainStatus
          , localStatus
          ]
            <> freshness
        )
  where
    hoogleDefaultNote = "unset — using hoogle's own default database"
    localUnsetNote = "unset — installed-package symbols are unindexed"

dbSource :: Text -> Text -> Maybe String -> IO SearchSource
dbSource label unsetNote mPath = case mPath of
    Nothing -> pure (SearchSource label unsetNote True)
    Just path -> do
        ok <- doesFileExist path
        pure
            ( SearchSource
                label
                (T.pack path <> if ok then "" else " MISSING")
                ok
            )

searchCacheReport :: [FilePath] -> IO [Text]
searchCacheReport roots = map render <$> searchCacheStatus roots
  where
    render s =
        "  search cache: "
            <> sourceLabel s
            <> " = "
            <> sourceDetail s
            <> if sourceUsable s then "" else "  [degraded]"

{- | Point @SABELA_HOOGLE_LOCAL_DB@ at the in-repo index when the operator has
not chosen one and the file is on disk. Returns the path it adopted, so a
caller never reports an adoption that did not happen.
-}
adoptLocalDb :: [FilePath] -> IO (Maybe FilePath)
adoptLocalDb roots = do
    chosen <- lookupEnv "SABELA_HOOGLE_LOCAL_DB"
    case chosen of
        Just p | not (null p) -> pure Nothing
        _ -> do
            found <- firstExisting [fst (localSearchCachePaths r) | r <- roots]
            mapM_ (setEnv "SABELA_HOOGLE_LOCAL_DB") found
            pure found

firstExisting :: [FilePath] -> IO (Maybe FilePath)
firstExisting [] = pure Nothing
firstExisting (p : ps) = do
    ok <- doesFileExist p
    if ok then pure (Just p) else firstExisting ps

{- | Whether the index still describes the packages it indexed, named so the
claim is attributable. Nothing when either side is unknown, because an unknown
age is not a freshness claim.
-}
localIndexFreshness ::
    FilePath -> Maybe Integer -> Maybe Integer -> Maybe SearchSource
localIndexFreshness db (Just generated) (Just newestSource) =
    Just (SearchSource "local index age" detail fresh)
  where
    fresh = newestSource <= generated
    detail =
        T.pack db
            <> " generated at epoch "
            <> T.pack (show generated)
            <> "; newest in-repo package epoch "
            <> T.pack (show newestSource)
            <> if fresh then "" else " — rebuild with `make search-cache-local`"
localIndexFreshness _ _ _ = Nothing

{- | The root whose own index is the database in use. Nothing when no local
database is in use, or when the one in use is nobody's index here: a meta
beside a database nothing queries describes nothing the caller asked about.
-}
freshnessRoot :: Maybe FilePath -> [(FilePath, FilePath)] -> Maybe FilePath
freshnessRoot Nothing _ = Nothing
freshnessRoot (Just db) rootIndexes =
    listToMaybe [root | (root, index) <- rootIndexes, index == db]

-- | The epoch the generator recorded, or nothing when it recorded none.
parseMetaEpoch :: Text -> Maybe Integer
parseMetaEpoch =
    listToMaybe
        . mapMaybe (readMaybe . T.unpack . T.strip)
        . mapMaybe (T.stripPrefix "generated_epoch=" . T.strip)
        . T.lines

{- | The age of the local index that is actually being queried, read from the
meta beside it. Silent about every other index.
-}
indexFreshnessFor :: Maybe FilePath -> [FilePath] -> IO [SearchSource]
indexFreshnessFor mDb roots = do
    db <- mapM resolvePath mDb
    pairs <-
        mapM
            (\r -> (,) r <$> resolvePath (fst (localSearchCachePaths r)))
            roots
    case freshnessRoot db pairs of
        Nothing -> pure []
        Just root -> do
            let (indexPath, metaPath) = localSearchCachePaths root
            generated <- (parseMetaEpoch =<<) <$> readTextMaybe metaPath
            newest <- newestPackageEpoch root
            pure (maybe [] pure (localIndexFreshness indexPath generated newest))

-- | A path in the form the filesystem agrees on, or as given when it cannot say.
resolvePath :: FilePath -> IO FilePath
resolvePath p = do
    r <- try (canonicalizePath p) :: IO (Either SomeException FilePath)
    pure (either (const p) id r)

readTextMaybe :: FilePath -> IO (Maybe Text)
readTextMaybe p = do
    r <- try (TIO.readFile p) :: IO (Either SomeException Text)
    pure (either (const Nothing) Just r)

{- | When the packages this repository defines last changed, read from the
cabal files at its root and one directory down.
-}
newestPackageEpoch :: FilePath -> IO (Maybe Integer)
newestPackageEpoch root = do
    top <- entriesOf root
    nested <-
        mapM (\d -> map ((root </> d) </>) <$> entriesOf (root </> d)) top
    epochs <-
        mapM modifiedEpoch (filter isCabal (map (root </>) top <> concat nested))
    pure (maximumMaybe (catMaybes epochs))
  where
    isCabal = (== ".cabal") . takeExtension
    maximumMaybe [] = Nothing
    maximumMaybe xs = Just (maximum xs)

entriesOf :: FilePath -> IO [FilePath]
entriesOf dir = do
    ok <- doesDirectoryExist dir
    if not ok
        then pure []
        else do
            r <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
            pure (either (const []) id r)

modifiedEpoch :: FilePath -> IO (Maybe Integer)
modifiedEpoch p = do
    r <- try (getModificationTime p) :: IO (Either SomeException UTCTime)
    pure (either (const Nothing) (Just . floor . utcTimeToPOSIXSeconds) r)
