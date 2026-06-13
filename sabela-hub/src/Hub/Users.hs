{-# LANGUAGE OverloadedStrings #-}

{- | Persisted user roles (admin or not), mirroring the 'Hub.Share' store
shape: one dir per user at @<usersDir>\/<emailHash>\/meta@ plus a 'TVar'
cache keyed by normalized email. Per-user dirs need no write lock.
-}
module Hub.Users (
    UserRecord (..),
    UserStore,
    newUserStore,
    isAdmin,
    isAnyAdmin,
    grantAdmin,
    revokeAdmin,

    -- * Pure helpers (exported for testing)
    emailHash,
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Monad (forM, forM_)
import qualified Data.ByteString as BS
import Data.Char (intToDigit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    listDirectory,
 )
import System.FilePath ((</>))

import Hub.Meta (parseMeta, writeMetaLine)
import Hub.Types (isLowerHex, normalizeEmail)

data UserRecord = UserRecord
    { urEmail :: Text
    , urIsAdmin :: Bool
    , urCreatedAt :: Text
    }
    deriving (Eq, Show)

{- | Per-user dirs keyed by a deterministic 'emailHash', so two writers CAN
target the same dir; 'usLock' serializes every disk+cache mutation so a
grant racing a revoke can't leave the two out of sync (cf. 'Hub.Share', whose
fresh-slug-per-write makes that safe without a lock).
-}
data UserStore = UserStore
    { usBaseDir :: FilePath
    , usCache :: TVar (Map Text UserRecord)
    , usLock :: MVar ()
    }

{- | Open the store, hydrate from disk, and ensure the bootstrap admin (if
given and non-blank) holds the role — without it and with no admin on disk,
the admin surface is unreachable (granting requires being admin).
-}
newUserStore :: FilePath -> Maybe Text -> IO UserStore
newUserStore dir bootstrap = do
    createDirectoryIfMissing True dir
    entries <- listDirectory dir
    users <-
        catMaybes
            <$> forM
                [e | e <- entries, isLowerHex (T.pack e)]
                (loadUser dir)
    cache <- newTVarIO (Map.fromList [(urEmail u, u) | u <- users])
    lock <- newMVar ()
    let store = UserStore{usBaseDir = dir, usCache = cache, usLock = lock}
    forM_ (mfilterNonBlank (normalizeEmail <$> bootstrap)) (grantAdmin store)
    pure store
  where
    mfilterNonBlank (Just e) | not (T.null e) = Just e
    mfilterNonBlank _ = Nothing

{- | Whether the (normalized) email holds the admin role. Every lookup goes
through 'normalizeEmail' so a casing mismatch can't shed the role.
-}
isAdmin :: UserStore -> Text -> IO Bool
isAdmin store email = do
    m <- readTVarIO (usCache store)
    pure $ maybe False urIsAdmin (Map.lookup (normalizeEmail email) m)

-- | Whether any admin exists (for the startup inert-surface warning).
isAnyAdmin :: UserStore -> IO Bool
isAnyAdmin store = any urIsAdmin . Map.elems <$> readTVarIO (usCache store)

grantAdmin :: UserStore -> Text -> IO ()
grantAdmin store email = setAdmin store email True

revokeAdmin :: UserStore -> Text -> IO ()
revokeAdmin store email = setAdmin store email False

{- | The directory key: hex-encoded UTF-8 bytes of the normalized email.
Dependency-free and injective (no digest, no collision class), lowercase hex
only, so it passes the same path guard as share slugs.
-}
emailHash :: Text -> Text
emailHash email =
    T.pack $ concatMap hexByte (BS.unpack (TE.encodeUtf8 (normalizeEmail email)))
  where
    hexByte w =
        let n = fromIntegral w :: Int
         in [intToDigit (n `div` 16), intToDigit (n `mod` 16)]

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

{- | Upsert the role under 'usLock' so disk and cache stay consistent.
Keeps an existing record's createdAt, mints one otherwise.
-}
setAdmin :: UserStore -> Text -> Bool -> IO ()
setAdmin store email admin = withMVar (usLock store) $ \_ -> do
    let norm = normalizeEmail email
    m <- readTVarIO (usCache store)
    created <- case Map.lookup norm m of
        Just u -> pure (urCreatedAt u)
        Nothing -> T.pack . iso8601Show <$> getCurrentTime
    let user =
            UserRecord{urEmail = norm, urIsAdmin = admin, urCreatedAt = created}
        dir = usBaseDir store </> T.unpack (emailHash norm)
    createDirectoryIfMissing True dir
    BS.writeFile (dir </> "meta") (TE.encodeUtf8 (metaText user))
    atomically $ modifyTVar' (usCache store) (Map.insert norm user)

metaText :: UserRecord -> Text
metaText u =
    T.unlines
        [ writeMetaLine "email" (urEmail u)
        , writeMetaLine "isAdmin" (if urIsAdmin u then "true" else "false")
        , writeMetaLine "createdAt" (urCreatedAt u)
        ]

{- | Load one record. @isAdmin@ is exact-match @true@ — never key presence —
and the email inside @meta@ is authoritative (re-normalized defensively);
the hash is only the directory key.
-}
loadUser :: FilePath -> FilePath -> IO (Maybe UserRecord)
loadUser baseDir entry = do
    let metaF = baseDir </> entry </> "meta"
    e <- doesFileExist metaF
    if not e
        then pure Nothing
        else do
            txt <- TE.decodeUtf8Lenient <$> BS.readFile metaF
            let get k = lookup k (parseMeta txt)
            pure $ do
                email <- normalizeEmail <$> get "email"
                created <- get "createdAt"
                let admin = get "isAdmin" == Just "true"
                pure UserRecord{urEmail = email, urIsAdmin = admin, urCreatedAt = created}
