{- | One holder per cache bucket, across threads and processes: an in-process
registry slot (interruptible, deadline-able) stacked on an OS file lock from
base's GHC.IO.Handle.Lock (flock on POSIX, LockFileEx on Windows). Lock files
live OUTSIDE the buckets they guard and are never deleted, so deleting a
bucket cannot orphan a waiter onto an unlinked inode.
-}
module Sabela.Session.TryCache.Lease (
    Lease,
    heldForBucketName,
    leaseBucketDir,
    leaseHeldElsewhere,
    leaseKeyText,
    withBucketLease,
    withCandidateLease,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (
    MVar,
    modifyMVar,
    newMVar,
    putMVar,
    takeMVar,
    tryTakeMVar,
 )
import Control.Exception (bracket, bracketOnError, finally, handle, onException, try)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.IO.Handle.Lock (
    FileLockingNotSupported (..),
    LockMode (ExclusiveLock),
    hTryLock,
 )
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((<.>), (</>))
import System.IO (Handle, IOMode (ReadWriteMode), hClose, openFile, withFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

import Sabela.Session.EnvKey (envBucketName)

-- | Proof that this thread holds the bucket; mutations require one.
data Lease = Lease
    { leaseBucketDir :: FilePath
    , leaseKeyText :: Text
    , leaseSlot :: MVar ()
    , leaseHandle :: Maybe Handle
    }

registry :: MVar (M.Map FilePath (MVar ()))
registry = unsafePerformIO (newMVar M.empty)
{-# NOINLINE registry #-}

slotFor :: FilePath -> IO (MVar ())
slotFor path = modifyMVar registry $ \m -> case M.lookup path m of
    Just s -> pure (m, s)
    Nothing -> do
        s <- newMVar ()
        pure (M.insert path s m, s)

lockPathFor :: FilePath -> String -> FilePath
lockPathFor root bucketName = root </> "locks" </> bucketName <.> "lock"

pollIntervalUs :: Int
pollIntervalUs = 100 * 1000

{- | Runs the body with the bucket's lease, or with Nothing when another
holder kept it past the deadline. Acquire and release are exception-safe;
a filesystem that refuses file locks degrades to the in-process slot alone.
-}
withBucketLease :: FilePath -> Text -> Int -> (Maybe Lease -> IO a) -> IO a
withBucketLease root keyText deadlineUs = bracket acquire release
  where
    bucketName = envBucketName keyText
    lockPath = lockPathFor root bucketName
    acquire = do
        startNs <- getMonotonicTimeNSec
        slot <- slotFor lockPath
        got <- timeout (max 1 deadlineUs) (takeMVar slot)
        case got of
            Nothing -> pure Nothing
            Just () -> (`onException` putMVar slot ()) $ do
                mHandle <- osAcquire startNs
                case mHandle of
                    Left () -> do
                        putMVar slot ()
                        pure Nothing
                    Right h ->
                        pure . Just $
                            Lease
                                { leaseBucketDir = root </> bucketName
                                , leaseKeyText = keyText
                                , leaseSlot = slot
                                , leaseHandle = h
                                }
    osAcquire startNs = do
        createDirectoryIfMissing True (root </> "locks")
        bracketOnError (openFile lockPath ReadWriteMode) hClose $ \h -> do
            r <- try (pollLock startNs h)
            case r of
                Left FileLockingNotSupported -> hClose h >> pure (Right Nothing)
                Right True -> pure (Right (Just h))
                Right False -> hClose h >> pure (Left ())
    pollLock startNs h = do
        locked <- hTryLock h ExclusiveLock
        if locked
            then pure True
            else do
                nowNs <- getMonotonicTimeNSec
                let spentUs = fromIntegral ((nowNs - startNs) `div` 1000) :: Int
                if spentUs + pollIntervalUs >= deadlineUs
                    then pure False
                    else threadDelay pollIntervalUs >> pollLock startNs h
    release Nothing = pure ()
    release (Just lease) =
        closeQuietly (leaseHandle lease) `finally` putMVar (leaseSlot lease) ()

-- | Whether some other holder (thread or process) has the key's bucket.
leaseHeldElsewhere :: FilePath -> Text -> IO Bool
leaseHeldElsewhere root keyText =
    heldForBucketName root (envBucketName keyText)

{- | The same question asked from a bucket directory name, for eviction and
sweeps that walk the cache without knowing key texts.
-}
heldForBucketName :: FilePath -> String -> IO Bool
heldForBucketName root bucketName = do
    let lockPath = lockPathFor root bucketName
    slot <- slotFor lockPath
    got <- tryTakeMVar slot
    case got of
        Nothing -> pure True
        Just () -> osHeld lockPath `finally` putMVar slot ()
  where
    osHeld lockPath = do
        exists <- doesFileExist lockPath
        if not exists
            then pure False
            else handle (\FileLockingNotSupported -> pure False) $
                withFile lockPath ReadWriteMode $ \h -> do
                    locked <- hTryLock h ExclusiveLock
                    pure (not locked)

{- | Non-blocking: take the bucket's lease, run the action while HOLDING it,
release, and answer whether the action ran. Deletion under this lease cannot
race a concurrent acquire — the property a probe-then-delete lacks.
-}
withCandidateLease :: FilePath -> String -> IO () -> IO Bool
withCandidateLease root bucketName act = do
    let lockPath = lockPathFor root bucketName
    slot <- slotFor lockPath
    got <- tryTakeMVar slot
    case got of
        Nothing -> pure False
        Just () -> (`finally` putMVar slot ()) $ do
            exists <- doesFileExist lockPath
            if not exists
                then act >> pure True
                else handle (\FileLockingNotSupported -> act >> pure True) $
                    withFile lockPath ReadWriteMode $ \h -> do
                        locked <- hTryLock h ExclusiveLock
                        if locked then act >> pure True else pure False

closeQuietly :: Maybe Handle -> IO ()
closeQuietly mh = do
    r <- try (mapM_ hClose mh) :: IO (Either IOError ())
    either (const (pure ())) pure r
