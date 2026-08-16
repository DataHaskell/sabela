{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The bucket lease: one holder at a time, across threads (the property
POSIX fcntl locks lack) and across processes; bounded waits; lock files that
outlive the buckets they guard.
-}
module Test.LeaseSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (throwIO, try)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (isJust, isNothing)
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    removeDirectoryRecursive,
    removeFile,
 )
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Data.Text (Text)
import System.FilePath ((<.>), (</>))

import Sabela.Session.EnvKey (envBucketName)
import Sabela.Session.TryCache.Lease (
    leaseBucketDir,
    leaseHeldElsewhere,
    withBucketLease,
 )

keyA :: Text
keyA = "deps:aeson\nghc:9.6.7\nschema:1"

quickBudgetUs :: Int
quickBudgetUs = 200 * 1000

spec :: Spec
spec = describe "bucket leases" $ do
    it "grants the lease and derives the bucket under the root" $
        withSystemTempDirectory "lease-spec" $ \root ->
            withBucketLease root keyA quickBudgetUs $ \case
                Nothing -> expectationFailure "uncontended acquire refused"
                Just lease ->
                    take (length root) (leaseBucketDir lease)
                        `shouldBe` root

    it "excludes a second THREAD until release, within the deadline" $
        withSystemTempDirectory "lease-spec" $ \root -> do
            firstIn <- newEmptyMVar
            releaseNow <- newEmptyMVar
            secondGot <- newEmptyMVar
            _ <- forkIO $
                withBucketLease root keyA (5 * 1000 * 1000) $ \mLease -> do
                    putMVar firstIn (isJust mLease)
                    takeMVar releaseNow
            takeMVar firstIn `shouldReturn` True
            _ <- forkIO $
                withBucketLease root keyA (5 * 1000 * 1000) $ \mLease ->
                    putMVar secondGot (isJust mLease)
            threadDelay (100 * 1000)
            putMVar releaseNow ()
            takeMVar secondGot `shouldReturn` True

    it "a contended acquire past its deadline answers busy, not a block" $
        withSystemTempDirectory "lease-spec" $ \root -> do
            held <- newEmptyMVar
            releaseNow <- newEmptyMVar
            _ <- forkIO $
                withBucketLease root keyA (5 * 1000 * 1000) $ \_ -> do
                    putMVar held ()
                    takeMVar releaseNow
            takeMVar held
            second <-
                withBucketLease root keyA quickBudgetUs $ \mLease ->
                    pure (isNothing mLease)
            putMVar releaseNow ()
            second `shouldBe` True

    it "releases on an exception in the body" $
        withSystemTempDirectory "lease-spec" $ \root -> do
            r <-
                try
                    ( withBucketLease root keyA quickBudgetUs $ \_ ->
                        throwIO (userError "boom")
                    ) ::
                    IO (Either IOError ())
            r `shouldSatisfy` either (const True) (const False)
            withBucketLease root keyA quickBudgetUs $ \mLease ->
                isJust mLease `shouldBe` True

    it "the lock file survives deleting the bucket it guards" $
        withSystemTempDirectory "lease-spec" $ \root -> do
            bucket <- withBucketLease root keyA quickBudgetUs $ \case
                Nothing -> fail "uncontended acquire refused"
                Just lease -> do
                    createDirectoryIfMissing True (leaseBucketDir lease)
                    removeDirectoryRecursive (leaseBucketDir lease)
                    pure (leaseBucketDir lease)
            doesDirectoryExist bucket `shouldReturn` False
            locks <- doesFileExist (lockPathUnder root)
            locks `shouldBe` True
            withBucketLease root keyA quickBudgetUs $ \mLease ->
                isJust mLease `shouldBe` True

    it "leaseHeldElsewhere sees a held lease and never a free one" $
        withSystemTempDirectory "lease-spec" $ \root -> do
            free <- leaseHeldElsewhere root keyA
            free `shouldBe` False
            held <- newEmptyMVar
            releaseNow <- newEmptyMVar
            _ <- forkIO $
                withBucketLease root keyA (5 * 1000 * 1000) $ \_ -> do
                    putMVar held ()
                    takeMVar releaseNow
            takeMVar held
            leaseHeldElsewhere root keyA `shouldReturn` True
            putMVar releaseNow ()

    it "an acquire that dies before the OS lock leaves no poisoned slot" $
        withSystemTempDirectory "lease-spec" $ \root -> do
            writeFile (root </> "locks") "a file where the lock dir must go"
            r <-
                try (withBucketLease root keyA quickBudgetUs (\_ -> pure ())) ::
                    IO (Either IOError ())
            r `shouldSatisfy` either (const True) (const False)
            removeFile (root </> "locks")
            withBucketLease root keyA quickBudgetUs $ \mLease ->
                isJust mLease `shouldBe` True

    it "many sequential acquires neither leak nor starve" $
        withSystemTempDirectory "lease-spec" $ \root -> do
            counter <- newIORef (0 :: Int)
            mapM_
                ( \_ -> withBucketLease root keyA quickBudgetUs $ \case
                    Just _ -> modifyIORef' counter (+ 1)
                    Nothing -> pure ()
                )
                [1 .. 25 :: Int]
            readIORef counter `shouldReturn` 25
  where
    lockPathUnder root = root </> "locks" </> envBucketName keyA <.> "lock"
