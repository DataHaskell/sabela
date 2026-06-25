{-# LANGUAGE OverloadedStrings #-}

module Test.SampleVerifySpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Eval.Agent (
    SampleResult (..),
    SampleVerify (..),
    sampleVerifyOne,
 )

type Guard = (IORef Int, IORef Int)

newGuard :: IO Guard
newGuard = (,) <$> newIORef 0 <*> newIORef 0

withConcurrencyGuard :: Guard -> (Text -> IO a) -> Text -> IO a
withConcurrencyGuard (inflight, peak) act c = do
    n <- atomicModifyIORef' inflight (\k -> (k + 1, k + 1))
    atomicModifyIORef' peak (\p -> (max p n, ()))
    threadDelay 2000
    r <- act c
    atomicModifyIORef' inflight (\k -> (k - 1, ()))
    pure r

good :: Text -> Bool
good = ("good" `T.isPrefixOf`)

spec :: Spec
spec = describe "B4 sample K, verify one at a time" $ do
    it "accepts the first passing candidate and inserts only it" $ do
        inserts <- newMVar []
        let cands = ["bad0", "bad1", "good2", "good3"]
            sv =
                SampleVerify
                    { svSample = \i -> pure (cands !! i)
                    , svRollout = pure . good
                    , svInsert = \c -> modifyMVar_ inserts (pure . (c :))
                    }
        res <- sampleVerifyOne (length cands) sv
        srAccepted res `shouldBe` Just "good2"
        inserted <- readMVar inserts
        inserted `shouldBe` ["good2"]

    it "never inserts when no candidate passes" $ do
        inserts <- newMVar []
        let cands = ["bad0", "bad1", "bad2"]
            sv =
                SampleVerify
                    { svSample = \i -> pure (cands !! i)
                    , svRollout = \_ -> pure False
                    , svInsert = \c -> modifyMVar_ inserts (pure . (c :))
                    }
        res <- sampleVerifyOne (length cands) sv
        srAccepted res `shouldBe` Nothing
        inserted <- readMVar inserts
        inserted `shouldBe` []
        srProbed res `shouldBe` ["bad0", "bad1", "bad2"]

    it
        "dispatches rollouts and the insert serially even when sampling is concurrent"
        $ do
            guard <- newGuard
            let cands = map (\i -> "bad" <> T.pack (show i)) [0 :: Int .. 5] ++ ["good"]
                sv =
                    SampleVerify
                        { svSample = \i -> do
                            threadDelay 3000 -- model latency: would overlap if dispatched here
                            pure (cands !! i)
                        , svRollout = withConcurrencyGuard guard (pure . good)
                        , svInsert = withConcurrencyGuard guard (\_ -> pure ())
                        }
            _ <- sampleVerifyOne (length cands) sv
            peakSeen <- readIORef (snd guard)
            peakSeen `shouldBe` 1

    it "rolls out candidates in order and stops at the first pass" $ do
        order <- newIORef []
        let cands = ["bad0", "good1", "good2"]
            sv =
                SampleVerify
                    { svSample = \i -> pure (cands !! i)
                    , svRollout = \c -> do
                        atomicModifyIORef' order (\xs -> (xs ++ [c], ()))
                        pure (good c)
                    , svInsert = \_ -> pure ()
                    }
        _ <- sampleVerifyOne (length cands) sv
        rolled <- readIORef order
        rolled `shouldBe` ["bad0", "good1"]
