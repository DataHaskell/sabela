{-# LANGUAGE OverloadedStrings #-}

module Test.ReaperSpec (spec) where

import Control.Concurrent.STM
import Hub.Reaper
import Hub.Session
import Hub.Types
import Test.Hspec
import Test.MockEcs

spec :: Spec
spec = describe "Reaper.sweepOrphans" $ do
    it "stops every running task at startup when nothing is tracked" $ do
        ms <- newMockState
        atomically $
            writeTVar
                (mockRunningTasks ms)
                [TaskId "arn:task/a", TaskId "arn:task/b", TaskId "arn:task/c"]
        sm <- newSessionManager (mockEcsBackend ms) testConfig
        sweepOrphans sm
        stops <- readTVarIO (mockStopCalls ms)
        stops `shouldBe` 3

    it "leaves tracked tasks alone" $ do
        ms <- newMockState
        sm <- newSessionManager (mockEcsBackend ms) testConfig
        atomically $
            writeTVar
                (mockRunningTasks ms)
                [ TaskId "arn:task/a"
                , TaskId "arn:aws:ecs:us-east-1:123:task/mock/abc123"
                , TaskId "arn:task/c"
                ]
        _ <- getOrCreateSession sm (SessionId "s1") (UserId "user@test.com")
        sweepOrphans sm
        stops <- readTVarIO (mockStopCalls ms)
        stops `shouldBe` 2

    it "is a no-op when nothing is running" $ do
        ms <- newMockState
        sm <- newSessionManager (mockEcsBackend ms) testConfig
        sweepOrphans sm
        stops <- readTVarIO (mockStopCalls ms)
        stops `shouldBe` 0
