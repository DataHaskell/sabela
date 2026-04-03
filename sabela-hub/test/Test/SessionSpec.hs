{-# LANGUAGE OverloadedStrings #-}

module Test.SessionSpec (spec) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Hub.Session
import Hub.Types
import Test.Hspec
import Test.MockEcs

spec :: Spec
spec = describe "Session" $ do
    describe "getOrCreateSession" $ do
        it "creates a new session on first request" $ do
            ms <- newMockState
            sm <- newSessionManager (mockEcsBackend ms) testConfig
            result <- getOrCreateSession sm (SessionId "s1") (UserId "user@test.com")
            result `shouldBe` Right "10.0.1.100"
            runs <- readTVarIO (mockRunCalls ms)
            runs `shouldBe` 1

        it "reuses existing session on second request" $ do
            ms <- newMockState
            sm <- newSessionManager (mockEcsBackend ms) testConfig
            _ <- getOrCreateSession sm (SessionId "s1") (UserId "user@test.com")
            result <- getOrCreateSession sm (SessionId "s1") (UserId "user@test.com")
            result `shouldBe` Right "10.0.1.100"
            runs <- readTVarIO (mockRunCalls ms)
            runs `shouldBe` 1

        it "creates separate sessions for different session IDs" $ do
            ms <- newMockState
            sm <- newSessionManager (mockEcsBackend ms) testConfig
            r1 <- getOrCreateSession sm (SessionId "s1") (UserId "user1@test.com")
            r2 <- getOrCreateSession sm (SessionId "s2") (UserId "user2@test.com")
            r1 `shouldBe` Right "10.0.1.100"
            r2 `shouldBe` Right "10.0.1.100"
            runs <- readTVarIO (mockRunCalls ms)
            runs `shouldBe` 2

        it "returns error when task stops unexpectedly" $ do
            ms <- newMockState
            atomically $ writeTVar (mockTaskStatus ms) TaskStopped
            sm <- newSessionManager (mockEcsBackend ms) testConfig
            result <- getOrCreateSession sm (SessionId "s1") (UserId "user@test.com")
            result `shouldBe` Left "Task stopped unexpectedly"

    describe "cleanupSession" $ do
        it "stops the task and removes the session" $ do
            ms <- newMockState
            sm <- newSessionManager (mockEcsBackend ms) testConfig
            _ <- getOrCreateSession sm (SessionId "s1") (UserId "user@test.com")
            cleanupSession sm (SessionId "s1")
            stops <- readTVarIO (mockStopCalls ms)
            stops `shouldBe` 1
            sessions <- listSessions sm
            Map.size sessions `shouldBe` 0

        it "is a no-op for nonexistent session" $ do
            ms <- newMockState
            sm <- newSessionManager (mockEcsBackend ms) testConfig
            cleanupSession sm (SessionId "nonexistent")
            stops <- readTVarIO (mockStopCalls ms)
            stops `shouldBe` 0
