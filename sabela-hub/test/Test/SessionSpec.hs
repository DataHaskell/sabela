{-# LANGUAGE OverloadedStrings #-}

module Test.SessionSpec (spec) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Time (getCurrentTime)
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

    describe "lookupBySessionId" $ do
        it "resolves a normal session id" $ do
            ms <- newMockState
            sm <- newSessionManager (mockEcsBackend ms) testConfig
            _ <- getOrCreateSession sm (SessionId "s1") (UserId "u@x")
            r <- lookupBySessionId sm (SessionId "s1")
            (sessionUserId <$> r) `shouldBe` Just (UserId "u@x")

        it "a forged reattach: session id cannot adopt a placeholder" $ do
            ms <- newMockState
            sm <- newSessionManager (mockEcsBackend ms) testConfig
            now <- getCurrentTime
            -- Placeholder lives under the ReattachPlaceholder constructor;
            -- any SessionId lookup goes through UserSession, so the two
            -- name-spaces are disjoint at the type level.
            insertSession sm (ReattachPlaceholder (TaskId "sabela-user-victim")) $
                Session
                    (TaskId "sabela-user-victim")
                    (SReady (TaskIp "x"))
                    now
                    (UserId "")
                    Authed
                    Nothing
            r <- lookupBySessionId sm (SessionId "reattach:sabela-user-victim")
            r `shouldSatisfy` isNothing
