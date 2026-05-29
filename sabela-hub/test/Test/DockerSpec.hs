{-# LANGUAGE OverloadedStrings #-}

{- | Phase 1 acceptance tests for the local-Docker backend: pure argv builders
plus an in-process end-to-end of the session lifecycle (create, same-user
idempotency, reattach-not-reap, liveness, idle override) driven through a
fake 'DockerOps' that simulates the real daemon's behaviour.
-}
module Test.DockerSpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (addUTCTime, getCurrentTime)
import Hub.Docker
import Hub.Reaper (reapIdle, reconcileLiveness)
import Hub.Session
import Hub.Types
import Test.Hspec
import Test.MockEcs (testConfig)

testDockerConfig :: DockerConfig
testDockerConfig =
    DockerConfig
        { dcImage = "datahaskell/sabela:latest"
        , dcNetwork = "sabela-net"
        , dcDataRoot = "/mnt/sabela"
        , dcEnv = [("CABAL_DIR", "/root/.cabal")]
        , dcMemory = "6g"
        , dcCpus = "2"
        , dcNamePrefix = "sabela-user-"
        }

-- | In-memory stand-in for the Docker daemon, keyed by container name.
data FakeDocker = FakeDocker
    { fdContainers :: TVar (Map.Map Text TaskStatus)
    , fdRunCount :: TVar Int
    }

newFakeDocker :: IO FakeDocker
newFakeDocker = FakeDocker <$> newTVarIO Map.empty <*> newTVarIO 0

fakeDockerOps :: FakeDocker -> DockerOps
fakeDockerOps fd =
    DockerOps
        { doRun = \spec -> atomically $ do
            modifyTVar' (fdRunCount fd) (+ 1)
            modifyTVar'
                (fdContainers fd)
                (Map.insert (rsName spec) (TaskRunning (TaskIp (rsName spec))))
        , doInspect = \name -> atomically $ do
            m <- readTVar (fdContainers fd)
            pure (Map.findWithDefault TaskStopped name m)
        , doRemove = atomically . modifyTVar' (fdContainers fd) . Map.delete
        , doList = \prefix -> atomically $ do
            m <- readTVar (fdContainers fd)
            pure
                [ TaskId n
                | (n, st) <- Map.toList m
                , prefix `T.isPrefixOf` n
                , isRunning st
                ]
        }
  where
    isRunning (TaskRunning _) = True
    isRunning _ = False

seedRunning :: FakeDocker -> Text -> IO ()
seedRunning fd name =
    atomically $
        modifyTVar' (fdContainers fd) (Map.insert name (TaskRunning (TaskIp name)))

newHarness :: IO (SessionManager, FakeDocker)
newHarness = do
    fd <- newFakeDocker
    backend <- dockerBackend testDockerConfig (fakeDockerOps fd)
    sm <- newSessionManager backend testConfig
    pure (sm, fd)

spec :: Spec
spec = describe "Hub.Docker" $ do
    describe "pure arg builders" $ do
        it "containerName sanitizes @ and . then prefixes" $
            containerName testDockerConfig "user@test.com"
                `shouldBe` "sabela-user-user_test_com"

        it "userRunSpec → runArgs is the expected docker run argv" $
            runArgs (userRunSpec testDockerConfig "a@b.com")
                `shouldBe` [ "run"
                           , "-d"
                           , "--name"
                           , "sabela-user-a_b_com"
                           , "--network"
                           , "sabela-net"
                           , "--memory"
                           , "6g"
                           , "--memory-swap"
                           , "6g"
                           , "--cpus"
                           , "2"
                           , "--pids-limit"
                           , "512"
                           , "-v"
                           , "/mnt/sabela/users/a_b_com:/mnt/sabela/users/a_b_com"
                           , "-v"
                           , "/mnt/sabela/lean:/mnt/sabela/lean:ro"
                           , "-v"
                           , "/mnt/sabela/python:/mnt/sabela/python:ro"
                           , "-e"
                           , "CABAL_DIR=/root/.cabal"
                           , "datahaskell/sabela:latest"
                           , "/opt/bin/sabela"
                           , "3000"
                           , "/mnt/sabela/users/a_b_com"
                           ]

        it "inspect / stop / list argv" $ do
            inspectArgs "c"
                `shouldBe` [ "inspect"
                           , "-f"
                           , "{{.State.Running}}|{{.State.Status}}"
                           , "c"
                           ]
            stopArgs "c" `shouldBe` ["rm", "-f", "c"]
            listArgs "sabela-user-"
                `shouldBe` [ "ps"
                           , "--filter"
                           , "name=sabela-user-"
                           , "--filter"
                           , "status=running"
                           , "--format"
                           , "{{.Names}}"
                           ]

        it "parseInspect maps docker state to TaskStatus" $ do
            parseInspect "c" "true|running\n" `shouldBe` TaskRunning (TaskIp "c")
            parseInspect "c" "false|exited" `shouldBe` TaskStopped
            parseInspect "c" "false|created" `shouldBe` TaskPending

        it "parseList drops blanks" $
            parseList "sabela-user-a\nsabela-user-b\n\n"
                `shouldBe` [TaskId "sabela-user-a", TaskId "sabela-user-b"]

    describe "session lifecycle (fake Docker)" $ do
        it "spawns one container and returns its address" $ do
            (sm, fd) <- newHarness
            r <- getOrCreateSession sm (SessionId "s1") (UserId "a@b.com")
            r `shouldBe` Right "sabela-user-a_b_com"
            readTVarIO (fdRunCount fd) `shouldReturn` 1

        it "two sessions for the same user reuse one container" $ do
            (sm, fd) <- newHarness
            _ <- getOrCreateSession sm (SessionId "s1") (UserId "a@b.com")
            _ <- getOrCreateSession sm (SessionId "s2") (UserId "a@b.com")
            readTVarIO (fdRunCount fd) `shouldReturn` 1

        it "concurrent creation for one user runs docker once" $ do
            (sm, fd) <- newHarness
            v1 <- newEmptyMVar
            v2 <- newEmptyMVar
            _ <-
                forkIO
                    ( getOrCreateSession sm (SessionId "s1") (UserId "a@b.com")
                        >>= putMVar v1
                    )
            _ <-
                forkIO
                    ( getOrCreateSession sm (SessionId "s2") (UserId "a@b.com")
                        >>= putMVar v2
                    )
            _ <- takeMVar v1
            _ <- takeMVar v2
            readTVarIO (fdRunCount fd) `shouldReturn` 1

        it "cleanupSession stops the container" $ do
            (sm, fd) <- newHarness
            _ <- getOrCreateSession sm (SessionId "s1") (UserId "a@b.com")
            cleanupSession sm (SessionId "s1")
            readTVarIO (fdContainers fd) `shouldReturn` Map.empty

    describe "reattach + liveness" $ do
        it "reattaches a running container instead of reaping it" $ do
            (sm, fd) <- newHarness
            seedRunning fd "sabela-user-x_y"
            reattachSessions sm
            ss <- listSessions sm
            Map.keys ss `shouldBe` [ReattachPlaceholder (TaskId "sabela-user-x_y")]
            m <- readTVarIO (fdContainers fd)
            Map.member "sabela-user-x_y" m `shouldBe` True
            readTVarIO (fdRunCount fd) `shouldReturn` 0

        it "a returning user adopts the reattached container" $ do
            (sm, fd) <- newHarness
            seedRunning fd "sabela-user-x_y"
            reattachSessions sm
            _ <- getOrCreateSession sm (SessionId "real") (UserId "x@y")
            ss <- listSessions sm
            Map.keys ss `shouldBe` [UserSession (SessionId "real")]
            readTVarIO (fdRunCount fd) `shouldReturn` 0

        it "liveness reaps a session whose container died" $ do
            (sm, fd) <- newHarness
            _ <- getOrCreateSession sm (SessionId "s1") (UserId "a@b.com")
            -- container vanishes (OOM / crash / external rm)
            atomically $
                modifyTVar' (fdContainers fd) (Map.delete "sabela-user-a_b_com")
            reconcileLiveness sm
            ss <- listSessions sm
            Map.keys ss `shouldBe` []

        it "idle reaper honors a per-session override" $ do
            (sm, fd) <- newHarness
            seedRunning fd "sabela-user-old"
            now <- getCurrentTime
            insertSession sm (UserSession (SessionId "old")) $
                Session
                    { sessionTaskId = TaskId "sabela-user-old"
                    , sessionState = SReady (TaskIp "sabela-user-old")
                    , sessionLastActivity = addUTCTime (-100) now
                    , sessionUserId = UserId "old@x"
                    , sessionKind = Public
                    , sessionIdleOverride = Just 1
                    }
            reapIdle sm
            ss <- listSessions sm
            Map.keys ss `shouldBe` []
