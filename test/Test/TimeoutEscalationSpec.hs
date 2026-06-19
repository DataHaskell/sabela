{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Pins the staged kill ladder (P1 watchdog-respawn, stress cases 7–10,
13, 17): 'escalateKill' walks INT → grace → TERM → grace → KILL through the
portable group wrappers, so a SIGINT-responsive computation exits on the
first rung while a signal-ignoring one is still reaped by the final KILL.
The post-timeout notice tells the user the session was killed and the next
run respawns, instead of the old opaque "session killed".
-}
module Test.TimeoutEscalationSpec (spec) where

import Control.Concurrent (threadDelay)
import qualified Data.Text as T
import Sabela.Session.Proc (
    ProcSession (..),
    destroySession,
    escalateKill,
    sessionProcessSpec,
    withSpawnedSession,
 )
import Sabela.Session.Timeout (timedOutKilledMessage)
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (getProcessExitCode, proc, waitForProcess)
import System.Timeout (timeout)
import Test.Hspec

withTimeout :: Int -> IO a -> IO a
withTimeout usec action = do
    r <- timeout usec action
    case r of
        Nothing -> expectationFailure "Timed out" >> error "unreachable"
        Just x -> pure x

waitUntilExited :: ProcSession -> IO ()
waitUntilExited ps = do
    ex <- getProcessExitCode (psProc ps)
    case ex of
        Just _ -> pure ()
        Nothing -> threadDelay 50_000 >> waitUntilExited ps

withExe :: String -> (FilePath -> IO ()) -> IO ()
withExe name action = do
    found <- findExecutable name
    case found of
        Nothing -> pendingWith (name <> " not found on PATH")
        Just p -> action p

spec :: Spec
spec = describe "timeout escalation (P1 watchdog-respawn)" $ do
    describe "post-timeout notice" $
        it "tells the user the session was killed and will respawn" $
            timedOutKilledMessage 5_000_000
                `shouldSatisfy` \m ->
                    "respawn" `T.isInfixOf` m && "5 seconds" `T.isInfixOf` m

    describe "escalateKill ladder" $ do
        it "reaps a SIGINT-responsive process on the first rung" $
            withExe "sleep" $ \_ -> do
                ps <-
                    withSpawnedSession
                        (sessionProcessSpec Nothing (proc "sleep" ["30"]))
                        pure
                threadDelay 100_000
                withTimeout 10_000_000 (escalateKill ps)
                code <- withTimeout 5_000_000 (waitForProcess (psProc ps))
                destroySession ps
                code `shouldNotBe` ExitSuccess

        it "reaps a process that ignores INT and TERM (only KILL recovers)" $
            withExe "sh" $ \_ -> do
                ps <-
                    withSpawnedSession
                        ( sessionProcessSpec
                            Nothing
                            (proc "sh" ["-c", "trap '' INT TERM; sleep 30"])
                        )
                        pure
                threadDelay 200_000
                withTimeout 10_000_000 (escalateKill ps)
                code <- withTimeout 5_000_000 (waitForProcess (psProc ps))
                destroySession ps
                code `shouldNotBe` ExitSuccess

        it "is safe (a no-op) once the leader has already exited" $
            withExe "true" $ \_ -> do
                ps <-
                    withSpawnedSession
                        (sessionProcessSpec Nothing (proc "true" []))
                        pure
                withTimeout 5_000_000 (waitUntilExited ps)
                withTimeout 5_000_000 (escalateKill ps)
                destroySession ps
