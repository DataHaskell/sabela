{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Orphan prevention (kernel stress case 31): when the server dies
ungracefully, the interpreter's process group must not survive as an orphan.
This drives the REAL reaper — a stand-in "server" process, a victim spawned
in its own process group, then a SIGKILL of the stand-in — and asserts the
victim is reaped. (The earlier version unit-tested a poll loop with an
injected predicate, which masked that the production detection never fired.)

Both processes are spawned as grandchildren (backgrounded under @sh@) so they
are reparented to @init@ and fully reaped on death — a direct child would
linger as a zombie and confuse an OS @kill -0@ liveness probe.
-}
module Test.OrphanSpec (spec) where

import Test.Hspec

#if !defined(mingw32_HOST_OS)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Sabela.Session.ParentPoller (spawnReaperFor)
import System.Directory (findExecutable)
import System.Posix.Signals (nullSignal, sigKILL, signalProcess)
import System.Posix.Types (ProcessID)
import System.Process (readProcess)

-- | Run @cmd@ backgrounded under sh; return the grandchild's pid.
spawnGrandchild :: String -> IO ProcessID
spawnGrandchild cmd = do
    out <- readProcess "sh" ["-c", cmd ++ " >/dev/null 2>&1 & echo $!"] ""
    pure (read (takeWhile (`notElem` "\r\n ") (dropWhile (== ' ') out)))

isAlive :: ProcessID -> IO Bool
isAlive pid =
    either (const False) (const True)
        <$> (try (signalProcess nullSignal pid) :: IO (Either SomeException ()))

pollDead :: ProcessID -> Int -> IO Bool
pollDead pid n
    | n <= 0 = pure False
    | otherwise = do
        alive <- isAlive pid
        if alive then threadDelay 100000 >> pollDead pid (n - 1) else pure True

quietKill :: ProcessID -> IO ()
quietKill pid =
    void (try (signalProcess sigKILL pid) :: IO (Either SomeException ()))
#endif

spec :: Spec
#if defined(mingw32_HOST_OS)
spec = describe "orphan prevention (Windows)" $
    it "uses a Job Object (KILL_ON_JOB_CLOSE); runtime check is manual/CI" $
        pendingWith
            "Implemented in Sabela.Session.ParentPoller via a Job Object \
            \(KILL_ON_JOB_CLOSE); an automated kill-the-server runtime check \
            \needs a Windows host — verify on the windows.yml CI runner."
#else
spec = describe "orphan reaper (stress case 31)" $
    it "group-kills the interpreter when the server dies ungracefully" $ do
        mperl <- findExecutable "perl"
        msh <- findExecutable "sh"
        case (mperl, msh) of
            (Just _, Just _) -> do
                -- Victim guaranteed in its OWN process group (pgid == pid).
                vic <- spawnGrandchild "perl -e 'setpgrp(0,0); exec(\"sleep\",\"60\")'"
                srv <- spawnGrandchild "sleep 60"
                spawnReaperFor srv vic
                threadDelay 700000
                a0 <- isAlive vic
                a0 `shouldBe` True -- alive while server up
                quietKill srv -- ungraceful server death
                reaped <- pollDead vic 60
                quietKill srv
                quietKill vic
                reaped `shouldBe` True
            _ -> pendingWith "perl/sh not on PATH"
#endif
