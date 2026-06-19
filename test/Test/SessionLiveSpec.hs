{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Live-process session tests split out of 'Test.SessionLoopSpec' (the
module-size cap): the spawned process group is reachable and reaped, and a
running cell can be interrupted while the session stays usable.
-}
module Test.SessionLiveSpec (spec) where

import Control.Concurrent (
    forkIO,
    newEmptyMVar,
    putMVar,
    takeMVar,
    threadDelay,
 )
import qualified Data.Text as T
import Sabela.Handlers (setupReplProject)
import Sabela.Session (interruptIfBusy, mkSessionConfig, runBlock)
import Sabela.Session.Proc (
    ProcSession (..),
    destroySession,
    interruptGroup,
    sessionProcessSpec,
    withSpawnedSession,
 )
import Sabela.Output (displayPrelude)
import Sabela.Session.Process (closeSession, newSession, resetSession)
import ScriptHs.Parser (CabalMeta (..))
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (
    getPid,
    getProcessExitCode,
    proc,
    readProcessWithExitCode,
    waitForProcess,
 )
import System.Timeout (timeout)
import Test.Hspec

withTimeout :: Int -> IO a -> IO a
withTimeout usec action = do
    r <- timeout usec action
    case r of
        Nothing -> expectationFailure "Timed out" >> error "unreachable"
        Just x -> pure x

spec :: Spec
spec = do
    describe "session process group" $ do
        it "interruptGroup reaches the spawned group" $ do
            sleep <- findExecutable "sleep"
            case sleep of
                Nothing -> pendingWith "sleep not found on PATH"
                Just _ -> do
                    ps <-
                        withSpawnedSession
                            (sessionProcessSpec Nothing (proc "sleep" ["30"]))
                            pure
                    threadDelay 100_000
                    interruptGroup ps
                    code <- withTimeout 5_000_000 (waitForProcess (psProc ps))
                    destroySession ps
                    code `shouldNotBe` ExitSuccess
        it "destroySession kills grandchildren, not just the direct child" $ do
            pgrep <- findExecutable "pgrep"
            case pgrep of
                Nothing -> pendingWith "pgrep not found on PATH"
                Just _ -> do
                    ps <-
                        withSpawnedSession
                            ( sessionProcessSpec
                                Nothing
                                (proc "sh" ["-c", "sleep 30 & wait"])
                            )
                            pure
                    Just pid <- getPid (psProc ps)
                    threadDelay 200_000
                    withTimeout 10_000_000 (destroySession ps)
                    (code, out, _) <-
                        readProcessWithExitCode "pgrep" ["-g", show pid] ""
                    (code, words out) `shouldSatisfy` \(c, pids) ->
                        c /= ExitSuccess || null pids
        it "destroySession is idempotent and safe after the leader exited" $ do
            true <- findExecutable "true"
            case true of
                Nothing -> pendingWith "true not found on PATH"
                Just _ -> do
                    ps <-
                        withSpawnedSession
                            (sessionProcessSpec Nothing (proc "true" []))
                            pure
                    withTimeout 5_000_000 (waitUntilExited ps)
                    withTimeout 10_000_000 (destroySession ps)
                    withTimeout 10_000_000 (destroySession ps)

    describe "integration: interrupt" $
        it "stops a running cell; idle interrupts no-op; session stays usable" $ do
            cabal <- findExecutable "cabal"
            case cabal of
                Nothing -> pendingWith "cabal not found on PATH"
                Just _ -> withSystemTempDirectory "sabela-test" $ \dir -> do
                    setupReplProject [] dir emptyMeta
                    cfg <- mkSessionConfig dir dir
                    sess <- withTimeout 120_000_000 (newSession cfg)
                    interruptIfBusy sess
                    done <- newEmptyMVar
                    _ <- forkIO $ do
                        r <-
                            runBlock
                                sess
                                "Control.Concurrent.threadDelay 60000000"
                        putMVar done r
                    threadDelay 3_000_000
                    interruptIfBusy sess
                    (_, err) <- withTimeout 30_000_000 (takeMVar done)
                    (out2, _) <-
                        withTimeout 30_000_000 (runBlock sess "21 * 2")
                    withTimeout 15_000_000 (closeSession sess)
                    err `shouldSatisfy` T.isInfixOf "Interrupted"
                    T.strip out2 `shouldBe` "42"

    describe "integration: ghci-backed session" $ do
        it "newSession/runBlock returns stdout and empty stderr for a simple expression" $ do
            cabal <- findExecutable "cabal"
            case cabal of
                Nothing -> pendingWith "cabal not found on PATH; skipping integration test"
                Just _ -> withSystemTempDirectory "sabela-test" $ \dir -> do
                    setupReplProject [] dir emptyMeta
                    cfg <- mkSessionConfig dir dir
                    sess <- withTimeout 60_000_000 (newSession cfg)
                    (out, err) <- withTimeout 10_000_000 (runBlock sess "1 + 1")
                    withTimeout 10_000_000 (closeSession sess)

                    T.strip out `shouldBe` "2"
                    T.strip err `shouldBe` ""

        it "captures errors into stderr" $ do
            cabal <- findExecutable "cabal"
            case cabal of
                Nothing -> pendingWith "cabal not found on PATH; skipping integration test"
                Just _ -> withSystemTempDirectory "sabela-test" $ \dir -> do
                    setupReplProject [] dir emptyMeta
                    cfg <- mkSessionConfig dir dir
                    sess <- withTimeout 60_000_000 (newSession cfg)
                    (out, err) <- withTimeout 10_000_000 (runBlock sess "let x = 1\nx + \"a\"")
                    withTimeout 10_000_000 (closeSession sess)

                    let combined = T.toLower (out <> "\n" <> err)
                    combined `shouldSatisfy` T.isInfixOf "error"

        it "resetSession yields a working new session" $ do
            cabal <- findExecutable "cabal"
            case cabal of
                Nothing -> pendingWith "cabal not found on PATH; skipping integration test"
                Just _ -> withSystemTempDirectory "sabela-test" $ \dir -> do
                    setupReplProject [] dir emptyMeta
                    cfg <- mkSessionConfig dir dir
                    sess1 <- withTimeout 60_000_000 (newSession cfg)
                    sess2 <- withTimeout 60_000_000 (resetSession sess1)
                    (out, err) <- withTimeout 10_000_000 (runBlock sess2 "2 + 3")
                    withTimeout 10_000_000 (closeSession sess2)

                    T.strip out `shouldBe` "5"
                    T.strip err `shouldBe` ""

        it "scatterSelect renders a canvas and round-trips its selection" $ do
            cabal <- findExecutable "cabal"
            case cabal of
                Nothing -> pendingWith "cabal not found on PATH; skipping integration test"
                Just _ -> withSystemTempDirectory "sabela-test" $ \dir -> do
                    setupReplProject [] dir emptyMeta
                    cfg <- mkSessionConfig dir dir
                    sess <- withTimeout 60_000_000 (newSession cfg)
                    -- mirror a dataframe cell's default-extensions so the prelude
                    -- is type-checked under OverloadedStrings (catches ambiguous
                    -- IsString literals that a bare session would let through)
                    _ <-
                        withTimeout 10_000_000 $
                            runBlock
                                sess
                                ":set -XOverloadedStrings -XTypeApplications -XScopedTypeVariables"
                    _ <- withTimeout 30_000_000 (runBlock sess displayPrelude)
                    (selOut, _) <-
                        withTimeout 20_000_000 $
                            runBlock
                                sess
                                "writeIORef _sabelaWidgetRef [(\"s\",\"[0,2]\")] >> (sample (scatterSelect \"s\" [(1.0,2.0),(3.0,4.0),(5.0,6.0)]) >>= print)"
                    (htmlOut, _) <-
                        withTimeout 20_000_000 $
                            runBlock
                                sess
                                "display (scatterSelect \"s\" [(1.0,2.0),(3.0,4.0)]) >> return ()"
                    (optOut, _) <-
                        withTimeout 20_000_000 $
                            runBlock
                                sess
                                "render (scatterSelectWith \"s\" (defScatter {soTitle = \"MyTitle\", soColorBy = [1,2,3]}) [(1.0,2.0),(3.0,4.0),(5.0,6.0)])"
                    withTimeout 10_000_000 (closeSession sess)

                    T.strip selOut `shouldBe` "[0,2]"
                    htmlOut `shouldSatisfy` T.isInfixOf "text/html"
                    htmlOut `shouldSatisfy` T.isInfixOf "<canvas"
                    htmlOut `shouldSatisfy` T.isInfixOf "parent.postMessage"
                    -- granite-style options thread into the canvas: title + color-by data
                    optOut `shouldSatisfy` T.isInfixOf "MyTitle"
                    optOut `shouldSatisfy` T.isInfixOf "cval:[1.0,2.0,3.0"

waitUntilExited :: ProcSession -> IO ()
waitUntilExited ps = do
    ex <- getProcessExitCode (psProc ps)
    case ex of
        Just _ -> pure ()
        Nothing -> threadDelay 50_000 >> waitUntilExited ps

emptyMeta :: CabalMeta
emptyMeta =
    CabalMeta
        { metaDeps = []
        , metaExts = []
        , metaGhcOptions = []
        , metaExtraLibDirs = []
        , metaExtraIncludeDirs = []
        , metaPackages = []
        , metaSourceRepos = []
        , metaUnknownKeys = []
        }
