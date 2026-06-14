{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.SessionSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.STM (atomically)
import Data.Function ((&))
import Data.IORef (IORef, newIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Handlers (resolveLocalPackages, setupReplProject)
import Sabela.Output (displayPrelude)
import ScriptHs.Parser (CabalMeta (..))
import System.Directory (findExecutable)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec (
    Spec,
    describe,
    expectationFailure,
    it,
    pendingWith,
    shouldBe,
    shouldNotBe,
    shouldReturn,
    shouldSatisfy,
 )

import Data.Unique (newUnique)
import Sabela.Session (
    Marker (Marker),
    Session (..),
    SessionConfig (..),
    getMarker,
    readErrorBuffer,
    resetErrorBuffer,
    runBlock,
 )
import Sabela.Session.Drain (
    DrainResult (..),
    drainUntilMarker,
 )
import Sabela.Session.Proc (ProcSession (..))
import Sabela.Session.Process (
    closeSession,
    newSession,
    resetSession,
    startupErrorMessage,
 )
import Sabela.Session.Reader (
    OutQueue,
    enqueueEof,
    enqueueLine,
    newOutQueue,
 )

{- | A dummy Session safe for functions that only use the queue, error
buffer, counter, or config; touching process fields crashes the test.
-}
dummySession ::
    OutQueue ->
    IORef [Text] ->
    IORef Int ->
    SessionConfig ->
    IO Session
dummySession q errRef ctrRef cfg = do
    lock <- newMVar ()
    cbRef <- newIORef (\_ -> pure ())
    klock <- newMVar ()
    uid <- newUnique
    busy <- newIORef False
    let ps =
            ProcSession
                { psId = uid
                , psProc = error "dummySession: psProc used unexpectedly"
                , psPgid = Nothing
                , psKillLock = klock
                , psStdin = error "dummySession: psStdin used unexpectedly"
                , psStdout = error "dummySession: psStdout used unexpectedly"
                , psStderr = error "dummySession: psStderr used unexpectedly"
                , psQueue = q
                }
    pure
        Session
            { sessProcSess = ps
            , sessLock = lock
            , sessErrBuf = errRef
            , sessCounter = ctrRef
            , sessConfig = cfg
            , sessErrCallback = cbRef
            , sessBusy = busy
            }

push :: OutQueue -> Text -> IO ()
push q t = atomically (enqueueLine q (T.length t) t)

defaultCfg :: SessionConfig
defaultCfg = SessionConfig{scProjectDir = ".", scWorkDir = "."}

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

withTimeout :: Int -> IO a -> IO a
withTimeout usec action = do
    r <- timeout usec action
    case r of
        Nothing -> expectationFailure "Timed out" >> error "unreachable"
        Just x -> pure x

spec :: Spec
spec = do
    describe "resolveLocalPackages" $ do
        -- The sticky sabela-notebook support package is always prepended so
        -- every notebook can @import Sabela.Notebook.*@.
        let support = "/work/.sabela/sabela-notebook"
        it "always prepends the sticky support package dir" $
            resolveLocalPackages "/work" [] emptyMeta
                `shouldBe` [support]
        it "passes absolute notebook package dirs through unchanged" $
            resolveLocalPackages "/work" [] emptyMeta{metaPackages = ["/abs/pkg"]}
                `shouldBe` [support, "/abs/pkg"]
        it "resolves relative notebook dirs against the working dir" $
            resolveLocalPackages "/work" [] emptyMeta{metaPackages = ["../sibling"]}
                `shouldBe` [support, "/work/../sibling"]
        it "keeps operator overlays first and dedupes against notebook dirs" $
            resolveLocalPackages
                "/work"
                ["/op/over"]
                emptyMeta{metaPackages = ["/abs/pkg", "/op/over"]}
                `shouldBe` [support, "/op/over", "/abs/pkg"]

    describe "error buffer helpers" $ do
        it "resetErrorBuffer clears, readErrorBuffer returns lines in original order" $ do
            q <- newOutQueue
            errRef <- newIORef []
            ctrRef <- newIORef 0
            sess <- dummySession q errRef ctrRef defaultCfg

            writeIORef errRef ["third", "second", "first"]

            readErrorBuffer sess
                `shouldReturn` (T.unlines ["first", "second", "third"] & T.strip)

            resetErrorBuffer sess
            readErrorBuffer sess `shouldReturn` ""

    describe "startupErrorMessage" $ do
        it "is the bare message when no stderr was captured" $
            startupErrorMessage "" `shouldBe` "GHCi exited during startup"
        it "appends the captured cabal/GHCi stderr when present" $
            startupErrorMessage "Missing C libraries: TKMath, TKernel"
                `shouldBe` "GHCi exited during startup:\n"
                    <> "Missing C libraries: TKMath, TKernel"

    describe "drainUntilMarker" $ do
        it "collects output until marker, excluding marker" $ do
            q <- newOutQueue
            _ <- forkIO $ do
                push q "line 1"
                push q "line 2"
                push q "---SABELA_MARKER_0---"
                push q "line after (should not be read)"
            out <-
                withTimeout
                    2_000_000
                    (drainUntilMarker q "---SABELA_MARKER_0---" (\_ -> pure ()))
            out `shouldBe` DrainOk "line 1\nline 2"

        it "reports a sticky EOF if the marker never arrives" $ do
            q <- newOutQueue
            _ <- forkIO $ do
                push q "hello"
                atomically (enqueueEof q)
            out <-
                withTimeout
                    2_000_000
                    (drainUntilMarker q "---SABELA_MARKER_999---" (\_ -> pure ()))
            out `shouldBe` DrainEof "hello"
            again <-
                withTimeout
                    2_000_000
                    (drainUntilMarker q "---SABELA_MARKER_999---" (\_ -> pure ()))
            again `shouldBe` DrainEof ""

        it "discards a stale run's output at a lower-numbered marker" $ do
            q <- newOutQueue
            _ <- forkIO $ do
                push q "stale tail from run 4"
                push q "---SABELA_MARKER_4---"
                push q "real output"
                push q "---SABELA_MARKER_5---"
            out <-
                withTimeout
                    2_000_000
                    (drainUntilMarker q "---SABELA_MARKER_5---" (\_ -> pure ()))
            out `shouldBe` DrainOk "real output"

    describe "getMarker" $ do
        it "increments counter and produces distinct markers" $ do
            q <- newOutQueue
            errRef <- newIORef []
            ctrRef <- newIORef 0
            sess <- dummySession q errRef ctrRef defaultCfg

            Marker a <- getMarker sess
            Marker b <- getMarker sess

            a `shouldNotBe` b
            a `shouldSatisfy` T.isPrefixOf "---SABELA_MARKER_"
            b `shouldSatisfy` T.isPrefixOf "---SABELA_MARKER_"

    describe "displayPrelude scatter widget" $ do
        it "defines scatterSelect" $
            displayPrelude `shouldSatisfy` T.isInfixOf "scatterSelect ::"
        it "defines scatterSelectWith (granite-style options)" $
            displayPrelude `shouldSatisfy` T.isInfixOf "scatterSelectWith ::"
        it "uses balanced :{ / :} multiline blocks" $
            T.count ":{" displayPrelude `shouldBe` T.count ":}" displayPrelude

    describe "integration: ghci-backed session" $ do
        it "newSession/runBlock returns stdout and empty stderr for a simple expression" $ do
            cabal <- findExecutable "cabal"
            case cabal of
                Nothing -> pendingWith "cabal not found on PATH; skipping integration test"
                Just _ -> withSystemTempDirectory "sabela-test" $ \dir -> do
                    setupReplProject [] dir emptyMeta
                    let cfg = SessionConfig{scProjectDir = dir, scWorkDir = dir}
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
                    let cfg = SessionConfig{scProjectDir = dir, scWorkDir = dir}
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
                    let cfg = SessionConfig{scProjectDir = dir, scWorkDir = dir}
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
                    let cfg = SessionConfig{scProjectDir = dir, scWorkDir = dir}
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
