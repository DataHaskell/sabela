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
    mkSessionConfig,
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
import Sabela.Session.Timeout (
    defaultTimeoutConfig,
    tcExecutionUs,
    tcResyncUs,
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
    qlock <- newMVar ()
    cbRef <- newIORef (\_ -> pure ())
    klock <- newMVar ()
    uid <- newUnique
    busy <- newIORef False
    lastInt <- newIORef Nothing
    gen <- newIORef 1
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
            , sessQueryLock = qlock
            , sessErrBuf = errRef
            , sessCounter = ctrRef
            , sessConfig = cfg
            , sessErrCallback = cbRef
            , sessBusy = busy
            , sessNonce = 4242
            , sessLastInterruptTime = lastInt
            , sessionGen = gen
            }

push :: OutQueue -> Text -> IO ()
push q t = atomically (enqueueLine q (T.length t) t)

defaultCfg :: SessionConfig
defaultCfg =
    SessionConfig
        { scProjectDir = "."
        , scWorkDir = "."
        , scExecutionTimeoutUs = tcExecutionUs defaultTimeoutConfig
        , scResyncTimeoutUs = tcResyncUs defaultTimeoutConfig
        }

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

