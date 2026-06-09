{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.SessionSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.STM (TBQueue, atomically, newTBQueueIO, writeTBQueue)
import Control.Exception (evaluate)
import Data.Function ((&))
import Data.IORef (IORef, newIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Handlers (setupReplProject)
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

import Sabela.Session (
    Marker (Marker),
    Session (..),
    SessionConfig (..),
    drainUntilMarker,
    eofText,
    getMarker,
    readErrorBuffer,
    resetErrorBuffer,
    runBlock,
 )
import Sabela.Session.Process (
    closeSession,
    newSession,
    resetSession,
 )

{- | A dummy Session that is safe to pass to functions that only use sessLines / sessErrBuf / sessCounter / sessConfig.
Any attempt to touch the other fields will crash, which is fine for unit tests that don't use them.
-}
dummySession ::
    TBQueue Text ->
    IORef [Text] ->
    IORef Int ->
    SessionConfig ->
    IO Session
dummySession ch errRef ctrRef cfg = do
    lock <- newMVar ()
    cbRef <- newIORef (\_ -> pure ())
    pure
        Session
            { sessLock = lock
            , sessStdin = error "dummySession: sessStdin used unexpectedly"
            , sessStdout = error "dummySession: sessStdout used unexpectedly"
            , sessStderr = error "dummySession: sessStderr used unexpectedly"
            , sessProc = error "dummySession: sessProc used unexpectedly"
            , sessLines = ch
            , sessErrBuf = errRef
            , sessCounter = ctrRef
            , sessConfig = cfg
            , sessErrCallback = cbRef
            }

defaultCfg :: SessionConfig
defaultCfg = SessionConfig{scProjectDir = ".", scWorkDir = "."}

emptyMeta :: CabalMeta
emptyMeta =
    CabalMeta
        { metaDeps = []
        , metaExts = []
        , metaGhcOptions = []
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
    describe "processHandle" $ do
        it "runs eofHandler when hIsEOF is True" $ do
            evaluate (eofText `seq` ()) `shouldReturn` ()

    describe "error buffer helpers" $ do
        it "resetErrorBuffer clears, readErrorBuffer returns lines in original order" $ do
            ch <- newTBQueueIO 256
            errRef <- newIORef []
            ctrRef <- newIORef 0
            sess <- dummySession ch errRef ctrRef defaultCfg

            writeIORef errRef ["third", "second", "first"]

            readErrorBuffer sess
                `shouldReturn` (T.unlines ["first", "second", "third"] & T.strip)

            resetErrorBuffer sess
            readErrorBuffer sess `shouldReturn` ""

    describe "drainUntilMarker" $ do
        it "collects output until marker, excluding marker" $ do
            ch <- newTBQueueIO 256
            errRef <- newIORef []
            ctrRef <- newIORef 0
            sess <- dummySession ch errRef ctrRef defaultCfg

            let mk = Marker "---SABELA_MARKER_0---"

            _ <- forkIO $ do
                atomically $ writeTBQueue ch "line 1"
                atomically $ writeTBQueue ch "line 2"
                atomically $ writeTBQueue ch "---SABELA_MARKER_0---"
                atomically $ writeTBQueue ch "line after (should not be read)"

            out <- withTimeout 2_000_000 (drainUntilMarker sess mk)
            out `shouldBe` "line 1\nline 2"

        it "stops on eofText even if marker never arrives" $ do
            ch <- newTBQueueIO 256
            errRef <- newIORef []
            ctrRef <- newIORef 0
            sess <- dummySession ch errRef ctrRef defaultCfg

            let mk = Marker "---SABELA_MARKER_999---"

            _ <- forkIO $ do
                atomically $ writeTBQueue ch "hello"
                atomically $ writeTBQueue ch eofText
                atomically $ writeTBQueue ch "after eof (ignored)"

            out <- withTimeout 2_000_000 (drainUntilMarker sess mk)
            out `shouldBe` "hello"

    describe "getMarker" $ do
        it "increments counter and produces distinct markers" $ do
            ch <- newTBQueueIO 256
            errRef <- newIORef []
            ctrRef <- newIORef 0
            sess <- dummySession ch errRef ctrRef defaultCfg

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
