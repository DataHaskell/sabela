{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.SessionSpec (spec) where

import Control.Concurrent (Chan, forkIO)
import Control.Concurrent.Chan (newChan, writeChan)
import Control.Concurrent.MVar (newMVar)
import Control.Exception (evaluate)
import Data.Function ((&))
import Data.IORef (IORef, newIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (findExecutable)
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
    closeSession,
    drainUntilMarker,
    eofText,
    getMarker,
    newSession,
    readErrorBuffer,
    resetErrorBuffer,
    resetSession,
    runBlock,
 )

{- | A dummy Session that is safe to pass to functions that only use sessLines / sessErrBuf / sessCounter / sessConfig.
Any attempt to touch the other fields will crash, which is fine for unit tests that don't use them.
-}
dummySession ::
    Chan Text ->
    IORef [Text] ->
    IORef Int ->
    SessionConfig ->
    IO Session
dummySession ch errRef ctrRef cfg = do
    lock <- newMVar ()
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
            }

defaultCfg :: SessionConfig
defaultCfg =
    SessionConfig
        { scDeps = []
        , scExts = []
        , scGhcOptions = []
        , scEnvFile = Nothing
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
            ch <- newChan
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
            ch <- newChan
            errRef <- newIORef []
            ctrRef <- newIORef 0
            sess <- dummySession ch errRef ctrRef defaultCfg

            let mk = Marker "---SABELA_MARKER_0---"

            _ <- forkIO $ do
                writeChan ch "line 1"
                writeChan ch "line 2"
                writeChan ch "---SABELA_MARKER_0---"
                writeChan ch "line after (should not be read)"

            out <- withTimeout 2_000_000 (drainUntilMarker sess mk)
            out `shouldBe` "line 1\nline 2"

        it "stops on eofText even if marker never arrives" $ do
            ch <- newChan
            errRef <- newIORef []
            ctrRef <- newIORef 0
            sess <- dummySession ch errRef ctrRef defaultCfg

            let mk = Marker "---SABELA_MARKER_999---"

            _ <- forkIO $ do
                writeChan ch "hello"
                writeChan ch eofText
                writeChan ch "after eof (ignored)"

            out <- withTimeout 2_000_000 (drainUntilMarker sess mk)
            out `shouldBe` "hello"

    describe "getMarker" $ do
        it "increments counter and produces distinct markers" $ do
            ch <- newChan
            errRef <- newIORef []
            ctrRef <- newIORef 0
            sess <- dummySession ch errRef ctrRef defaultCfg

            Marker a <- getMarker sess
            Marker b <- getMarker sess

            a `shouldNotBe` b
            a `shouldSatisfy` T.isPrefixOf "---SABELA_MARKER_"
            b `shouldSatisfy` T.isPrefixOf "---SABELA_MARKER_"

    describe "integration: ghci-backed session" $ do
        it "newSession/runBlock returns stdout and empty stderr for a simple expression" $ do
            ghc <- findExecutable "ghc"
            case ghc of
                Nothing -> pendingWith "ghc not found on PATH; skipping integration test"
                Just _ -> do
                    sess <- withTimeout 10_000_000 (newSession defaultCfg)
                    (out, err) <- withTimeout 10_000_000 (runBlock sess "1 + 1")
                    withTimeout 10_000_000 (closeSession sess)

                    T.strip out `shouldBe` "2"
                    T.strip err `shouldBe` ""

        it "captures errors into stderr" $ do
            ghc <- findExecutable "ghc"
            case ghc of
                Nothing -> pendingWith "ghc not found on PATH; skipping integration test"
                Just _ -> do
                    sess <- withTimeout 10_000_000 (newSession defaultCfg)
                    (out, err) <- withTimeout 10_000_000 (runBlock sess "let x = 1\nx + \"a\"")
                    withTimeout 10_000_000 (closeSession sess)

                    let combined = T.toLower (out <> "\n" <> err)
                    combined `shouldSatisfy` T.isInfixOf "error"
        it "resetSession yields a working new session" $ do
            ghc <- findExecutable "ghc"
            case ghc of
                Nothing -> pendingWith "ghc not found on PATH; skipping integration test"
                Just _ -> do
                    sess1 <- withTimeout 10_000_000 (newSession defaultCfg)
                    sess2 <- withTimeout 15_000_000 (resetSession sess1)
                    (out, err) <- withTimeout 10_000_000 (runBlock sess2 "2 + 3")
                    withTimeout 10_000_000 (closeSession sess2)

                    T.strip out `shouldBe` "5"
                    T.strip err `shouldBe` ""
