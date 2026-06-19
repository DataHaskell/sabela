{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Pins the session capture/teardown contract: reader loops terminate
at EOF with a sticky tombstone, binary and over-cap output can't kill or
bloat the server, and teardown reclaims the whole process group.
-}
module Test.SessionLoopSpec (spec) where

import Control.Concurrent (
    MVar,
    forkIO,
    newEmptyMVar,
    putMVar,
    takeMVar,
    threadDelay,
    tryReadMVar,
    tryTakeMVar,
 )
import Control.Concurrent.STM (atomically, readTVarIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import Sabela.Handlers (setupReplProject)
import Sabela.Session (
    interruptIfBusy,
    mkSessionConfig,
    runBlock,
 )
import Sabela.Session.Drain (
    DrainResult (..),
    drainUntilMarker,
    runAccumCapBytes,
 )
import Sabela.Session.Proc (
    ProcSession (..),
    destroySession,
    interruptGroup,
    sessionProcessSpec,
    withSpawnedSession,
 )
import Sabela.Session.Process (closeSession, newSession)
import Sabela.Session.Reader (
    OutQueue (..),
    dequeueLine,
    drainToEof,
    enqueueEof,
    enqueueLine,
    errLoop,
    mkMarkerText,
    newOutQueue,
    queueBytesCap,
    readLoop,
    scanDiscarded,
 )
import ScriptHs.Parser (CabalMeta (..))
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (
    createPipe,
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

popAll :: OutQueue -> IO [T.Text]
popAll q = go []
  where
    go acc = do
        r <- withTimeout 2_000_000 (atomically (dequeueLine q))
        case r of
            Nothing -> pure (reverse acc)
            Just t -> go (t : acc)

spec :: Spec
spec = do
    describe "readLoop" $ do
        it "delivers lines, then terminates with a sticky EOF tombstone" $ do
            (r, w) <- createPipe
            q <- newOutQueue
            done <- newEmptyMVar
            _ <- forkIO (readLoop r q >> putMVar done ())
            BS.hPut w "a\nb\n"
            hClose w
            popAll q `shouldReturn` ["a", "b"]
            withTimeout 2_000_000 (atomically (dequeueLine q))
                `shouldReturn` Nothing
            withTimeout 2_000_000 (takeMVar done)
        it "survives undecodable binary bytes via lenient decoding" $ do
            (r, w) <- createPipe
            q <- newOutQueue
            done <- newEmptyMVar
            _ <- forkIO (readLoop r q >> putMVar done ())
            BS.hPut w (BS.pack [0x1f, 0x8b, 0xff, 0xfe, 0x0a])
            BS.hPut w "after\n"
            hClose w
            lns <- popAll q
            length lns `shouldBe` 2
            last lns `shouldBe` "after"
            withTimeout 2_000_000 (takeMVar done)
        it "delivers a partial line at EOF before the tombstone" $ do
            (r, w) <- createPipe
            q <- newOutQueue
            _ <- forkIO (readLoop r q)
            BS.hPut w "no newline"
            hClose w
            popAll q `shouldReturn` ["no newline"]
        it "is freed from a full queue by the teardown drain" $ do
            (r, w) <- createPipe
            q <- newOutQueue
            done <- newEmptyMVar
            _ <- forkIO (readLoop r q >> putMVar done ())
            let line = BC.replicate 80 'x' <> "\n"
            mapM_ (\_ -> BS.hPut w line) [1 .. 200 :: Int]
            hClose w
            threadDelay 100_000
            withTimeout 5_000_000 (drainToEof q)
            withTimeout 2_000_000 (takeMVar done)
        it "truncates an over-cap line but rescues an embedded marker" $ do
            (r, w) <- createPipe
            q <- newOutQueue
            _ <- forkIO (readLoop r q)
            _ <- forkIO $ do
                let big = BC.replicate (10 * 1024 * 1024) 'x'
                BS.hPut w big
                BS.hPut w "yyy---SABELA_MARKER_7---zzz\n"
                BS.hPut w "after\n"
                hClose w
            lns <- popAll q
            case lns of
                (huge : rest) -> do
                    T.isSuffixOf "[line truncated by sabela]" huge
                        `shouldBe` True
                    rest `shouldSatisfy` elem "---SABELA_MARKER_7---"
                    last rest `shouldBe` "after"
                [] -> expectationFailure "no lines captured"

    describe "scanDiscarded" $
        it "finds a marker straddling two scan windows via the carry" $ do
            found <- newIORef []
            let emit _ t = modifyIORef' found (t :)
            carry <- scanDiscarded emit "noise noise ---SABELA_MAR"
            _ <- scanDiscarded emit (carry <> "KER_42---more noise")
            readIORef found `shouldReturn` ["---SABELA_MARKER_42---"]

    describe "errLoop" $ do
        it "keeps the ring bounded under a stderr flood" $ do
            (r, w) <- createPipe
            buf <- newIORef []
            cb <- newIORef (\_ -> pure ())
            done <- newEmptyMVar
            _ <- forkIO (errLoop r buf cb >> putMVar done ())
            mapM_
                (\i -> BS.hPut w (BC.pack (show (i :: Int) <> "\n")))
                [1 .. 600]
            hClose w
            withTimeout 5_000_000 (takeMVar done)
            fmap length (readIORef buf) `shouldReturn` 500
        it "captures lines and terminates at EOF instead of spinning" $ do
            (r, w) <- createPipe
            buf <- newIORef []
            cb <- newIORef (\_ -> pure ())
            done <- newEmptyMVar
            _ <- forkIO (errLoop r buf cb >> putMVar done ())
            BS.hPut w "warning: x\n"
            hClose w
            withTimeout 2_000_000 (takeMVar done)
            fmap (elem "warning: x") (readIORef buf) `shouldReturn` True

    describe "OutQueue" $
        it "lands the EOF tombstone on a full queue by dropping the oldest" $ do
            q <- newOutQueue
            mapM_
                (\i -> atomically (enqueueLine q 1 (T.pack (show (i :: Int)))))
                [1 .. 64]
            withTimeout 2_000_000 (atomically (enqueueEof q))
            lns <- popAll q
            length lns `shouldBe` 63

    describe "bounded output queue (stress cases 2,3)" $ do
        it "blocks the producer once the byte budget is exhausted" $ do
            q <- newOutQueue
            let chunk = queueBytesCap `div` 8
            done <- newEmptyMVar
            _ <- forkIO $ do
                mapM_
                    (atomically . enqueueLine q chunk . T.pack . show)
                    [1 .. 12 :: Int]
                putMVar done ()
            threadDelay 200_000
            -- 8 chunks fill the budget; the 9th enqueue must still be blocked,
            -- so the producer cannot have finished and the queue stays bounded.
            tryTakeMVar done `shouldReturn` Nothing
            used <- readTVarIO (oqBytes q)
            used `shouldSatisfy` (<= queueBytesCap)
            -- draining credits bytes back, unblocking the producer to completion.
            withTimeout 5_000_000 (drainWhilePending q done)
            withTimeout 2_000_000 (takeMVar done)

        it "caps drainUntilMarker accumulation, emits the notice, streams all" $ do
            q <- newOutQueue
            let mk = mkMarkerText 700_003
                lineLen = 1024 * 1024
                lineCount = (runAccumCapBytes `div` lineLen) + 5
                body = T.replicate lineLen "x"
            seen <- newIORef (0 :: Int)
            _ <- forkIO $ do
                mapM_
                    (\_ -> atomically (enqueueLine q lineLen body))
                    [1 .. lineCount]
                atomically (enqueueLine q (T.length mk) mk)
                atomically (enqueueEof q)
            res <-
                withTimeout
                    30_000_000
                    (drainUntilMarker q mk (\_ -> modifyIORef' seen (+ 1)))
            case res of
                DrainOk out ->
                    out
                        `shouldSatisfy` T.isSuffixOf "[output truncated by sabela]"
                DrainEof _ -> expectationFailure "ended at EOF, not the marker"
            -- every kept line is streamed to onLine even past the accum cap.
            readIORef seen `shouldReturn` lineCount


-- Pop lines (crediting the byte budget back) until the blocked producer,
-- observed via the peek-only MVar, has finished enqueuing.
drainWhilePending :: OutQueue -> MVar () -> IO ()
drainWhilePending q done = go
  where
    go = do
        finished <- tryReadMVar done
        case finished of
            Just () -> pure ()
            Nothing -> do
                _ <- atomically (dequeueLine q)
                threadDelay 1_000
                go

