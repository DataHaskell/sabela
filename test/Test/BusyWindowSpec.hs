{-# LANGUAGE OverloadedStrings #-}

{- | The post-settled consistency window (R6.4): a settle-then-immediate-write
sequence never yields a busy denial across generated eventboard-generation
interleavings — occupancy observed while the generation has not advanced past
the settled one is the settling run's release tail, so admission retries
instead of denying. A genuine busy (generation advanced, or no settle
observed) names the locking cell and its elapsed time.
-}
module Test.BusyWindowSpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (forM_, void)
import Data.Aeson (Value (..), object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)

import Sabela.AI.KernelVocab (
    BusyEvidence (..),
    BusyVerdict (..),
    busyDenyJson,
    busyVerdict,
    resolveOccupied,
 )
import qualified Sabela.AI.Store as AIStore
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.Session.Admission (Admission (..))
import Sabela.State (App (..), bumpGeneration)
import Test.Hspec
import Test.WriteAckFixture (
    callTool,
    fastRn,
    field,
    insertSrc,
    mkFixture,
    mkScriptedFixture,
    withAckEnv,
 )

spec :: Spec
spec = describe "post-settled consistency window (R6.4)" $ do
    describe "busyVerdict (pure law over the evidence grid)" $ do
        it "never denies inside the window, over every interleaving"
            $ forM_
                [ BusyEvidence occ (Just g) g holder
                | occ <- [False, True]
                , g <- [0, 1, 5, 100]
                , holder <- [Nothing, Just (3, 250)]
                ]
            $ \e -> case busyVerdict e of
                DenyBusy _ ->
                    expectationFailure
                        ("denied inside the window: " <> show e)
                _ -> pure ()

        it "admits whenever the kernel is not occupied"
            $ forM_
                [ BusyEvidence False sg g h
                | sg <- [Nothing, Just 0, Just 3]
                , g <- [0, 3, 7]
                , h <- [Nothing, Just (1, 10)]
                ]
            $ \e -> busyVerdict e `shouldBe` AdmitNow

        it "denies a genuine busy (gen advanced or no settle), naming the holder" $ do
            busyVerdict (BusyEvidence True (Just 3) 4 (Just (7, 900)))
                `shouldBe` DenyBusy (Just (7, 900))
            busyVerdict (BusyEvidence True Nothing 0 Nothing)
                `shouldBe` DenyBusy Nothing

    describe "resolveOccupied (scripted settle-then-immediate-write)" $ do
        it "admits once the release tail drops, for tails of 0..5 samples" $
            forM_ [0 :: Int, 1, 2, 3, 4, 5] $ \tail' -> do
                remaining <- newIORef tail'
                let sample = do
                        n <- readIORef remaining
                        modifyIORef' remaining (max 0 . subtract 1)
                        pure (BusyEvidence (n > 0) (Just 9) 9 Nothing)
                v <- resolveOccupied 8 (pure ()) sample
                v `shouldBe` AdmitNow

        it "denies when a NEW generation appears mid-retry" $ do
            step <- newIORef (0 :: Int)
            let sample = do
                    n <- readIORef step
                    writeIORef step (n + 1)
                    pure $
                        if n < 2
                            then BusyEvidence True (Just 4) 4 Nothing
                            else BusyEvidence True (Just 4) 5 (Just (2, 40))
            v <- resolveOccupied 8 (pure ()) sample
            v `shouldBe` DenyBusy (Just (2, 40))

    describe "busyDenyJson" $ do
        it "names the locking cell and elapsed time" $ do
            let v = busyDenyJson (Just (7, 1200))
            field "cellId" v `shouldBe` Just (Number 7)
            field "elapsedMs" v `shouldBe` Just (Number 1200)
            field "busy" v `shouldBe` Just (Bool True)
            -- Distinguished from the own-write bounce (R6.4).
            field "cause" v `shouldBe` Just (String "other-run")

        it "labels the cause when the holder is unknown" $ do
            let v = busyDenyJson Nothing
            field "busy" v `shouldBe` Just (Bool True)
            field "cause" v `shouldBe` Just (String "other-run")

    describe "settle-then-immediate-write (wiring at executeTool)" $ do
        it "never bounces busy inside the window, across release tails" $
            withAckEnv $
                forM_ [0 :: Int, 1, 3] $ \tailSamples -> do
                    (app, store, busyRef) <- mkScriptedFixture
                    let rn = fastRn app
                    _ <- insertSrc app store rn "wOne = (1 :: Int)"
                    _ <- callTool app store rn "await_idle" (object [])
                    -- The release tail: sbBusy stays up for a few samples
                    -- after the settle, then drops — the raced interleaving.
                    remaining <- newIORef tailSamples
                    writeIORef busyRef $ do
                        n <- readIORef remaining
                        modifyIORef' remaining (max 0 . subtract 1)
                        pure (n > 0)
                    out <-
                        toolOutcomeValue
                            <$> callTool
                                app
                                store
                                rn
                                "insert_cell"
                                (object ["source" .= ("wTwo = (2 :: Int)" :: Text)])
                    case field "busy" out of
                        Just (Bool True) ->
                            expectationFailure
                                ( "busy denial inside the window (tail "
                                    <> show tailSamples
                                    <> "): "
                                    <> show out
                                )
                        _ -> pure ()

        it "a genuine busy (new generation) denies naming the cause" $
            withAckEnv $ do
                (app, store, busyRef) <- mkScriptedFixture
                let rn = fastRn app
                _ <- insertSrc app store rn "wThree = (3 :: Int)"
                _ <- callTool app store rn "await_idle" (object [])
                writeIORef busyRef (pure True)
                _ <- bumpGeneration (appEvents app)
                out <-
                    toolOutcomeValue
                        <$> callTool
                            app
                            store
                            rn
                            "check_type"
                            (object ["expr" .= ("wThree" :: Text)])
                field "busy" out `shouldBe` Just (Bool True)
                field "cause" out `shouldBe` Just (String "other-run")

    describe "admission-gate deny (wiring)" $
        it "a held AI gate denies naming the holder cell and elapsed" $ do
            (_, store) <- mkFixture
            heldGate <- newEmptyMVar
            released <- newEmptyMVar
            holding <- newEmptyMVar
            void . forkIO $ do
                r <- AIStore.admitKernel store 7 $ do
                    putMVar holding ()
                    takeMVar heldGate
                case r of
                    Ran () -> putMVar released ()
                    Busy{} -> pure ()
            takeMVar holding
            threadDelay 30000
            bounced <- AIStore.admitKernel store 9 (pure ())
            case bounced of
                Ran () -> expectationFailure "second caller should bounce"
                Busy cid ms -> do
                    cid `shouldBe` 7
                    (ms >= 30) `shouldBe` True
                    let v = busyDenyJson (Just (cid, ms))
                    field "cellId" v `shouldBe` Just (Number 7)
            putMVar heldGate ()
            takeMVar released
