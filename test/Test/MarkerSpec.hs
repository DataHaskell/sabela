{-# LANGUAGE OverloadedStrings #-}

module Test.MarkerSpec (spec) where

import Control.Concurrent.STM (atomically)
import qualified Data.Text as T
import Sabela.Session.Drain (DrainResult (..), drainUntilMarker)
import Sabela.Session.Reader (
    enqueueEof,
    enqueueLine,
    markerNonceBase,
    markerNumberIn,
    mkMarkerText,
    newOutQueue,
 )
import Test.Hspec

realMarker :: Int -> Int -> T.Text
realMarker nonce run = mkMarkerText (nonce * markerNonceBase + run)

spec :: Spec
spec = describe "marker forgery resistance (stress case 1)" $ do
    it "does not end a cell early on a forged sentinel line" $ do
        q <- newOutQueue
        let real = realMarker 314159265358 3
            forged = "---SABELA_MARKER_3---"
        atomically $ do
            enqueueLine q 1 "out1"
            enqueueLine q 1 forged
            enqueueLine q 1 "out2"
            enqueueLine q 1 real
            enqueueEof q
        res <- drainUntilMarker q real (\_ -> pure ())
        case res of
            DrainOk out -> do
                out `shouldSatisfy` T.isInfixOf "out1"
                out `shouldSatisfy` T.isInfixOf "out2"
                out `shouldSatisfy` T.isInfixOf forged
            DrainEof _ -> expectationFailure "ended at EOF, not the real marker"

    it "a forged earlier-numbered sentinel does not wipe accumulated output" $ do
        q <- newOutQueue
        let real = realMarker 271828182845 9
        atomically $ do
            enqueueLine q 1 "keepme"
            enqueueLine q 1 "---SABELA_MARKER_1---"
            enqueueLine q 1 real
            enqueueEof q
        res <- drainUntilMarker q real (\_ -> pure ())
        case res of
            DrainOk out -> out `shouldSatisfy` T.isInfixOf "keepme"
            DrainEof _ -> expectationFailure "ended at EOF, not the real marker"

    describe "markerNumberIn parses the first well-formed marker only" $ do
        it "extracts the number despite a garbage prefix" $
            markerNumberIn "garbage ---SABELA_MARKER_100001---"
                `shouldBe` Just 100001

        it "extracts the number with trailing junk after the marker" $
            markerNumberIn "---SABELA_MARKER_99999--- ignored"
                `shouldBe` Just 99999

        it "rejects a non-digit marker tail as ordinary output" $
            markerNumberIn "---SABELA_MARKER_abc---" `shouldBe` Nothing
