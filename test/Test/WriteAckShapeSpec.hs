{-# LANGUAGE OverloadedStrings #-}

{- | The R3.6 general invariant: the write-ack envelope decodes against ONE
declared shape ('Sabela.AI.WriteAck.parseAckEnvelope') across the full
outcome-class grid — queued / executing / completed-ok / completed-error /
busy — generated over outcome combinations, with serialisation-in-string
execution fields rejected outright. The same decoder is then held against
the REAL @executeTool@ producers, so the declared shape cannot drift from
what the server actually emits.
-}
module Test.WriteAckShapeSpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar)
import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import Data.Maybe (isJust)
import Data.Text (Text)
import Test.Hspec

import Sabela.AI.Types (toolOutcomeIsError, toolOutcomeValue)
import Sabela.AI.WriteAck (
    AckEnvelope (..),
    AckStatus (..),
    BusyAck (..),
    WriteAck (..),
    busyAckJson,
    parseAckEnvelope,
    writeAckJson,
 )
import Test.WriteAckFixture (
    callTool,
    fastRn,
    insertSrc,
    mkFixture,
    slowRn,
    withAckEnv,
 )

{- | The generated outcome-class grid: every status with its execution class,
crossed with the duplicate flag and note presence.
-}
writeGrid :: [WriteAck]
writeGrid =
    [ WriteAck cid status hash execution dup note
    | (status, execution) <-
        [ (AckQueued, Nothing)
        , (AckExecuting, Nothing)
        , (AckCompleted, Just (object ["ok" .= True]))
        , (AckCompleted, Just (object ["ok" .= False, "outcome" .= err]))
        , (AckCompleted, Just Null)
        ]
    , cid <- [0, 7]
    , hash <- [Nothing, Just "p123"]
    , dup <- [False, True]
    , note <- [Nothing, Just "the write landed"]
    ]
  where
    err = "Raised" :: Text

busyGrid :: [BusyAck]
busyGrid = [BusyAck cid ms | cid <- [0, 3], ms <- [0, 120000]]

spec :: Spec
spec = describe "write-ack declared shape (R3.6)" $ do
    it "every write outcome class round-trips through the one declared shape" $
        forM_ writeGrid $ \wa ->
            parseAckEnvelope (writeAckJson wa) `shouldBe` Just (EnvWrite wa)

    it "every busy outcome round-trips through the same decoder" $
        forM_ busyGrid $ \ba ->
            parseAckEnvelope (busyAckJson ba) `shouldBe` Just (EnvBusy ba)

    it "a serialised-in-string execution field fails the decode outright" $
        forM_ writeGrid $ \wa -> do
            let smuggled =
                    writeAckJson
                        wa{waExecution = Just (String "{\"ok\":true}")}
            parseAckEnvelope smuggled `shouldBe` Nothing

    around_ withAckEnv $
        it "the real executeTool producers emit only the declared shape" $ do
            (app, store) <- mkFixture
            let rn = fastRn app
            -- Completed inline (fast write).
            done <- insertSrc app store rn "x = 1"
            case parseAckEnvelope done of
                Just (EnvWrite wa) -> waStatus wa `shouldBe` AckCompleted
                other -> expectationFailure ("undecodable ack: " <> show other)
            -- Executing ack, then its duplicate, then the own-write bounce.
            (app2, store2) <- mkFixture
            barrier <- newEmptyMVar
            let rn2 = slowRn app2 barrier
            ack <- insertSrc app2 store2 rn2 "y = 2"
            case parseAckEnvelope ack of
                Just (EnvWrite wa) -> waStatus wa `shouldBe` AckExecuting
                other -> expectationFailure ("undecodable ack: " <> show other)
            dup <- insertSrc app2 store2 rn2 "y = 2"
            case parseAckEnvelope dup of
                Just (EnvWrite wa) -> waDuplicate wa `shouldBe` True
                other -> expectationFailure ("undecodable dup: " <> show other)
            busy <-
                callTool app2 store2 rn2 "execute_cell" (object ["cell_id" .= (0 :: Int)])
            toolOutcomeIsError busy `shouldBe` True
            case parseAckEnvelope (toolOutcomeValue busy) of
                Just (EnvBusy ba) -> baCellId ba `shouldBe` 0
                other -> expectationFailure ("undecodable busy: " <> show other)
            (isJust (parseAckEnvelope ack), isJust (parseAckEnvelope dup))
                `shouldBe` (True, True)
            putMVar barrier ()
