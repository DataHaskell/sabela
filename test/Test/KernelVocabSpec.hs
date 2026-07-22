{-# LANGUAGE OverloadedStrings #-}

{- | The closed kernel-state vocabulary (R1.7/R3.6): every state string any
kernel\/await\/ack surface emits is a member of one documented enum, checked
over the full generated outcome grid of the REAL producers — never a
bench-task fixture — and an out-of-vocabulary state fails the one-shape
validator.
-}
module Test.KernelVocabSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Kernel (awaitTag)
import Sabela.AI.KernelState (kernelStateJSON, kernelStateOf)
import Sabela.AI.KernelVocab (
    inVocabulary,
    outOfVocabStates,
    stateVocabulary,
    vocabularyLine,
 )
import Sabela.AI.WriteAck (
    AckStatus (..),
    BusyAck (..),
    WriteAck (..),
    ackStatusText,
    busyAckJson,
    writeAckJson,
 )
import Sabela.State.EventBus (AwaitResult (..))
import Test.Hspec

-- | Every kernel-state wire object over the full input grid.
kernelStateGrid :: [Value]
kernelStateGrid =
    [ kernelStateJSON (kernelStateOf alive gen busy building)
    | alive <- [False, True]
    , gen <- [0, 1, 7]
    , busy <- [False, True]
    , building <- [False, True]
    ]

-- | Every write-ack wire object over the status × duplicate grid.
writeAckGrid :: [Value]
writeAckGrid =
    [ writeAckJson (WriteAck 3 st Nothing Nothing dup Nothing)
    | st <- [minBound .. maxBound]
    , dup <- [False, True]
    ]

spec :: Spec
spec = describe "closed kernel-state vocabulary (R1.7/R3.6)" $ do
    it "kernel_status emits only vocabulary members over the full grid" $
        concatMap outOfVocabStates kernelStateGrid `shouldBe` []

    it "await_idle waited tags are vocabulary members over the full grid" $ do
        let tags =
                "idle"
                    : map awaitTag [AwaitSettled, AwaitKernelDead, AwaitTimedOut]
        filter (not . inVocabulary) tags `shouldBe` []

    it "await_idle result objects validate over the tag × status grid" $ do
        let objs =
                [ object ["waited" .= tag, "status" .= st]
                | tag <-
                    "idle"
                        : map
                            awaitTag
                            [AwaitSettled, AwaitKernelDead, AwaitTimedOut]
                , st <- kernelStateGrid
                ]
        concatMap outOfVocabStates objs `shouldBe` []

    it "write-ack statuses are vocabulary members over the full grid" $ do
        filter
            (not . inVocabulary)
            (map ackStatusText [minBound .. maxBound])
            `shouldBe` []
        concatMap outOfVocabStates writeAckGrid `shouldBe` []

    it "the own-write busy bounce validates" $
        outOfVocabStates (busyAckJson (BusyAck 2 1500)) `shouldBe` []

    it "an out-of-vocabulary state fails the validator" $ do
        outOfVocabStates (object ["state" .= ("wedged" :: Text)])
            `shouldBe` ["wedged"]
        outOfVocabStates
            (object ["status" .= object ["state" .= ("kernelGone" :: Text)]])
            `shouldBe` ["kernelGone"]
        outOfVocabStates (object ["waited" .= ("crashed" :: Text)])
            `shouldBe` ["crashed"]

    it "the documented enum is exactly the vocabulary (R1.7)" $ do
        stateVocabulary
            `shouldBe` [ "cold"
                       , "idle"
                       , "building"
                       , "executing"
                       , "settled"
                       , "kernelDead"
                       , "timedOut"
                       , "queued"
                       , "completed"
                       ]
        -- The advertised line names every member, so a tool description
        -- built from it can never advertise an undocumented state.
        filter (not . (`textIn` vocabularyLine)) stateVocabulary
            `shouldBe` []

textIn :: Text -> Text -> Bool
textIn needle hay = needle `elem` splitWords hay
  where
    splitWords = concatMap (T.splitOn "|") . T.words . stripPunct
    stripPunct = T.map (\c -> if c `elem` (".,:;" :: String) then ' ' else c)
