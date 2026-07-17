{-# LANGUAGE OverloadedStrings #-}

{- | Round-trip and law tests for the typed 'CellResult' carving (R2-2,
@docs/siza-redesign.md@ §1.2). These drive the REAL encoders
('Data.Aeson.toJSON' / 'fromJSON' of 'CellResult' and 'CellOutcome') and
the REAL pure mapper 'toCellResult', fed the actual @executeCell@ @Left@
strings exported from "Sabela.AI.Capabilities.Edit.Run" — closing the
run-1 gap where the wire was pinned against hand-reconstructed Aeson.
-}
module Test.CellResultWireSpec (spec) where

import Data.Aeson (FromJSON, ToJSON, Value (..), fromJSON, toJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Result (..))
import Data.List (sort)
import Data.Maybe (isNothing)
import Data.Text (Text)

import Sabela.AI.Capabilities.Edit.Run (
    abortCancelled,
    abortSuperseded,
    abortTimedOut,
 )
import Sabela.AI.CellResult
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Model (CellError (..), MimeType (..), OutputItem (..))
import Test.Hspec

-- | Round-trip a value through the real ToJSON/FromJSON pair.
roundTrips :: (Eq a, FromJSON a, Show a, ToJSON a) => a -> Expectation
roundTrips x = case fromJSON (toJSON x) of
    Success y -> y `shouldBe` x
    Error e -> expectationFailure ("fromJSON failed: " <> e)

objectKeys :: Value -> [Text]
objectKeys (Object o) = map Key.toText (KM.keys o)
objectKeys _ = []

tagOf :: Value -> Maybe Value
tagOf (Object o) = KM.lookup "tag" o
tagOf _ = Nothing

outcomeTag :: CellResult -> Maybe Value
outcomeTag cr = case toJSON cr of
    Object o -> KM.lookup "outcome" o >>= tagOf
    _ -> Nothing

sampleOutput :: OutputItem
sampleOutput = OutputItem MimePlain "hi"

cerr :: CellError
cerr = CellError (Just 1) (Just 2) "boom"

-- | The legacy ok-derivation, reproduced to assert the law against it.
legacyOk :: ExecutionResult -> Bool
legacyOk er = null (erErrors er) && isNothing (erError er)

spec :: Spec
spec = describe "CellResult typed carving (R2-2)" $ do
    describe "JSON round-trips through the real encoders" $ do
        it "round-trips every outcome tag" $ do
            roundTrips Succeeded
            roundTrips (Raised "kaboom")
            roundTrips (Rejected [cerr])
            roundTrips (Aborted Interrupted)
            roundTrips (Aborted Superseded)
            roundTrips (Aborted TimedOut)
        it "round-trips a full CellResult" $
            roundTrips
                ( CellResult
                    (Rejected [cerr])
                    [sampleOutput]
                    []
                )

    describe "the tagged outcome wire shape" $ do
        it "CellResult emits outcome/outputs/warnings/ok" $
            sort (objectKeys (toJSON (CellResult Succeeded [] [])))
                `shouldBe` ["ok", "outcome", "outputs", "warnings"]
        it "the outcome carries a tag discriminator" $
            tagOf (toJSON Succeeded) `shouldBe` Just (String "Succeeded")
        it "Raised tags as Raised" $
            tagOf (toJSON (Raised "x")) `shouldBe` Just (String "Raised")
        it "Rejected tags as Rejected" $
            tagOf (toJSON (Rejected [cerr]))
                `shouldBe` Just (String "Rejected")
        it "Aborted tags as Aborted" $
            tagOf (toJSON (Aborted TimedOut))
                `shouldBe` Just (String "Aborted")

    describe "the ok == Succeeded law" $ do
        it "ok is true exactly for Succeeded" $ do
            okCellResult (CellResult Succeeded [sampleOutput] []) `shouldBe` True
            okCellResult (CellResult (Raised "e") [] []) `shouldBe` False
            okCellResult (CellResult (Rejected [cerr]) [] []) `shouldBe` False
            okCellResult (CellResult (Aborted Interrupted) [] []) `shouldBe` False
        it "the wire ok field agrees with okCellResult" $ do
            let wireOk cr = case toJSON cr of
                    Object o -> KM.lookup "ok" o
                    _ -> Nothing
            wireOk (CellResult Succeeded [] []) `shouldBe` Just (Bool True)
            wireOk (CellResult (Raised "e") [] []) `shouldBe` Just (Bool False)

    describe "notebookHealthy = all okCellResult" $ do
        let ok = CellResult Succeeded [sampleOutput] []
            bad = CellResult (Rejected [cerr]) [] []
        it "all Succeeded => True" $
            notebookHealthy [ok, ok, ok] `shouldBe` True
        it "any non-Succeeded => False" $
            notebookHealthy [ok, bad, ok] `shouldBe` False
        it "empty => True" $
            notebookHealthy [] `shouldBe` True

    describe "toCellResult drives the legacy ok agreement" $ do
        it "okCellResult iff legacy (null errs && isNothing error)" $ do
            let ers =
                    [ ExecutionResult [sampleOutput] Nothing [] []
                    , ExecutionResult [] (Just "e") [] []
                    , ExecutionResult [] Nothing [cerr] []
                    , ExecutionResult [] (Just "e") [cerr] []
                    ]
            mapM_
                ( \er ->
                    okCellResult (toCellResult (Right er) (erOutputs er))
                        `shouldBe` legacyOk er
                )
                ers
        it "Right with error text maps to Raised" $
            outcomeTag (toCellResult (Right (ExecutionResult [] (Just "e") [] [])) [])
                `shouldBe` Just (String "Raised")
        it "Right with structured errors maps to Rejected" $
            outcomeTag (toCellResult (Right (ExecutionResult [] Nothing [cerr] [])) [])
                `shouldBe` Just (String "Rejected")
        it "Right clean maps to Succeeded" $
            outcomeTag (toCellResult (Right (ExecutionResult [] Nothing [] [])) [])
                `shouldBe` Just (String "Succeeded")
        it "warnings pass through to crWarnings, orthogonal to the outcome" $
            crWarnings (toCellResult (Right (ExecutionResult [] Nothing [] [cerr])) [])
                `shouldBe` [cerr]
        it "outputs pass through orthogonally to the outcome" $
            crOutputs
                ( toCellResult
                    (Right (ExecutionResult [sampleOutput] (Just "e") [] []))
                    [sampleOutput]
                )
                `shouldBe` [sampleOutput]

    describe "the real executeCell Left strings map to AbortReason" $ do
        it "abortCancelled -> Aborted Interrupted" $
            toCellResult (Left abortCancelled) []
                `shouldBe` CellResult (Aborted Interrupted) [] []
        it "abortSuperseded -> Aborted Superseded" $
            toCellResult (Left abortSuperseded) []
                `shouldBe` CellResult (Aborted Superseded) [] []
        it "abortTimedOut -> Aborted TimedOut" $
            toCellResult (Left abortTimedOut) []
                `shouldBe` CellResult (Aborted TimedOut) [] []
        it "the abort wire reason matches the constructor" $ do
            let reasonOf cr = case toJSON cr of
                    Object o -> case KM.lookup "outcome" o of
                        Just (Object oc) -> KM.lookup "reason" oc
                        _ -> Nothing
                    _ -> Nothing
            reasonOf (toCellResult (Left abortTimedOut) [])
                `shouldBe` Just (String "TimedOut")
