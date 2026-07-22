{-# LANGUAGE OverloadedStrings #-}

{- | Intention specs for the DIAGNOSTIC verify re-prompt.

Gemma sometimes stops well under budget and gives up on an error. Today the
re-prompt is one fixed sentence (\"the check does not pass yet\") with no
diagnostics — the model repeats itself and the loop ends @stuck@. The re-prompt
must say what is actually missing: no cell written at all, or the requested
deliverable binding absent.

Proposed API:

  requestedNames      :: Text -> [Text]           -- prompt -> deliverable names
  missingDeliverables :: Text -> [Text] -> [Text] -- prompt -> cell sources -> absent
    (Siza.Agent.Deliverable)
  verifyMessage'      :: Int -> [Text] -> Maybe Text -> Text  -- owned/missing/counterexample
    (Siza.Agent.Messages)
-}
module Test.VerifyDiagSpec (verifyDiagSpec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Deliverable (missingDeliverables, requestedNames)
import Siza.Agent.Messages (doneSignal, unconfirmedMessage, verifyMessage')

-- | The real evalExpr gate task prompt (abridged).
taskPrompt :: Text
taskPrompt =
    "Write a recursive-descent parser for arithmetic expressions. Use a \
    \parser-combinator library (megaparsec). Define `evalExpr :: String -> \
    \Maybe Double` which parses and evaluates the expression. Examples that \
    \must hold: evalExpr \"2 + 3 * 4\" == Just 14.0."

verifyDiagSpec :: Spec
verifyDiagSpec = describe "diagnostic verify re-prompt (intention)" $ do
    describe "requestedNames — the deliverables the prompt names" $ do
        it "reads the backtick-quoted `name :: sig` deliverable" $
            requestedNames taskPrompt `shouldBe` ["evalExpr"]
        it "is empty when the prompt names no typed deliverable" $
            requestedNames "make a nice plot of the data" `shouldBe` []

    describe "missingDeliverables — requested but defined in no cell" $ do
        it "reports the deliverable when no cell defines it" $
            missingDeliverables taskPrompt ["pNumber = undefined"]
                `shouldBe` ["evalExpr"]
        it "is satisfied by a defining cell" $
            missingDeliverables
                taskPrompt
                ["evalExpr :: String -> Maybe Double\nevalExpr s = Nothing"]
                `shouldBe` []
        it "is satisfied with no cells only if nothing was requested" $
            missingDeliverables taskPrompt [] `shouldBe` ["evalExpr"]

    describe "verifyMessage' — say what is actually missing" $ do
        it "names the missing deliverable and the tool to define it" $ do
            let msg = verifyMessage' 2 ["evalExpr"] Nothing
            msg `shouldSatisfy` T.isInfixOf "evalExpr"
            msg `shouldSatisfy` T.isInfixOf "not defined"
            msg `shouldSatisfy` T.isInfixOf "insert_cell"
        it "says so when NO cell has been written at all" $ do
            let msg = verifyMessage' 0 ["evalExpr"] Nothing
            msg `shouldSatisfy` T.isInfixOf "no cell"
        it "falls back to wrong-value guidance when everything is defined" $ do
            let msg = verifyMessage' 2 [] Nothing
            msg `shouldSatisfy` T.isInfixOf "check"
            msg `shouldSatisfy` (not . T.isInfixOf "not defined")
        it "always forbids stopping" $
            verifyMessage' 1 [] Nothing `shouldSatisfy` T.isInfixOf "not done"

    describe "unconfirmedMessage — uncheckable is NOT a failure claim (R5-T5)" $ do
        it "never claims failure, over the whole diagnosis grid" $
            mapM_
                ( \(owned, missing, guide) ->
                    T.toLower (unconfirmedMessage owned missing guide)
                        `shouldSatisfy` (not . T.isInfixOf "fail")
                )
                [ (o, m, g)
                | o <- [0, 1, 2]
                , m <- [[], ["evalExpr"]]
                , g <- [Nothing, Just "print the value and re-check"]
                ]
        it "opens with not-yet-confirmed and carries the what-to-run guidance" $ do
            let msg = unconfirmedMessage 2 [] (Just "print the value and re-check")
            msg `shouldSatisfy` T.isInfixOf "not yet confirmed"
            msg `shouldSatisfy` T.isInfixOf "print the value and re-check"
        it "still diagnoses the structurally-evident gaps" $ do
            unconfirmedMessage 0 [] Nothing `shouldSatisfy` T.isInfixOf "no cell"
            unconfirmedMessage 2 ["evalExpr"] Nothing
                `shouldSatisfy` T.isInfixOf "evalExpr"

    describe "doneSignal — the deliverable-green stop line" $ do
        it "says confirmed, asks for a one-line summary and a stop" $ do
            doneSignal `shouldSatisfy` T.isInfixOf "confirmed"
            doneSignal `shouldSatisfy` T.isInfixOf "stop"
        it "is one line and never advises more searching" $ do
            T.count "\n" doneSignal `shouldBe` 0
            T.toLower doneSignal `shouldSatisfy` (not . T.isInfixOf "search")
            T.toLower doneSignal `shouldSatisfy` (not . T.isInfixOf "discover")
