{-# LANGUAGE OverloadedStrings #-}

{- | R7-T3 (R5.8/R9.7): a card is useful-or-absent — emitted iff non-empty
body AND verified names AND descriptive framing; the run-181440 empty shell
and the "use ONLY these names" framing are both unrepresentable.
-}
module Test.CardGateSpec (cardGateSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Grammar.Card (cardHasBody, cardSigNames, emittableCard)
import Sabela.AI.Grammar.Synth (exclusivityViolations)
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover (GrammarMode (..), runDiscoverOutcomes)
import Test.DiscoverFixtures (textField)

header :: Text
header = "## Live API grammar (synthesised from :browse)\n\n### Granite.Svg\n"

goodBody :: Text
goodBody = "  bars :: [(Text, Double)] -> Plot -> Text\n"

exclusiveTail :: Text
exclusiveTail = "Use ONLY these names; nothing else is available.\n"

-- | The (body x verified x framing) emission grid.
cardGrid :: [(Text, Text, Bool, Text, Bool)]
cardGrid =
    [ ( T.intercalate "/" [bodyL, verifiedL, framingL]
      , header <> body <> framing
      , verified
      , "bars"
      , body == goodBody && verified && T.null framing
      )
    | (bodyL, body) <- [("empty", ""), ("nonempty", goodBody)]
    , (verifiedL, verified) <- [("verified", True), ("unverified", False)]
    , (framingL, framing) <- [("descriptive", ""), ("exclusive", exclusiveTail)]
    ]

depsSrc :: Text
depsSrc = "-- cabal: build-depends: granite\nimport Granite.Svg\nbars = 1"

insertCall :: ToolCall
insertCall = ToolCall "insert_cell" (object ["source" .= depsSrc])

failedOutcome :: ToolOutcome
failedOutcome =
    ToolOk
        ( object
            [ "cellId" .= (1 :: Int)
            , "execution"
                .= object
                    [ "ok" .= False
                    , "error" .= ("Variable not in scope: bars" :: Text)
                    ]
            ]
        )

browseWith :: Text -> ToolCall -> IO (Either Text ToolOutcome)
browseWith t _ = pure (Right (ToolOk (object ["exports" .= [t]])))

contentOf :: [Value] -> Text
contentOf = T.concat . map (textField "content")

cardGateSpec :: Spec
cardGateSpec = describe "cards are useful-or-absent, verified, descriptive" $ do
    describe "emittableCard over the (body x verified x framing) grid" $
        it "emits iff non-empty AND all names verified AND descriptive" $
            forM_ cardGrid $ \(label, card, verified, name, expected) -> do
                let ok n = verified && n == name
                (label, emittableCard ok card) `shouldBe` (label, expected)

    describe "card readers" $ do
        it "an empty-shell card has no body" $
            cardHasBody (header <> exclusiveTail) `shouldBe` False
        it "signature names parse back qualifier-free" $
            cardSigNames (header <> "  G.bars :: [(Text, Double)] -> Plot\n")
                `shouldBe` ["bars"]
        it "exclusive framing is a violation the gate rejects" $
            exclusivityViolations (header <> goodBody <> exclusiveTail)
                `shouldSatisfy` (not . null)

    describe "the run-181440 empty-shell fixture (red-then-green)" $ do
        it "a browse with no value bindings emits NO card at all" $ do
            out <-
                runDiscoverOutcomes
                    GrammarOn
                    (browseWith "-- helpers only, no exports")
                    [(insertCall, failedOutcome)]
            out `shouldBe` []
        it "a real browsed export still emits its card (regression floor)" $ do
            out <-
                runDiscoverOutcomes
                    GrammarOn
                    (browseWith "bars :: [(Text, Double)] -> Plot -> Text")
                    [(insertCall, failedOutcome)]
            length out `shouldBe` 1
            contentOf out `shouldSatisfy` T.isInfixOf "bars ::"
