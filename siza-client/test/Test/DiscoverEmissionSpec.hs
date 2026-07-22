{-# LANGUAGE OverloadedStrings #-}

{- | One sanitized module-API surface (R6.10/R3.6/R3.9): module-API bytes
attach ONLY to an install write whose execution FAILED with a diagnostic
implicating the module — never to any executionSucceeded result (including
the deliverable-write-that-also-installs), never to a transport error. Both
arms render through the ONE synthesized-card path (the raw
"Discovered API" banner is deleted), bounded by the envelope budget.
-}
module Test.DiscoverEmissionSpec (discoverEmissionSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover (
    GrammarMode (..),
    declaresDepsCall,
    runDiscoverOutcomes,
    seamDiscover,
 )
import Siza.Agent.Discover.Envelope (envelopeCharBudget)
import Test.DiscoverFixtures (textField)

depsSrc :: Text
depsSrc = "-- cabal: build-depends: granite\nimport Granite.Svg\nbars = 1"

plainSrc :: Text
plainSrc = "revenueTotal = 42"

insertCall :: Text -> ToolCall
insertCall src = ToolCall "insert_cell" (object ["source" .= src])

okOutcome :: ToolOutcome
okOutcome =
    ToolOk
        (object ["cellId" .= (1 :: Int), "execution" .= object ["ok" .= True]])

failedWith :: Text -> ToolOutcome
failedWith err =
    ToolOk
        ( object
            [ "cellId" .= (1 :: Int)
            , "execution" .= object ["ok" .= False, "error" .= err]
            ]
        )

failedOutcome :: ToolOutcome
failedOutcome = failedWith "Variable not in scope: bars"

-- | A browse dispatch answering every module query with the given text.
browseWith :: Text -> ToolCall -> IO (Either Text ToolOutcome)
browseWith t _ = pure (Right (ToolOk (object ["exports" .= [t]])))

browse :: ToolCall -> IO (Either Text ToolOutcome)
browse = browseWith "bars :: [(Text, Double)] -> Plot -> Text"

contentOf :: [Value] -> Text
contentOf = T.concat . map (textField "content")

discoverEmissionSpec :: Spec
discoverEmissionSpec = describe "discovery emission gating (R6.10)" $ do
    describe "declaresDepsCall" $ do
        it "recognises a dep-declaring write" $
            declaresDepsCall (insertCall depsSrc) `shouldBe` True
        it "a plain deliverable write declares nothing" $
            declaresDepsCall (insertCall plainSrc) `shouldBe` False

    describe "zero module-API bytes on any executionSucceeded result" $ do
        it "a successful dep-declaring write emits NOTHING (the R6.10 hole)" $
            forM_ [GrammarOff, GrammarOn] $ \mode -> do
                out <-
                    runDiscoverOutcomes mode browse [(insertCall depsSrc, okOutcome)]
                out `shouldBe` []
        it "a successful deliverable write emits nothing (60k-dump class)" $
            forM_ [GrammarOff, GrammarOn] $ \mode -> do
                out <-
                    runDiscoverOutcomes mode browse [(insertCall plainSrc, okOutcome)]
                out `shouldBe` []

    describe "nothing landed, nothing browsed (phantom-banner class)" $ do
        it "a transport-errored call gets no card" $ do
            out <-
                runDiscoverOutcomes
                    GrammarOff
                    browse
                    [(insertCall depsSrc, ToolErr (object ["error" .= ("boom" :: Text)]))]
            out `shouldBe` []
        it "a result with no execution report gets no card" $ do
            out <-
                runDiscoverOutcomes
                    GrammarOff
                    browse
                    [(insertCall depsSrc, ToolOk (object ["cellId" .= (1 :: Int)]))]
            out `shouldBe` []

    describe "a red install write with an implicating diagnostic gets the card" $ do
        it "not-in-scope after an install renders the module card" $ do
            out <-
                runDiscoverOutcomes GrammarOn browse [(insertCall depsSrc, failedOutcome)]
            length out `shouldBe` 1
            contentOf out `shouldSatisfy` T.isInfixOf "Granite.Svg"
        it "the card carries the full signature of the browsed export" $ do
            out <-
                runDiscoverOutcomes GrammarOn browse [(insertCall depsSrc, failedOutcome)]
            contentOf out
                `shouldSatisfy` T.isInfixOf "bars :: [(Text, Double)] -> Plot -> Text"
        it "a non-implicating diagnostic (instance error) emits nothing" $ do
            out <-
                runDiscoverOutcomes
                    GrammarOn
                    browse
                    [(insertCall depsSrc, failedWith "No instance for (Show Plot)")]
            out `shouldBe` []

    describe "one envelope shape: both arms render the synthesized card" $ do
        it "GrammarOff renders the same card shape as GrammarOn" $ do
            offOut <-
                runDiscoverOutcomes GrammarOff browse [(insertCall depsSrc, failedOutcome)]
            onOut <-
                runDiscoverOutcomes GrammarOn browse [(insertCall depsSrc, failedOutcome)]
            contentOf offOut `shouldBe` contentOf onOut
            contentOf offOut `shouldSatisfy` T.isInfixOf "Live API grammar"
        it "the raw 'Discovered API' banner path is gone" $
            forM_ [GrammarOff, GrammarOn] $ \mode -> do
                out <-
                    runDiscoverOutcomes mode browse [(insertCall depsSrc, failedOutcome)]
                contentOf out
                    `shouldSatisfy` (not . T.isInfixOf "Discovered API of newly installed")

    describe "the card is bounded by the envelope budget" $
        it "caps a runaway browse dump with reconciling counts" $ do
            let sigs =
                    T.intercalate
                        "\n"
                        [ "fn" <> T.pack (show i) <> " :: [(Text, Double)] -> Plot -> Text"
                        | i <- [1 .. 300 :: Int]
                        ]
            out <-
                runDiscoverOutcomes
                    GrammarOff
                    (browseWith sigs)
                    [(insertCall depsSrc, failedOutcome)]
            T.length (contentOf out)
                `shouldSatisfy` (<= envelopeCharBudget + 200)

    describe "R7.4/R6.10 emission gate over the FULL grid" $
        it "content attaches ONLY to a red implicated install write" $
            forM_ emissionGrid $ \(label, mode, call, outcome, expected) -> do
                out <- runDiscoverOutcomes mode browse [(call, outcome)]
                (label, not (null out)) `shouldBe` (label, expected)

    describe "seam re-browse fires only on an unresolved-symbol failure" $ do
        it "emits for not-in-scope and module-load diagnostics only" $
            forM_ seamGrid $ \(err, expected) -> do
                out <- seamDiscover GrammarOn browse [(depsSrc, err)]
                (err, not (null out)) `shouldBe` (err, expected)
        it "GrammarOff never seam-emits" $
            forM_ seamGrid $ \(err, _) -> do
                out <- seamDiscover GrammarOff browse [(depsSrc, err)]
                out `shouldBe` []

    describe "exclusivity-language lint over every synthesized card (R9.7)" $ do
        it "the install-failure card carries no exclusivity language" $ do
            out <-
                runDiscoverOutcomes GrammarOn browse [(insertCall depsSrc, failedOutcome)]
            forM_ bannedExclusivity $ \p ->
                (p, p `T.isInfixOf` T.toLower (contentOf out))
                    `shouldBe` (p, False)
        it "the seam card carries no exclusivity language" $ do
            out <- seamDiscover GrammarOn browse [(depsSrc, "Variable not in scope: bars")]
            forM_ bannedExclusivity $ \p ->
                (p, p `T.isInfixOf` T.toLower (contentOf out))
                    `shouldBe` (p, False)

-- | Exclusivity phrases banned from any synthesized card (search-api.md §6).
bannedExclusivity :: [Text]
bannedExclusivity =
    ["use only these", "only these names", "nothing else", "no other names"]

{- | The full (call class x outcome class x mode) grid: content attaches only
to the install write whose execution failed implicating the module — every
executionSucceeded cell of the grid is zero-byte in both arms.
-}
emissionGrid :: [(Text, GrammarMode, ToolCall, ToolOutcome, Bool)]
emissionGrid =
    [ ( T.intercalate "/" [callLabel, outcomeLabel, modeLabel]
      , mode
      , call
      , outcome
      , callInstalls && outcomeImplicates
      )
    | (callLabel, call, callInstalls) <- callClasses
    , (outcomeLabel, outcome, outcomeImplicates) <- outcomeClasses
    , (modeLabel, mode) <- [("off", GrammarOff), ("on", GrammarOn)]
    ]

callClasses :: [(Text, ToolCall, Bool)]
callClasses =
    [ ("deps-insert", insertCall depsSrc, True)
    , ("plain-insert", insertCall plainSrc, False)
    , ("deps-replace", replaceCall depsSrc, True)
    , ("plain-replace", replaceCall plainSrc, False)
    ,
        ( "non-owning"
        , ToolCall "execute_cell" (object ["cell_id" .= (1 :: Int)])
        , False
        )
    ]
  where
    replaceCall src =
        ToolCall
            "replace_cell_source"
            (object ["cell_id" .= (1 :: Int), "new_source" .= src])

outcomeClasses :: [(Text, ToolOutcome, Bool)]
outcomeClasses =
    [ ("exec-ok", okOutcome, False)
    , ("exec-failed-implicating", failedOutcome, True)
    , ("exec-failed-other", failedWith "No instance for (Show Plot)", False)
    , ("tool-error", ToolErr (object ["error" .= ("boom" :: Text)]), False)
    , ("no-execution-report", ToolOk (object ["cellId" .= (1 :: Int)]), False)
    ]

{- | Compile-state grid for the seam: §9.1 trigger 2 is an unresolved
symbol/module diagnostic — instance/type/parse errors implicate usage of
known names, not the name surface, and a clean cell has no trigger at all.
-}
seamGrid :: [(Text, Bool)]
seamGrid =
    [ ("Variable not in scope: bars", True)
    , ("Could not load module `Granite.Svg'", True)
    , ("No instance for (Show Plot)", False)
    , ("parse error on input `='", False)
    , ("Couldn't match type `Int' with `Text'", False)
    , ("", False)
    ]
