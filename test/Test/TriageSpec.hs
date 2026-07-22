{-# LANGUAGE OverloadedStrings #-}

{- | Intention specs for ERROR TRIAGE at the model-facing chokepoint. Context
is the scarcest weak-model resource: a raw GHC blob repeats hidden-package
advice our own prompt forbids and lists a dozen knock-on casualties of one
root cause. The model must see root causes, once each, with knock-ons
summarized — never the raw dump.

Proposed API (Sabela.AI.Triage):

  triageErrorText :: Text -> Text -> Text   -- cell source -> blob -> triaged
-}
module Test.TriageSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Triage (triageErrorText)

-- | The failing cell's shape (defines the knock-on names).
cellSrc :: Text
cellSrc =
    T.unlines
        [ "import Text.Megaparsec"
        , "type Parser = Parsec Void String"
        , "numberParser :: Parser Double"
        , "numberParser = lexeme (takeWhile1 p)"
        , "parseExpr = chainl1 parseTerm opAdd"
        , "parseTerm = chainl1 parseFactor opMul"
        , "parseFactor = choice [parens parseExpr, numberParser]"
        , "parens p = between (symbol \"(\") (symbol \")\") p"
        ]

-- | Abridged verbatim turn-6 gate blob: 2 root causes, duplicated site, knock-ons.
gateBlob :: Text
gateBlob =
    T.intercalate
        "\n"
        [ "cell 0, line 4: Variable not in scope:"
        , "  takeWhile1"
        , "    :: (Char -> Bool)"
        , "       -> ParsecT Void String Identity String"
        , "Perhaps use one of these:"
        , "  `takeWhileP' (imported from Text.Megaparsec)"
        , "cell 0, line 9: Variable not in scope:"
        , "  takeWhile1"
        , "    :: (Char -> Bool)"
        , "       -> ParsecT Void String Identity String"
        , "Perhaps use one of these:"
        , "  `takeWhileP' (imported from Text.Megaparsec)"
        , "cell 0, line 5: Variable not in scope: chainl1 :: t0 -> t1 -> Parser Double"
        , "cell 0, line 5: Variable not in scope: parseTerm"
        , "cell 0, line 6: Variable not in scope: parseFactor"
        , "cell 0, line 7: Variable not in scope: parens"
        , "cell 0, line 7: Variable not in scope: parseExpr"
        , "cell 0, line 7: Variable not in scope: numberParser"
        ]

-- | Abridged verbatim turn-13 hidden-package blob (advice the prompt forbids).
hiddenBlob :: Text
hiddenBlob =
    T.intercalate
        "\n"
        [ "cell 0: Could not load module `Text.Megaparsec'."
        , "It is a member of the hidden package `megaparsec-9.8.1'."
        , "You can run `:set -package megaparsec' to expose it."
        , "(Note: this unloads all the modules in the current scope.)"
        , "It is a member of the hidden package `megaparsec-9.8.0'."
        , "You can run `:set -package megaparsec' to expose it."
        , "(Note: this unloads all the modules in the current scope.)"
        , "cell 0: Could not load module `Text.Megaparsec.Char'."
        , "It is a member of the hidden package `megaparsec-9.8.1'."
        , "You can run `:set -package megaparsec' to expose it."
        , "(Note: this unloads all the modules in the current scope.)"
        ]

spec :: Spec
spec = describe "model-facing error triage (intention)" $ do
    describe "knock-on suppression — casualties of the cell's own failed defs" $ do
        let triaged = triageErrorText cellSrc gateBlob
        it "keeps the root causes" $ do
            triaged `shouldSatisfy` T.isInfixOf "takeWhile1"
            triaged `shouldSatisfy` T.isInfixOf "chainl1"
        it "suppresses errors for names this cell defines" $ do
            triaged `shouldSatisfy` (not . T.isInfixOf "parseFactor")
            triaged `shouldSatisfy` (not . T.isInfixOf "not in scope: parens")
        it "summarizes what was suppressed" $
            triaged `shouldSatisfy` T.isInfixOf "knock-on"
        it "keeps the did-you-mean hint with the root cause" $
            triaged `shouldSatisfy` T.isInfixOf "takeWhileP"

    describe "dedup — an identical message appears once" $ do
        let triaged = triageErrorText cellSrc gateBlob
        it "collapses the duplicated takeWhile1 diagnostic" $
            T.count "Variable not in scope:\n  takeWhile1" triaged
                `shouldBe` 1
        it "notes the extra sites" $
            triaged `shouldSatisfy` T.isInfixOf "more site"

    describe "hidden-package rewrite — never repeat advice the prompt forbids" $ do
        let triaged = triageErrorText cellSrc hiddenBlob
        it "drops every :set -package suggestion" $
            triaged `shouldSatisfy` (not . T.isInfixOf ":set -package")
        it "steers to the cabal line, naming the package" $
            triaged `shouldSatisfy` T.isInfixOf "-- cabal: build-depends: megaparsec"
        it "shrinks the blob substantially" $
            (T.length triaged < T.length hiddenBlob `div` 2) `shouldBe` True

    describe "generality — the rules key on GHC classes, not library names" $ do
        it "knock-on suppression works for a dataframe-flavoured cell" $ do
            let dfSrc =
                    "import DataFrame\nloadDf = readCsv \"x.csv\"\nrevenue = sumCol loadDf"
                dfBlob =
                    T.intercalate
                        "\n"
                        [ "cell 2, line 2: Variable not in scope: readCsv"
                        , "cell 2, line 3: Variable not in scope: loadDf"
                        ]
                triaged = triageErrorText dfSrc dfBlob
            triaged `shouldSatisfy` T.isInfixOf "readCsv"
            triaged `shouldSatisfy` (not . T.isInfixOf "not in scope: loadDf")
        it "hidden-package rewrite names whatever package GHC names" $
            triageErrorText
                ""
                "cell 1: Could not load module `Data.Aeson'.\nIt is a member of the hidden package `aeson-2.2.3.0'.\nYou can run `:set -package aeson' to expose it."
                `shouldSatisfy` T.isInfixOf "-- cabal: build-depends: aeson"

    describe "passthrough" $
        it "a single clean diagnostic is unchanged" $ do
            let one = "cell 0, line 2: Couldn't match type: Int with: Text"
            triageErrorText cellSrc one `shouldBe` one
