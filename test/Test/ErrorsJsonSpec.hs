{-# LANGUAGE OverloadedStrings #-}

module Test.ErrorsJsonSpec (errorsJsonSpec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.Errors.Json (
    annotateDefSites,
    parseJsonCompiled,
    parseJsonInteractive,
    quotedNames,
 )
import Sabela.Model (CellError (..))

-- Real GHC 9.12 `-fdiagnostics-as-json` lines (captured from ghci).
errLine :: Text
errLine =
    "{\"version\":\"1.1\",\"ghcVersion\":\"ghc-9.12.2\",\"span\":{\"file\":\"sabela-cell-7\",\"start\":{\"line\":2,\"column\":7},\"end\":{\"line\":2,\"column\":17}},\"severity\":\"Error\",\"code\":83865,\"message\":[\"Couldn't match type `[Char]' with `Int'\",\"In the expression: \\\"x\\\"\"],\"hints\":[]}"

warnLine :: Text
warnLine =
    "{\"version\":\"1.1\",\"ghcVersion\":\"ghc-9.12.2\",\"span\":{\"file\":\"sabela-cell-7\",\"start\":{\"line\":1,\"column\":3},\"end\":{\"line\":1,\"column\":4}},\"severity\":\"Warning\",\"code\":40910,\"message\":[\"Defined but not used: `x'\"],\"hints\":[],\"reason\":{\"flags\":[\"unused-matches\"]}}"

noSpanErr :: Text
noSpanErr =
    "{\"version\":\"1.1\",\"ghcVersion\":\"ghc-9.12.2\",\"span\":null,\"severity\":\"Error\",\"code\":35235,\"message\":[\"Could not find module `Data.NonExistent'.\"],\"hints\":[]}"

hintErr :: Text
hintErr =
    "{\"version\":\"1.1\",\"ghcVersion\":\"ghc-9.12.2\",\"span\":{\"file\":\"<interactive>\",\"start\":{\"line\":2,\"column\":1},\"end\":{\"line\":2,\"column\":8}},\"severity\":\"Error\",\"code\":88464,\"message\":[\"Variable not in scope: filtter\"],\"hints\":[\"Perhaps use `filter' (imported from Prelude)\"]}"

errorsJsonSpec :: Spec
errorsJsonSpec = describe "Sabela.Errors.Json" $ do
    describe "parseJsonInteractive" $ do
        it "extracts an error with its cell-relative line and column" $ do
            let (errs, warns, residual) = parseJsonInteractive errLine
            warns `shouldBe` []
            residual `shouldBe` ""
            case errs of
                [CellError ln col msg] -> do
                    ln `shouldBe` Just 2
                    col `shouldBe` Just 7
                    ("Couldn't match type" `T.isInfixOf` msg) `shouldBe` True
                other -> expectationFailure ("expected one error, got " <> show other)

        it "joins multi-paragraph messages" $ do
            let ([CellError _ _ msg], _, _) = parseJsonInteractive errLine
            ("In the expression" `T.isInfixOf` msg) `shouldBe` True

        it "splits warnings from errors by severity" $ do
            let (errs, warns, _) = parseJsonInteractive warnLine
            errs `shouldBe` []
            case warns of
                [CellError ln _ msg] -> do
                    ln `shouldBe` Just 1
                    ("Defined but not used" `T.isInfixOf` msg) `shouldBe` True
                other -> expectationFailure ("expected one warning, got " <> show other)

        it "keeps a null span as an error with no location" $ do
            let (errs, _, _) = parseJsonInteractive noSpanErr
            case errs of
                [CellError ln col _] -> (ln, col) `shouldBe` (Nothing, Nothing)
                other -> expectationFailure ("expected one error, got " <> show other)

        it "appends GHC hints to the message" $ do
            let ([CellError _ _ msg], _, _) = parseJsonInteractive hintErr
            ("Perhaps use" `T.isInfixOf` msg) `shouldBe` True

        it "returns non-JSON stderr lines as residual, untouched" $ do
            let raw = T.unlines ["runtime boom", errLine, "ld: warning: noise"]
                (errs, _, residual) = parseJsonInteractive raw
            length errs `shouldBe` 1
            ("runtime boom" `T.isInfixOf` residual) `shouldBe` True
            ("ld: warning: noise" `T.isInfixOf` residual) `shouldBe` True
            ("severity" `T.isInfixOf` residual) `shouldBe` False

        it "is empty for empty input" $
            parseJsonInteractive "" `shouldBe` ([], [], "")

    describe "annotateDefSites" $ do
        let resolve n = lookup n [("triple", 2), ("double", 2)]
        it "names the cell that defines a `Perhaps use' suggestion" $
            annotateDefSites resolve "Perhaps use `triple' (line 1)"
                `shouldBe` "Perhaps use `triple' (line 1) (defined in cell 2)"

        it "annotates the smart-quote form too" $
            annotateDefSites resolve "Perhaps use \8216triple\8217"
                `shouldBe` "Perhaps use \8216triple\8217 (defined in cell 2)"

        it "leaves a suggestion alone when the name is not a notebook def" $
            annotateDefSites resolve "Perhaps use `filter' (imported from Prelude)"
                `shouldBe` "Perhaps use `filter' (imported from Prelude)"

        it "touches only `Perhaps' lines, not the main message" $
            annotateDefSites resolve "Variable not in scope: `triple'"
                `shouldBe` "Variable not in scope: `triple'"

        it "lists each resolved name when several are suggested" $
            annotateDefSites resolve "Perhaps use one of these: `double', `triple'"
                `shouldBe` "Perhaps use one of these: `double', `triple' (`double' in cell 2, `triple' in cell 2)"

    describe "quotedNames" $
        it "pulls backtick and smart-quoted identifiers" $
            quotedNames "use `foo' or \8216bar\8217" `shouldBe` ["foo", "bar"]

    describe "parseJsonCompiled" $ do
        it "routes errors to their cell via the span file tag" $ do
            let (perCell, loose) = parseJsonCompiled errLine
            loose `shouldBe` []
            M.keys perCell `shouldBe` [7]

        it "puts a null-span error in the loose bucket" $ do
            let (perCell, loose) = parseJsonCompiled noSpanErr
            M.null perCell `shouldBe` True
            length loose `shouldBe` 1

        it "does not count warnings as compile failures" $ do
            let (perCell, loose) = parseJsonCompiled warnLine
            (M.null perCell, loose) `shouldBe` (True, [])
