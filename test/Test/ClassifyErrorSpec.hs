{-# LANGUAGE OverloadedStrings #-}

module Test.ClassifyErrorSpec (spec) where

import qualified Data.Text as T
import Sabela.Errors (parseErrors)
import Sabela.Handlers.Exec (classifyError, locateError)
import Sabela.Model (CellError (..), bareCellError)
import Test.Hspec

classify :: T.Text -> Maybe T.Text
classify rawErr = classifyError (parseErrors rawErr) rawErr

spec :: Spec
spec = describe "classifyError" $ do
    it "empty stderr is no error" $
        classify "" `shouldBe` Nothing

    it "a lone macOS linker warning is no error" $
        classify "ld: warning: -keep_dwarf_unwind is obsolete\n"
            `shouldBe` Nothing

    it "multiple linker warnings are no error" $
        classify
            ( "ld: warning: -keep_dwarf_unwind is obsolete\n"
                <> "ld: warning: ignoring duplicate libraries: '-lm'\n"
            )
            `shouldBe` Nothing

    it "a GHC warning is no error, so the gate cannot refuse a valid cell" $
        classify
            ( "<interactive>:577:15: warning: [GHC-63394] [-Wx-partial]\n"
                <> "    In the use of \8216head\8217\n"
                <> "    (imported from Prelude): \"This is a partial function, it\n"
                <> "    throws an error on empty lists.\"\n"
            )
            `shouldBe` Nothing

    it "a warning next to a real error keeps the error" $
        case classify
            ( "<interactive>:1:1: warning: [GHC-63394] [-Wx-partial]\n"
                <> "    In the use of \8216head\8217\n"
                <> "<interactive>:2:1: error: [GHC-88464]\n"
                <> "    Variable not in scope: zorp\n"
            ) of
            Nothing -> expectationFailure "expected the error to survive"
            Just msg -> msg `shouldSatisfy` T.isInfixOf "zorp"

    it "a real error survives, with the linker noise dropped" $ do
        let err =
                "ld: warning: -keep_dwarf_unwind is obsolete\n"
                    <> "<interactive>:1:1: error: [GHC-88464]\n"
                    <> "    Variable not in scope: zorp\n"
        case classify err of
            Nothing -> expectationFailure "expected an error"
            Just msg -> do
                msg `shouldSatisfy` T.isInfixOf "zorp"
                msg `shouldSatisfy` (not . T.isInfixOf "ld: warning:")

    it "ordinary runtime stderr is still an error" $
        classify "boom: user exception\n"
            `shouldBe` Just "boom: user exception"

    describe "locateError prefixes a diagnostic with cell + line" $ do
        it "names the cell and the cell-relative line" $
            locateError 12 (bareCellError (Just 1) (Just 1) "Variable not in scope: doule")
                `shouldBe` "cell 12, line 1: Variable not in scope: doule"

        it "omits the line when the diagnostic has no location" $
            locateError 3 (bareCellError Nothing Nothing "Could not find module `X'")
                `shouldBe` "cell 3: Could not find module `X'"
