{-# LANGUAGE OverloadedStrings #-}

module Test.FrameQuerySpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Sabela.Server.FrameFilter (columnBindings, columnIdent, filterBlock, renderType)
import Sabela.Server.FrameQuery (frameQueryBlock, queryFunction, validSlot)

spec :: Spec
spec = describe "FrameQuery (a table's expression box)" $ do
    describe "validSlot" $ do
        it "accepts the cell#n key a table carries" $
            validSlot "12#0" `shouldBe` True
        it "rejects anything that is not two numbers" $
            map validSlot ["", "#0", "1#", "a#0", "1#b", "1#0\"; :! rm"]
                `shouldBe` [False, False, False, False, False, False]

    describe "queryFunction" $ do
        it "defaults to the untyped query when the table names none" $
            queryFunction Nothing `shouldBe` Just "_sabelaFrameQuery"
        it "accepts only the harness's own query bindings" $
            map (queryFunction . Just) ["_sabelaDataFrameQuery", "print", "_sabelaQuery x", "System.Exit.exitWith"]
                `shouldBe` [Just "_sabelaDataFrameQuery", Nothing, Nothing, Nothing]

    describe "frameQueryBlock" $ do
        it "wraps a one-line expression as a lambda over df in a :{ :} block" $
            T.lines (frameQueryBlock "_sabelaDataFrameQuery" "3#1" "D.take 5 df")
                `shouldBe` [ ":{"
                           , "_sabelaDataFrameQuery \"3#1\" (\\df ->"
                           , "    D.take 5 df"
                           , "  )"
                           , ":}"
                           ]
        it "keeps a multi-line expression one statement by indenting every line" $
            T.lines (frameQueryBlock "_sabelaFrameQuery" "3#1" "df\n  |> D.take 5\n\n  |> D.sortBy D.Ascending [\"x\"]")
                `shouldBe` [ ":{"
                           , "_sabelaFrameQuery \"3#1\" (\\df ->"
                           , "    df"
                           , "      |> D.take 5"
                           , "      |> D.sortBy D.Ascending [\"x\"]"
                           , "  )"
                           , ":}"
                           ]

    describe "columnIdent (the library's sanitize rule)" $ do
        it "keeps a plain lower-case name" $
            columnIdent "homeplanet" `shouldBe` Just "homeplanet"
        it "lower-cases and underscores an unruly name" $
            columnIdent "Home Planet (raw)" `shouldBe` Just "home_planet_raw"
        it "wraps a keyword or a name that cannot start a variable" $
            map columnIdent ["type", "3rd"] `shouldBe` [Just "_type_", Just "_3rd_"]

    describe "renderType" $ do
        it "qualifies base types and Text through names the session has" $
            renderType "Maybe Text" `shouldBe` Just "(SabelaBase.Maybe SabelaDFText.Text)"
        it "handles a list type" $
            renderType "[Char]" `shouldBe` Just "[Prelude.Char]"
        it "refuses a type with characters outside a type expression" $
            renderType "Double -> IO ()" `shouldBe` Nothing

    describe "columnBindings" $
        it "binds each column once, as a typed column expression" $
            columnBindings [("age", "Double"), ("Age", "Int"), ("city", "")]
                `shouldBe` ["age = SabelaDF.col (SabelaDFText.pack \"age\") :: SabelaDF.Expr Prelude.Double"]

    describe "filterBlock" $
        it "binds the columns and applies the predicate with filterWhere" $
            T.lines (filterBlock "_sabelaDataFrameQuery" "1#0" [("city", "[Char]"), ("temp", "Double")] "temp .>. D.lit 20")
                `shouldBe` [ ":{"
                           , "_sabelaDataFrameQuery \"1#0\" (\\df ->"
                           , "    let city = SabelaDF.col (SabelaDFText.pack \"city\") :: SabelaDF.Expr [Prelude.Char]"
                           , "        temp = SabelaDF.col (SabelaDFText.pack \"temp\") :: SabelaDF.Expr Prelude.Double"
                           , "     in SabelaDF.filterWhere"
                           , "            ("
                           , "            temp .>. D.lit 20"
                           , "            )"
                           , "            df"
                           , "  )"
                           , ":}"
                           ]
