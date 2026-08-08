{-# LANGUAGE OverloadedStrings #-}

{- | The parquet footer reader, checked against a real file rather than a
hand-built one: the point of reading the format is that files other tools
wrote are readable.
-}
module Test.ParquetSpec (spec) where

import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Test.Hspec

import Sabela.Parquet (
    ParquetColumn (..),
    ParquetSchema (..),
    footerSlice,
    parquetFooter,
 )
import Sabela.Parquet.Read (isParquetPath, readParquetSchema)

irisPath :: FilePath
irisPath = "examples/data/iris.parquet"

spec :: Spec
spec = describe "Sabela.Parquet footer" $ do
    it "reads the columns and row count of a real duckdb-written file" $ do
        b <- BS.readFile irisPath
        case parquetFooter b of
            Left e -> expectationFailure ("expected a schema, got: " <> show e)
            Right s -> do
                pqRowCount s `shouldBe` 150
                map pqName (pqColumns s)
                    `shouldBe` [ "sepal.length"
                               , "sepal.width"
                               , "petal.length"
                               , "petal.width"
                               , "variety"
                               ]

    it "types the four measures as Double and the label as Text" $ do
        b <- BS.readFile irisPath
        case parquetFooter b of
            Left e -> expectationFailure (show e)
            Right s ->
                map pqType (pqColumns s)
                    `shouldBe` ["Double", "Double", "Double", "Double", "Text"]

    it "drops the schema root, which names no column" $ do
        b <- BS.readFile irisPath
        case parquetFooter b of
            Left e -> expectationFailure (show e)
            Right s -> map pqName (pqColumns s) `shouldNotContain` ["duckdb_schema"]

    it "refuses a file with no PAR1 marker" $
        parquetFooter "this is just some text, not a table at all"
            `shouldSatisfy` isLeft

    it "refuses a file too small to hold a footer" $
        parquetFooter "PAR1" `shouldSatisfy` isLeft

    it "refuses a truncated file rather than reading past its end" $ do
        b <- BS.readFile irisPath
        parquetFooter (BS.take 200 b) `shouldSatisfy` isLeft

    it "refuses a footer length that does not fit the file" $ do
        b <- BS.readFile irisPath
        let n = BS.length b
            -- claim a footer far larger than the file
            broken =
                BS.concat
                    [ BS.take (n - 8) b
                    , BS.pack [0xff, 0xff, 0xff, 0x7f]
                    , "PAR1"
                    ]
        footerSlice broken `shouldSatisfy` isLeft

    it "survives arbitrary bytes wearing the PAR1 markers" $ do
        let junk = BS.concat ["PAR1", BS.replicate 40 0x5a, BS.pack [8, 0, 0, 0], "PAR1"]
        parquetFooter junk `shouldSatisfy` isLeft

    describe "reading from disk" $ do
        it "gets the same schema by seeking to the tail as by reading it whole" $ do
            b <- BS.readFile irisPath
            got <- readParquetSchema irisPath
            got `shouldBe` parquetFooter b

        it "reports a missing file rather than throwing" $ do
            got <- readParquetSchema "examples/data/no-such-file.parquet"
            got `shouldSatisfy` isLeft

        it "reports a non-parquet file rather than throwing" $ do
            got <- readParquetSchema "examples/data/housing.csv"
            got `shouldSatisfy` isLeft

        it "recognises the extension it should try, and only that" $ do
            isParquetPath "a/b/iris.parquet" `shouldBe` True
            isParquetPath "a/b/IRIS.PARQUET" `shouldBe` True
            isParquetPath "a/b/iris.csv" `shouldBe` False
