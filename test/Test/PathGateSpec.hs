{-# LANGUAGE OverloadedStrings #-}

module Test.PathGateSpec (spec) where

import Data.Aeson (Value, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Sabela.AI.PathGate (pathGateCheck, pathRefs, writesFiles)

writeFixture :: FilePath -> FilePath -> IO ()
writeFixture root rel = do
    let full = root </> rel
    createDirectoryIfMissing True (takeDirectory full)
    writeFile full "x"

mentions :: Value -> String -> Bool
mentions v needle = needle `isInfixOf` LBS.unpack (encode v)

readsIris :: Text
readsIris = "main = readFile \"./examples/data/iris.csv\" >>= putStrLn"

spec :: Spec
spec = describe "Sabela.AI.PathGate" $ do
    describe "pathRefs" $ do
        it "finds a path literal in a read call" $
            pathRefs readsIris `shouldBe` ["./examples/data/iris.csv"]

        it "ignores literals that are not path-shaped" $
            pathRefs "greet = putStrLn \"hello\" >> print \"Data.Map\""
                `shouldBe` []

        it "ignores a formatting string that merely has a dot" $
            pathRefs "fmt x = printf \"%.2f\" x" `shouldBe` []

        it "ignores a path that only appears in a comment" $
            pathRefs "-- reads ./examples/data/iris.csv\nmain = pure ()"
                `shouldBe` []

        it "sees through an escaped quote earlier in the cell" $
            pathRefs "q = \"a \\\" b\"\nmain = readFile \"./data/x.csv\""
                `shouldBe` ["./data/x.csv"]

        it "keeps a URL literal" $
            pathRefs "main = readFile \"https://example.com/iris.csv\""
                `shouldBe` ["https://example.com/iris.csv"]

        it "ignores inline SVG markup, slashes and all" $
            pathRefs
                "svg = concat [\"<svg xmlns='http://www.w3.org/2000/svg' width='\", \"'/>\"]"
                `shouldBe` []

        it "ignores a closing tag split over a raw newline" $
            pathRefs "tag = \"</svg\n>\"\ndisplaySvg tag" `shouldBe` []

        it "ignores a bare filename that nothing reads" $
            pathRefs "link = \"index.html\"" `shouldBe` []

        it "keeps a bare filename that a read call is given" $
            pathRefs "main = readFile \"iris.csv\"" `shouldBe` ["iris.csv"]

    describe "writesFiles" $ do
        it "is True for a cell that writes" $
            writesFiles "main = writeFile \"./out.csv\" body" `shouldBe` True

        it "is False for a cell that only reads" $
            writesFiles readsIris `shouldBe` False

    describe "pathGateCheck" $ do
        it "passes a cell whose path exists" $
            withSystemTempDirectory "path-gate" $ \root -> do
                writeFixture root "examples/data/iris.csv"
                got <- pathGateCheck root [] readsIris
                got `shouldBe` Right (readsIris, [])

        it "passes a cell that names no path at all" $
            withSystemTempDirectory "path-gate" $ \root -> do
                got <- pathGateCheck root [] "x = 1 + 1"
                got `shouldBe` Right ("x = 1 + 1", [])

        it "refuses a missing path and names the same-stem candidate" $
            withSystemTempDirectory "path-gate" $ \root -> do
                writeFixture root "examples/data/iris.parquet"
                got <- pathGateCheck root [] readsIris
                case got of
                    Right _ -> expectationFailure "expected a refusal"
                    Left v -> do
                        v `shouldSatisfy` (`mentions` "examples/data/iris.parquet")
                        v `shouldSatisfy` (`mentions` "path-not-found")

        it "refuses a missing path even with nothing similar on disk" $
            withSystemTempDirectory "path-gate" $ \root -> do
                writeFixture root "notes.md"
                got <- pathGateCheck root [] readsIris
                case got of
                    Right _ -> expectationFailure "expected a refusal"
                    Left v -> v `shouldSatisfy` (`mentions` "iris.csv")

        it "repairs a unique basename match and discloses the rewrite" $
            withSystemTempDirectory "path-gate" $ \root -> do
                writeFixture root "data/iris.csv"
                got <- pathGateCheck root [] readsIris
                case got of
                    Left _ -> expectationFailure "expected a repair"
                    Right (src, notes) -> do
                        src `shouldSatisfy` ("./data/iris.csv" `T.isInfixOf`)
                        notes `shouldSatisfy` (not . null)

        it "leaves a cell that writes its own output file alone" $
            withSystemTempDirectory "path-gate" $ \root -> do
                let src = "main = writeFile \"./out/report.csv\" body"
                got <- pathGateCheck root [] src
                got `shouldBe` Right (src, [])

        it "allows a path another cell produces" $
            withSystemTempDirectory "path-gate" $ \root -> do
                let producer = "main = writeFile \"./out/report.csv\" body"
                    consumer = "main = readFile \"./out/report.csv\""
                got <- pathGateCheck root [producer] consumer
                got `shouldBe` Right (consumer, [])

        it "refuses a URL handed to a file-reading function" $
            withSystemTempDirectory "path-gate" $ \root -> do
                let src = "main = readFile \"https://example.com/iris.csv\""
                got <- pathGateCheck root [] src
                case got of
                    Right _ -> expectationFailure "expected a refusal"
                    Left v -> v `shouldSatisfy` (`mentions` "url-as-path")

        it "does not touch a URL that is not going to a file function" $
            withSystemTempDirectory "path-gate" $ \root -> do
                let src = "endpoint = \"https://example.com/iris.csv\""
                got <- pathGateCheck root [] src
                got `shouldBe` Right (src, [])
