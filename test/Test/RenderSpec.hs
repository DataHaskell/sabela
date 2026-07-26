{-# LANGUAGE OverloadedStrings #-}

module Test.RenderSpec (spec) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Test.Hspec

import Sabela.Model (Cell (..), CellType (..))
import Sabela.Output (parseMimeOutputs)
import Sabela.Server.Notebook (cellsToSegments, splitProseSegments)
import qualified Sabela.SessionTypes as ST
import ScriptHs.Markdown (Segment (..), parseMarkdown, reassemble)

prose :: Text -> Cell
prose t = Cell 0 ProseCell ST.Haskell t [] Nothing False

proseTexts :: [Segment] -> [Text]
proseTexts segs = [t | Prose t <- segs]

serialize :: [Cell] -> Text
serialize = reassemble . cellsToSegments

roundTrip :: [Cell] -> [Segment]
roundTrip = splitProseSegments . parseMarkdown . serialize

unicodeProse :: Text
unicodeProse = "λ-calculus: f → g, 日本語, café, 🎲 roll"

spec :: Spec
spec = describe "output rendering (cross-platform)" $ do
    it "preserves non-ASCII through the markdown round trip" $
        map T.strip (proseTexts (roundTrip [prose unicodeProse]))
            `shouldBe` [unicodeProse]

    it "preserves non-ASCII inside a rich MIME output block" $ do
        let html = "<p>λ → 日本語 🎲</p>"
            blob = "<!-- MIME:text/html -->\n" <> html <> "\n"
        parseMimeOutputs blob `shouldBe` [("text/html", html <> "\n")]

    it "serializes notebooks with LF endings only (no CRLF)" $ do
        let md = serialize [prose "first", prose unicodeProse, prose "last"]
        T.isInfixOf "\r" md `shouldBe` False

    it "round-trips non-ASCII through a real UTF-8 file on disk" $ do
        tmp <- getTemporaryDirectory
        let path = tmp </> "sabela-render-spec.md"
            cells = [prose unicodeProse, prose "second"]
            bytes = encodeUtf8 (serialize cells)
        BS.writeFile path bytes
        back <- decodeUtf8 <$> BS.readFile path
        removeFile path
        proseTexts (splitProseSegments (parseMarkdown back))
            `shouldBe` [unicodeProse, "second"]
