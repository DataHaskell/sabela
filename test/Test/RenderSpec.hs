{-# LANGUAGE OverloadedStrings #-}

{- | Output-rendering correctness with an eye to Windows pitfalls:
non-ASCII content must survive the markdown round trip and a real
on-disk round trip, and our serialization must stay LF-only so a
notebook written on one OS renders identically on another. These run
on the Windows CI job (.github/workflows/windows.yml), where a
locale-encoding or CRLF-translation regression would surface.
-}
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

-- A spread of non-ASCII the renderer must not mangle: Greek used in
-- Haskell prose, arrows, CJK, and an astral-plane emoji.
unicodeProse :: Text
unicodeProse = "λ-calculus: f → g, 日本語, café, 🎲 roll"

spec :: Spec
spec = describe "output rendering (cross-platform)" $ do
    -- T.strip isolates Unicode survival from scripths' known habit of
    -- adding edge whitespace around a lone prose round trip.
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
