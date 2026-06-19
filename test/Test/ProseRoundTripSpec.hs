{-# LANGUAGE OverloadedStrings #-}

module Test.ProseRoundTripSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.Model (Cell (..), CellType (..), OutputItem (..))
import Sabela.Output (parseMimeOutputs)
import Sabela.Server.Notebook (
    cellsToSegments,
    mimeIndicator,
    serializeOutputs,
    splitProseSegments,
    textToMime,
 )
import qualified Sabela.SessionTypes as ST
import ScriptHs.Markdown (
    MimeType (..),
    Segment (..),
    parseMarkdown,
    reassemble,
 )

prose :: Text -> Cell
prose t = Cell 0 ProseCell ST.Haskell t [] Nothing False

code :: Text -> Cell
code t = Cell 0 CodeCell ST.Haskell t [] Nothing False

-- Full on-disk round trip at the segment level: serialize cells to
-- markdown, then parse + split back into segments.
roundTrip :: [Cell] -> [Segment]
roundTrip = splitProseSegments . parseMarkdown . reassemble . cellsToSegments

proseTexts :: [Segment] -> [Text]
proseTexts segs = [t | Prose t <- segs]

spec :: Spec
spec = describe "markdown + MIME round trip" $ do
    it "keeps two consecutive prose cells separate" $
        roundTrip [prose "First cell", prose "Second cell"]
            `shouldBe` [Prose "First cell", Prose "Second cell"]

    it "keeps three consecutive prose cells separate" $
        proseTexts (roundTrip [prose "A", prose "B", prose "C"])
            `shouldBe` ["A", "B", "C"]

    -- NB: scripths' own round trip adds edge whitespace to a lone Prose
    -- segment (parseMarkdown does T.unlines; code fences add a blank
    -- line). That is pre-existing behavior outside this fix's scope, so
    -- these two cases assert content modulo surrounding whitespace.
    it "preserves prose boundaries around a code cell" $ do
        let segs = roundTrip [prose "Intro", code "x = 1", prose "Outro"]
        map T.strip (proseTexts segs) `shouldBe` ["Intro", "Outro"]
        length [() | CodeBlock{} <- segs] `shouldBe` 1

    it "leaves a single prose cell unchanged (no marker emitted)" $ do
        let md = reassemble (cellsToSegments [prose "Just one"])
        map T.strip (proseTexts (splitProseSegments (parseMarkdown md)))
            `shouldBe` ["Just one"]
        T.isInfixOf "sabela:cell" md `shouldBe` False

    it "inserts no spurious prose cell between two adjacent code cells" $
        roundTrip [code "x = 1", code "y = 2"]
            `shouldBe` [ CodeBlock "haskell" "x = 1\n" Nothing
                       , CodeBlock "haskell" "y = 2\n" Nothing
                       ]

    it "preserves an empty cell between two prose cells" $
        proseTexts (roundTrip [prose "A", prose "", prose "B"])
            `shouldBe` ["A", "", "B"]

    it "is idempotent across repeated save/reopen" $ do
        let cells0 = [prose "Alpha", prose "Beta", prose "Gamma"]
            md1 = reassemble (cellsToSegments cells0)
            cells1 =
                [ prose t
                | Prose t <- splitProseSegments (parseMarkdown md1)
                ]
            md2 = reassemble (cellsToSegments cells1)
        md2 `shouldBe` md1
        proseTexts (splitProseSegments (parseMarkdown md2))
            `shouldBe` ["Alpha", "Beta", "Gamma"]

    it "round-trips every MIME through textToMime . mimeIndicator (image too)" $
        let mimes =
                [ MimeHtml
                , MimeMarkdown
                , MimeSvg
                , MimeLatex
                , MimeJson
                , MimePlain
                , MimeImage "image/png"
                ]
         in map (mimeIndicator . textToMime . mimeIndicator) mimes
                `shouldBe` map mimeIndicator mimes

    it "serializeOutputs → parseMimeOutputs round-trips an output block" $
        let items =
                [ OutputItem MimeHtml "<p>hello</p>\n"
                , OutputItem MimeMarkdown "# Title\n"
                , OutputItem MimeSvg "<svg/>\n"
                ]
            roundTripped = parseMimeOutputs (serializeOutputs items)
            expected =
                [ ("text/html", "<p>hello</p>\n")
                , ("text/markdown", "# Title\n")
                , ("image/svg+xml", "<svg/>\n")
                ]
         in roundTripped `shouldBe` expected
