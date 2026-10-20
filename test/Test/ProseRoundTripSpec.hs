{-# LANGUAGE OverloadedStrings #-}

module Test.ProseRoundTripSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.Model (Cell (..), CellType (..), OutputItem (..))
import Sabela.Output (parseMimeOutputs)
import Sabela.Output.Fence (wrapRuntime)
import Sabela.Output.Grid (gridWidgetJs)
import Sabela.Output.Runtime (elideRuntimes, restoreRuntimes)
import Sabela.Server.Notebook (
    cellsToSegments,
    mimeIndicator,
    parseCodeOutputItems,
    serializeOutputs,
    splitProseSegments,
    textToMime,
 )
import qualified Sabela.SessionTypes as ST
import ScriptHs.Markdown (
    CodeOutput (..),
    MimeType (..),
    Segment (..),
    parseMarkdown,
    reassemble,
 )

prose :: Text -> Cell
prose t = Cell 0 ProseCell ST.Haskell t [] Nothing False

code :: Text -> Cell
code t = Cell 0 CodeCell ST.Haskell t [] Nothing False

roundTrip :: [Cell] -> [Segment]
roundTrip = splitProseSegments . parseMarkdown . reassemble . cellsToSegments

proseTexts :: [Segment] -> [Text]
proseTexts segs = [t | Prose t <- segs]

spec :: Spec
spec = do
    runtimeSpec
    proseSpec

proseSpec :: Spec
proseSpec = describe "markdown + MIME round trip" $ do
    it "keeps two consecutive prose cells separate" $
        roundTrip [prose "First cell", prose "Second cell"]
            `shouldBe` [Prose "First cell", Prose "Second cell"]

    it "keeps three consecutive prose cells separate" $
        proseTexts (roundTrip [prose "A", prose "B", prose "C"])
            `shouldBe` ["A", "B", "C"]

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

    it "keeps an untagged fence in a loaded file out of the code cells" $ do
        let md = "Intro.\n\n```\nplain text, not code\n```\n"
            segs = splitProseSegments (parseMarkdown md)
        [() | CodeBlock{} <- segs] `shouldBe` []
        T.isInfixOf "```\nplain text, not code\n```" (T.concat (proseTexts segs))
            `shouldBe` True

    it "keeps a prose cell containing an untagged fence prose across save/reopen" $ do
        let segs = roundTrip [prose "```\nplain\n```", code "x = 1"]
        length [() | CodeBlock{} <- segs] `shouldBe` 1
        T.isInfixOf "```\nplain\n```" (T.concat (proseTexts segs))
            `shouldBe` True

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

-- | A table output as the prelude prints it: the data, then the runtime.
gridOutput :: Text
gridOutput =
    T.concat
        [ "<div class=\"sabela-grid\">"
        , "<script type=\"application/json\" class=\"sabela-grid-data\">"
        , "{\"cols\":[\"a\"],\"rows\":[[\"1\"]],\"total\":1}"
        , "</script></div><script>"
        , wrapRuntime "grid" gridWidgetJs
        , "\nsabelaGridMountAll();</script>\n"
        ]

tableCell :: Cell
tableCell =
    Cell
        0
        CodeCell
        ST.Haskell
        "display t\n"
        [OutputItem MimeHtml gridOutput]
        Nothing
        False

runtimeSpec :: Spec
runtimeSpec = describe "a table's runtime in the notebook file" $ do
    it "puts back exactly what it took out" $
        restoreRuntimes (elideRuntimes gridOutput) `shouldBe` gridOutput

    it "elides and restores the same however many times it is asked" $ do
        elideRuntimes (elideRuntimes gridOutput) `shouldBe` elideRuntimes gridOutput
        restoreRuntimes (restoreRuntimes gridOutput) `shouldBe` gridOutput

    it "restores a runtime the writing build did not have" $
        let stale = wrapRuntime "grid" "// an older runtime\n"
         in restoreRuntimes (T.replace (wrapRuntime "grid" gridWidgetJs) stale gridOutput)
                `shouldBe` gridOutput

    it "cuts a runtime a notebook older than the fences carries" $ do
        let legacy =
                T.replace (wrapRuntime "grid" gridWidgetJs) gridWidgetJs gridOutput
            elided = elideRuntimes legacy
        T.isInfixOf "function sabelaGridBody" elided `shouldBe` False
        T.isInfixOf "sabelaGridMountAll();" elided `shouldBe` True
        T.isInfixOf "sabela-grid-data" elided `shouldBe` True
        restoreRuntimes elided `shouldBe` gridOutput

    it "leaves a script that only calls the runtime alone" $
        let call = "<div></div><script>sabelaGridMountAll();</script>"
         in elideRuntimes call `shouldBe` call

    it "writes the data but not the runtime" $ do
        let md = reassemble (cellsToSegments [tableCell])
        T.isInfixOf "sabela-grid-data" md `shouldBe` True
        T.isInfixOf "sabelaGridMountAll" md `shouldBe` True
        T.isInfixOf "function sabelaGridBody" md `shouldBe` False
        T.length md < T.length gridWidgetJs `shouldBe` True

    it "hands the reader back a table that can render itself" $ do
        let md = reassemble (cellsToSegments [tableCell])
            loaded =
                [ parseCodeOutputItems m o
                | CodeBlock _ _ (Just (CodeOutput m o)) <- parseMarkdown md
                ]
        loaded `shouldBe` [[OutputItem MimeHtml gridOutput]]
