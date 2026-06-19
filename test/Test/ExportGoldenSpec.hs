{-# LANGUAGE OverloadedStrings #-}

{- | Cross-platform golden for the static notebook export. A notebook
carrying rich LaTeX + HTML + non-ASCII outputs must export
byte-identical HTML on every OS — 'renderStaticNotebook' is pure
(aeson JSON + placeholder replacement, no timestamps or paths), so the
only way the bytes diverge is a platform bug (CRLF translation, a
non-UTF-8 codepage, ordering). The golden is blessed in this
environment and checked by the Windows CI job.

Re-bless after an intentional renderer change:

> SABELA_BLESS_GOLDEN=1 cabal test sabela:sabela-test \
>   --test-options='--match "rich-output export golden"'

The golden file is pinned to LF in .gitattributes (@test/golden/** -text@).
-}
module Test.ExportGoldenSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Test.Hspec

import Sabela.Dashboard (renderStaticNotebook)
import Sabela.Model (
    Cell (..),
    CellType (..),
    MimeType (..),
    Notebook (..),
    OutputItem (..),
 )
import Sabela.SessionTypes (CellLang (..))

goldenPath :: FilePath
goldenPath = "test/golden/export-rich.html"

-- Fixed minimal template: the golden pins the renderer's bytes, not the
-- frontend-coupled real page, so it stays stable across frontend edits.
template :: Text
template =
    "<!doctype html><html><head><meta charset=\"utf-8\"></head>\
    \<body><script>/*__SABELA_INJECT__*/</script></body></html>"

-- Outputs chosen to stress the platform-fragile paths: LaTeX (backslash
-- escaping), HTML, and astral-plane / CJK / accented Unicode.
richNotebook :: Notebook
richNotebook =
    Notebook
        "rich.md"
        [ Cell 1 ProseCell Haskell "# Rich output \8212 caf\233 \955" [] Nothing False
        , Cell
            2
            CodeCell
            Haskell
            "displayLatex gauss"
            [OutputItem MimeLatex "\\sum_{i=1}^{n} i = \\frac{n(n+1)}{2}"]
            Nothing
            False
        , Cell
            3
            CodeCell
            Haskell
            "displayHtml card"
            [ OutputItem MimeHtml "<p>caf\233 \8212 \955 \8594 \26085\26412\35486 \127922</p>"
            ]
            Nothing
            False
        , Cell
            4
            CodeCell
            Haskell
            "displayMarkdown table"
            [ OutputItem
                MimeMarkdown
                "| Function | MB |\n| --- | ---: |\n| mkFastString | 29.0 |\n| caf\233 \955 | 2.7 |\n"
            ]
            Nothing
            False
        ]

richMarkdown :: Text
richMarkdown =
    "# Rich output \8212 caf\233 \955\n\n```haskell\ndisplayLatex gauss\n```\n"

rendered :: BS.ByteString
rendered =
    LBS.toStrict
        (renderStaticNotebook (TE.encodeUtf8 template) richNotebook richMarkdown)

spec :: Spec
spec = describe "Sabela.Dashboard rich-output export golden" $
    it "exports byte-identical HTML across platforms (LaTeX + HTML + Unicode)" $ do
        bless <- lookupEnv "SABELA_BLESS_GOLDEN"
        case bless of
            Just _ -> BS.writeFile goldenPath rendered
            Nothing -> do
                exists <- doesFileExist goldenPath
                if not exists
                    then
                        expectationFailure
                            ( "missing golden "
                                <> goldenPath
                                <> "; bless with SABELA_BLESS_GOLDEN=1"
                            )
                    else do
                        golden <- BS.readFile goldenPath
                        rendered `shouldBe` golden
