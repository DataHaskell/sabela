{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the static "notebook" (tutorial) export renderer: it must embed
the notebook JSON (with cell source), flag the notebook render mode, embed the
reassembled markdown for the in-page download, and keep any @</script>@ in cell
content escaped. The plain dashboard render must stay mode-free.
-}
module Test.NotebookExportSpec (spec) where

import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Sabela.Dashboard (renderStaticDashboard, renderStaticNotebook)
import Sabela.Model (Cell (..), CellType (..), MimeType (..), Notebook (..), OutputItem (..))
import Sabela.SessionTypes (CellLang (..))
import Test.Hspec

-- A minimal template carrying the injection placeholder.
template :: Text
template = "<html><body><script>/*__SABELA_INJECT__*/</script></body></html>"

renderNotebook :: Notebook -> Text -> Text
renderNotebook nb md =
    TE.decodeUtf8
        (LBS.toStrict (renderStaticNotebook (TE.encodeUtf8 template) nb md))

renderDashboard :: Notebook -> Text
renderDashboard nb =
    TE.decodeUtf8 (LBS.toStrict (renderStaticDashboard (TE.encodeUtf8 template) nb))

nb1 :: Notebook
nb1 =
    Notebook
        "tut.md"
        [ Cell 1 ProseCell Haskell "# Title" [] Nothing False
        , Cell
            2
            CodeCell
            Haskell
            "putStrLn \"hi\""
            [OutputItem MimePlain "hi"]
            Nothing
            False
        ]

spec :: Spec
spec = describe "Sabela.Dashboard static notebook export" $ do
    it "flags the notebook render mode" $
        renderNotebook nb1 "md"
            `shouldSatisfy` T.isInfixOf "window.__SABELA_RENDER_MODE__ = \"notebook\""

    it "embeds the notebook JSON including cell source" $ do
        let out = renderNotebook nb1 "md"
        out `shouldSatisfy` T.isInfixOf "window.__SABELA_STATIC__"
        out `shouldSatisfy` T.isInfixOf "putStrLn" -- cellSource is present
    it "embeds the reassembled markdown for download" $ do
        let out = renderNotebook nb1 "# Title\n\n```haskell\nputStrLn x\n```\n"
        out `shouldSatisfy` T.isInfixOf "window.__SABELA_MARKDOWN__ ="
        out `shouldSatisfy` T.isInfixOf "haskell" -- md content carried through
    it "escapes </script> in cell content so it cannot close the tag" $ do
        let evil = Notebook "x" [Cell 1 ProseCell Haskell "</script><b>x</b>" [] Nothing False]
            out = renderNotebook evil "md"
        out `shouldSatisfy` T.isInfixOf "<\\/script>" -- escaped form present
        out `shouldSatisfy` T.isInfixOf "<\\/b>"

    it "escapes </script> in the embedded markdown too" $
        renderNotebook nb1 "before </script> after"
            `shouldSatisfy` T.isInfixOf "<\\/script>"

    it "leaves the plain dashboard render mode-free (regression)" $ do
        let out = renderDashboard nb1
        out `shouldSatisfy` T.isInfixOf "window.__SABELA_STATIC__"
        out `shouldSatisfy` (not . T.isInfixOf "__SABELA_RENDER_MODE__")
        out `shouldSatisfy` (not . T.isInfixOf "__SABELA_MARKDOWN__")
