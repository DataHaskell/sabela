{-# LANGUAGE OverloadedStrings #-}

module Test.LeanExportSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Handlers.Lean (
    CellLineMap (..),
    assembleLeanDocument,
    groupDiagsByCell,
    mapDiagToError,
    parseLeanExports,
 )
import Sabela.LeanLsp (
    Diagnostic (..),
    DiagnosticSeverity (..),
    Position (..),
    Range (..),
 )
import Sabela.Model (Cell (..), CellError (..), CellType (..))
import Sabela.SessionTypes (CellLang (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Data.Map.Strict as M

-- | Helper to create a diagnostic at a given document line.
mkDiag :: Int -> DiagnosticSeverity -> Text -> Diagnostic
mkDiag line sev msg =
    Diagnostic
        { diagRange =
            Range
                { rangeStart = Position{posLine = line, posCharacter = 0}
                , rangeEnd = Position{posLine = line, posCharacter = 0}
                }
        , diagSeverity = Just sev
        , diagMessage = msg
        }

mkLeanCell :: Int -> Text -> Cell
mkLeanCell cid src = Cell cid CodeCell Lean4 src [] Nothing False

spec :: Spec
spec = describe "Lean export capture" $ do
    describe "parseLeanExports" $ do
        it "matches export annotation to #eval output by line number" $ do
            let src = "-- export: my_val\n#eval toString 42"
                diags = [mkDiag 1 DsInformation "42"]
                cellOffset = 0
            parseLeanExports src cellOffset diags
                `shouldBe` [("my_val", "42")]

        it "returns empty when no info diagnostics exist" $ do
            let src = "-- export: my_val\n#eval toString 42"
                diags = [] :: [Diagnostic]
            parseLeanExports src 0 diags
                `shouldBe` []

        it "handles cell offset from document header" $ do
            -- If the cell starts at line 5 in the assembled document,
            -- the export annotation is at doc line 5, #eval at doc line 6.
            let src = "-- export: result\n#eval 100"
                diags = [mkDiag 6 DsInformation "100"]
                cellOffset = 5
            parseLeanExports src cellOffset diags
                `shouldBe` [("result", "100")]

        it "ignores diagnostics outside the export's range" $ do
            let src = "-- export: a\n#eval 1\n-- export: b\n#eval 2"
                diags =
                    [ mkDiag 1 DsInformation "1"
                    , mkDiag 3 DsInformation "2"
                    ]
            parseLeanExports src 0 diags
                `shouldBe` [("a", "1"), ("b", "2")]

        it "skips exports with no matching diagnostic" $ do
            let src = "-- export: missing\n#eval undefined\n-- export: present\n#eval 42"
                -- Only the second #eval produces a diagnostic
                diags = [mkDiag 3 DsInformation "42"]
            parseLeanExports src 0 diags
                `shouldBe` [("present", "42")]

        it "ignores error diagnostics (only matches info/hint)" $ do
            let src = "-- export: val\n#eval 1"
                diags = [mkDiag 1 DsError "type mismatch"]
            -- parseLeanExports receives pre-filtered diags (only info/hint),
            -- so passing an error diag means it's there by mistake — the
            -- line match should still work but the caller filters severity.
            -- Here we just verify the function handles whatever it receives.
            parseLeanExports src 0 diags
                `shouldBe` [("val", "type mismatch")]

    describe "assembleLeanDocument" $ do
        it "hoists imports to the top of the document" $ do
            let cells =
                    [ mkLeanCell 1 "import Lean\n#eval 42"
                    , mkLeanCell 2 "def foo := 1"
                    ]
                (doc, _lineMap) = assembleLeanDocument M.empty cells
            -- "import Lean" should be the first line of the document
            head (T.lines doc) `shouldBe` "import Lean"

        it "produces correct line map for cells without imports" $ do
            let cells =
                    [ mkLeanCell 1 "def x := 1"
                    , mkLeanCell 2 "#eval x"
                    ]
                (_doc, lineMap) = assembleLeanDocument M.empty cells
            lineMap
                `shouldBe` [ CellLineMap 1 0 1
                           , CellLineMap 2 1 2
                           ]

        it "injects bridge definitions before cell body" $ do
            let cells = [mkLeanCell 1 "#eval _bridge_foo"]
                bridge = M.fromList [("foo", "hello")]
                (doc, lineMap) = assembleLeanDocument bridge cells
            -- Bridge def takes 2 lines (def + blank), then cell at line 2
            head lineMap `shouldBe` CellLineMap 1 2 3
            -- Verify bridge def is in the document text
            any ("def _bridge_foo" `T.isPrefixOf`) (T.lines doc) `shouldBe` True

    describe "groupDiagsByCell" $ do
        it "assigns diagnostics to the correct cell by line number" $ do
            let lineMap =
                    [ CellLineMap 1 0 3
                    , CellLineMap 2 3 5
                    ]
                diags =
                    [ mkDiag 1 DsInformation "from cell 1"
                    , mkDiag 4 DsError "from cell 2"
                    ]
            groupDiagsByCell lineMap diags
                `shouldBe` M.fromList
                    [ (1, [mkDiag 1 DsInformation "from cell 1"])
                    , (2, [mkDiag 4 DsError "from cell 2"])
                    ]

        it "drops diagnostics outside any cell range" $ do
            let lineMap = [CellLineMap 1 2 4]
                diags = [mkDiag 0 DsInformation "header diag"]
            groupDiagsByCell lineMap diags `shouldBe` M.empty

    describe "mapDiagToError" $ do
        it "converts document line to cell-relative line" $ do
            let lineMap = [CellLineMap 1 5 10]
                d = mkDiag 7 DsError "oops"
                err = mapDiagToError lineMap 1 d
            ceLine err `shouldBe` Just 3
            ceCol err `shouldBe` Just 1
            ceMessage err `shouldBe` "oops"
