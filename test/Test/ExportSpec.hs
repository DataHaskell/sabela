{-# LANGUAGE OverloadedStrings #-}

module Test.ExportSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.Export (freezeWidgetSource, proseComment, splitArgs)
import Sabela.Export.Analyze (
    NotebookGraph (..),
    backwardSlice,
    buildNotebookGraph,
 )
import Sabela.Export.Block (Hoisted (..), splitProgram)
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.SessionTypes (CellLang (..))
import ScriptHs.Render (TrailKind (..))

mkCell :: Int -> Text -> Cell
mkCell cid src =
    Cell
        { cellId = cid
        , cellType = CodeCell
        , cellLang = Haskell
        , cellSource = src
        , cellOutputs = []
        , cellError = Nothing
        , cellDirty = False
        }

nb :: [Cell] -> Notebook
nb = Notebook "test.md"

sliceIds :: Int -> [Cell] -> [Int]
sliceIds target cells = map cellId (backwardSlice target (buildNotebookGraph (nb cells)))

spec :: Spec
spec = describe "Sabela.Export" $ do
    describe "backwardSlice" $ do
        it "includes the target and its transitive dependencies, in document order" $ do
            let cells =
                    [ mkCell 1 "let x = 1"
                    , mkCell 2 "let y = x + 1"
                    , mkCell 3 "let z = y + 1"
                    ]
            sliceIds 3 cells `shouldBe` [1, 2, 3]
            sliceIds 2 cells `shouldBe` [1, 2]
            sliceIds 1 cells `shouldBe` [1]

        it "excludes cells the target does not depend on" $ do
            let cells =
                    [ mkCell 1 "let x = 1"
                    , mkCell 2 "let y = x + 1"
                    , mkCell 3 "let z = 42"
                    ]
            sliceIds 2 cells `shouldBe` [1, 2]

        it "pulls in both arms of a diamond" $ do
            let cells =
                    [ mkCell 1 "let a = 1"
                    , mkCell 2 "let b = a + 1"
                    , mkCell 3 "let c = a + 2"
                    , mkCell 4 "let d = b + c"
                    ]
            sliceIds 4 cells `shouldBe` [1, 2, 3, 4]

        it "is empty for a non-existent / non-code target" $
            sliceIds 99 [mkCell 1 "let x = 1"] `shouldBe` []

        it "excludes redefinition-flagged cells from the slice" $ do
            let cells =
                    [ mkCell 1 "let x = 1"
                    , mkCell 2 "let y = x + 1"
                    , mkCell 3 "let x = 2"
                    ]
                g = buildNotebookGraph (nb cells)
            ngRedefIds g `shouldBe` S.fromList [3]
            -- even targeting the redef cell yields nothing (it's excluded)
            map cellId (backwardSlice 3 g) `shouldBe` []

    describe "buildNotebookGraph — widget detection" $ do
        it "marks cells that instantiate a widget and their downstream" $ do
            let cells =
                    [ mkCell 1 "n <- display (slider \"n\" (5 :: Int) 1 10)"
                    , mkCell 2 "let doubled = n * 2"
                    , mkCell 3 "let unrelated = 7"
                    ]
                g = buildNotebookGraph (nb cells)
            ngWidgetCells g `shouldBe` S.fromList [1]
            ngReactiveSet g `shouldBe` S.fromList [1, 2]

    describe "freezeWidgetSource" $ do
        it "freezes a slider to its current value, preserving the type annotation" $
            freezeWidgetSource
                (M.fromList [("celsius", "37")])
                "c <- display (slider \"celsius\" (20 :: Int) (-40) 120)"
                `shouldBe` "c = (37 :: Int)"

        it "freezes a slider to its default when no value is stored" $
            freezeWidgetSource
                M.empty
                "c <- display (slider \"celsius\" (20 :: Int) (-40) 120)"
                `shouldBe` "c = (20 :: Int)"

        it "freezes a checkbox to a Bool" $
            freezeWidgetSource
                (M.fromList [("v", "true")])
                "v <- display (checkbox \"v\" False)"
                `shouldBe` "v = True"

        it "freezes a dropdown to a quoted string" $
            freezeWidgetSource
                (M.fromList [("shape", "Square")])
                "shape <- display (dropdown \"shape\" [\"Circle\", \"Square\"] \"Circle\")"
                `shouldBe` "shape = \"Square\""

        it "freezes a textInput default when unset" $
            freezeWidgetSource
                M.empty
                "name <- display (textInput \"name\" \"World\")"
                `shouldBe` "name = \"World\""

        it "freezes a clicked button to Just ()" $
            freezeWidgetSource
                (M.fromList [("go", "clicked")])
                "clicked <- display (button \"Compute\" \"go\")"
                `shouldBe` "clicked = Just ()"

        it "leaves non-widget binds untouched" $
            freezeWidgetSource M.empty "x <- readFile \"a.txt\""
                `shouldBe` "x <- readFile \"a.txt\""

        it "leaves composed/unrecognized widget expressions untouched" $ do
            let line =
                    "area <- display (liftA2 (*) (slider \"w\" (1 :: Int) 1 9) (slider \"h\" (1 :: Int) 1 9))"
            freezeWidgetSource M.empty line `shouldBe` line

    describe "splitArgs" $ do
        it "splits top-level args, treating parens/brackets/strings as atomic" $
            splitArgs "\"celsius\" (20 :: Int) (-40) 120"
                `shouldBe` ["\"celsius\"", "(20 :: Int)", "(-40)", "120"]

        it "keeps a bracketed list as one argument" $
            splitArgs "\"shape\" [\"A\", \"B\"] \"A\""
                `shouldBe` ["\"shape\"", "[\"A\", \"B\"]", "\"A\""]

    describe "proseComment" $ do
        it "wraps prose in a block comment" $
            proseComment ["Hello world"] `shouldBe` "{- \nHello world\n-}"

        it "neutralizes embedded comment delimiters" $ do
            let out = proseComment ["closing -} here"]
            -- the only "-}" left should be the closing delimiter
            T.isInfixOf "-}" (T.dropEnd 2 out) `shouldBe` False

        it "is empty for no prose" $
            proseComment [] `shouldBe` ""

    describe "splitProgram (everything-in-main)" $ do
        let split = splitProgram (const TrailUnknown) S.empty
        it
            "keeps an IO bind and a dependent binding together in main (binding becomes a let)"
            $ do
                let (h, stmts) = split ["x <- getLine\ny = length x"]
                stmts `shouldBe` ["x <- getLine", "let y = length x"]
                hTopDecls h `shouldBe` []

        it "attaches a type signature to its binding as a single let" $ do
            let (_, stmts) = split ["f :: Int -> Int\nf x = x + 1"]
            stmts `shouldBe` ["let f :: Int -> Int\n    f x = x + 1"]

        it "hoists data declarations to the top level" $ do
            let (h, stmts) = split ["data Color = Red | Blue"]
            hTopDecls h `shouldBe` ["data Color = Red | Blue"]
            stmts `shouldBe` []

        it "hoists imports and translates :set -X to a LANGUAGE pragma" $ do
            let (h, _) = split ["import Data.List (sort)", ":set -XGADTs"]
            hImports h `shouldBe` ["import Data.List (sort)"]
            hPragmas h `shouldBe` ["{-# LANGUAGE GADTs #-}"]

        it "drops <- binds whose binder is in the skip set (widget params)" $ do
            let (_, stmts) =
                    splitProgram
                        (const TrailUnknown)
                        (S.fromList ["c"])
                        ["c <- slider\nlet f = c + 1"]
            stmts `shouldBe` ["let f = c + 1"]
