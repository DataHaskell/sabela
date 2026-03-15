{-# LANGUAGE OverloadedStrings #-}

module Test.TopoSpec (spec) where

import Data.List (elemIndex)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import Sabela.Model (Cell (..), CellType (..))
import Sabela.Topo
import Test.Hspec

-- | Helper to construct a code cell for testing.
mkCell :: Int -> Text -> Cell
mkCell cid src =
    Cell
        { cellId = cid
        , cellType = CodeCell
        , cellSource = src
        , cellOutputs = []
        , cellError = Nothing
        , cellDirty = False
        }

spec :: Spec
spec = describe "Sabela.Topo" $ do
    describe "computeTopoOrder" $ do
        it "preserves notebook order for a linear chain already in dependency order" $ do
            let cells =
                    [ mkCell 1 "let x = 1"
                    , mkCell 2 "let y = x + 1"
                    , mkCell 3 "let z = y + 1"
                    ]
                (result, redefMap) = computeTopoOrder cells
            map cellId (trOrdered result) `shouldBe` [1, 2, 3]
            redefMap `shouldBe` M.empty
            trCycleIds result `shouldBe` S.empty

        it "puts definer before user for a reverse-order chain" $ do
            -- Notebook order: z uses y (cell 1), y uses x (cell 2), x defined (cell 3)
            let cells =
                    [ mkCell 1 "let z = y + 1"
                    , mkCell 2 "let y = x + 1"
                    , mkCell 3 "let x = 1"
                    ]
                (result, redefMap) = computeTopoOrder cells
            -- x (cell 3) must be first, then y (cell 2), then z (cell 1)
            map cellId (trOrdered result) `shouldBe` [3, 2, 1]
            redefMap `shouldBe` M.empty
            trCycleIds result `shouldBe` S.empty

        it "handles a diamond dependency correctly" $ do
            let cells =
                    [ mkCell 1 "let a = 1"
                    , mkCell 2 "let b = a + 1"
                    , mkCell 3 "let c = a + 2"
                    , mkCell 4 "let d = b + c"
                    ]
                (result, _) = computeTopoOrder cells
                ordered = map cellId (trOrdered result)
            -- a (cell 1) must come first, d (cell 4) must come last
            case ordered of
                (first : rest) -> do
                    first `shouldBe` 1
                    last rest `shouldBe` 4
                [] -> expectationFailure "trOrdered should not be empty"
            trCycleIds result `shouldBe` S.empty

        it "puts the later redefining cell in redefMap, not in trOrdered" $ do
            let cells = [mkCell 1 "let x = 1", mkCell 2 "let x = 2"]
                (result, redefMap) = computeTopoOrder cells
            -- cell 2 should be in redefMap (it redefines x)
            M.member 2 redefMap `shouldBe` True
            -- cell 2 should NOT appear in trOrdered
            map cellId (trOrdered result) `shouldNotContain` [2]
            -- cell 1 (canonical definer) should appear in trOrdered
            map cellId (trOrdered result) `shouldContain` [1]

        it "detects a simple two-cell cycle" $ do
            let cells =
                    [ mkCell 1 "let a = b + 1"
                    , mkCell 2 "let b = a + 1"
                    ]
                (result, _) = computeTopoOrder cells
            trCycleIds result `shouldBe` S.fromList [1, 2]
            trOrdered result `shouldBe` []

        it "runs non-cycle cells when a cycle exists among other cells" $ do
            let cells =
                    [ mkCell 1 "let x = 1"
                    , mkCell 2 "let a = b + 1"
                    , mkCell 3 "let b = a + 1"
                    ]
                (result, _) = computeTopoOrder cells
            -- cell 1 has no cycle, should run
            map cellId (trOrdered result) `shouldBe` [1]
            -- cells 2 and 3 form a cycle
            trCycleIds result `shouldBe` S.fromList [2, 3]

    describe "selectAffectedTopo" $ do
        it "propagates edit to downstream cells but not unrelated ones" $ do
            let cells =
                    [ mkCell 1 "let x = 1"
                    , mkCell 2 "let y = x + 1"
                    , mkCell 3 "let z = 42"
                    ]
                (result, _) = selectAffectedTopo 1 cells
                orderedIds = map cellId (trOrdered result)
            -- editing cell 1 (x) should affect cell 2 (y = x + 1)
            orderedIds `shouldContain` [1]
            orderedIds `shouldContain` [2]
            -- cell 3 is unrelated
            orderedIds `shouldNotContain` [3]

        it "finds affected cells even when the dep appears later in notebook order" $ do
            -- cell 1 uses y (defined by cell 2), but cell 1 appears first in notebook
            let cells =
                    [ mkCell 1 "let z = y + 1"
                    , mkCell 2 "let y = 1"
                    ]
                -- edit cell 2 (y), which cell 1 depends on
                (result, _) = selectAffectedTopo 2 cells
                orderedIds = map cellId (trOrdered result)
            -- both cells should be in the affected set
            orderedIds `shouldContain` [1]
            orderedIds `shouldContain` [2]
            -- topo order: cell 2 (y) must come before cell 1 (z = y + 1)
            case (elemIndex 2 orderedIds, elemIndex 1 orderedIds) of
                (Just idx2, Just idx1) -> idx2 `shouldSatisfy` (< idx1)
                _ -> expectationFailure "both cells should be in trOrdered"
