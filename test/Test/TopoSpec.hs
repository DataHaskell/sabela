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

        it "only re-executes the edited leaf cell with no downstream" $ do
            let cells =
                    [ mkCell 1 "let x = 1"
                    , mkCell 2 "let y = x + 1"
                    , mkCell 3 "let z = y + 1"
                    ]
                (result, _) = selectAffectedTopo 3 cells
                orderedIds = map cellId (trOrdered result)
            orderedIds `shouldBe` [3]

        it "re-executes from mid-chain through all downstream" $ do
            let cells =
                    [ mkCell 1 "let x = 1"
                    , mkCell 2 "let y = x + 1"
                    , mkCell 3 "let z = y + 1"
                    ]
                (result, _) = selectAffectedTopo 2 cells
                orderedIds = map cellId (trOrdered result)
            orderedIds `shouldContain` [2]
            orderedIds `shouldContain` [3]
            orderedIds `shouldNotContain` [1]

        it "only affects the relevant subtree, not independent cells" $ do
            let cells =
                    [ mkCell 1 "let a = 1"
                    , mkCell 2 "let b = 2"
                    , mkCell 3 "let c = a + 1"
                    , mkCell 4 "let d = b + 1"
                    ]
                (result, _) = selectAffectedTopo 1 cells
                orderedIds = map cellId (trOrdered result)
            orderedIds `shouldContain` [1]
            orderedIds `shouldContain` [3]
            orderedIds `shouldNotContain` [2]
            orderedIds `shouldNotContain` [4]

        it "re-executes all cells in a diamond when editing the root" $ do
            let cells =
                    [ mkCell 1 "let a = 1"
                    , mkCell 2 "let b = a + 1"
                    , mkCell 3 "let c = a + 2"
                    , mkCell 4 "let d = b + c"
                    ]
                (result, _) = selectAffectedTopo 1 cells
                orderedIds = map cellId (trOrdered result)
            orderedIds `shouldContain` [1]
            orderedIds `shouldContain` [2]
            orderedIds `shouldContain` [3]
            orderedIds `shouldContain` [4]

        it "re-executes only one branch and join in a diamond" $ do
            let cells =
                    [ mkCell 1 "let a = 1"
                    , mkCell 2 "let b = a + 1"
                    , mkCell 3 "let c = a + 2"
                    , mkCell 4 "let d = b + c"
                    ]
                (result, _) = selectAffectedTopo 2 cells
                orderedIds = map cellId (trOrdered result)
            orderedIds `shouldContain` [2]
            orderedIds `shouldContain` [4]
            orderedIds `shouldNotContain` [1]
            orderedIds `shouldNotContain` [3]

        it "propagates through a long transitive chain from root" $ do
            let cells =
                    [ mkCell 1 "let a = 1"
                    , mkCell 2 "let b = a + 1"
                    , mkCell 3 "let c = b + 1"
                    , mkCell 4 "let d = c + 1"
                    , mkCell 5 "let e = d + 1"
                    ]
                (result, _) = selectAffectedTopo 1 cells
                orderedIds = map cellId (trOrdered result)
            orderedIds `shouldBe` [1, 2, 3, 4, 5]

        it "propagates from mid-chain only to downstream cells" $ do
            let cells =
                    [ mkCell 1 "let a = 1"
                    , mkCell 2 "let b = a + 1"
                    , mkCell 3 "let c = b + 1"
                    , mkCell 4 "let d = c + 1"
                    , mkCell 5 "let e = d + 1"
                    ]
                (result, _) = selectAffectedTopo 3 cells
                orderedIds = map cellId (trOrdered result)
            orderedIds `shouldBe` [3, 4, 5]

        it "only re-executes the edited cell when it has no deps or dependents" $ do
            let cells =
                    [ mkCell 1 "let x = 1"
                    , mkCell 2 "let y = 2"
                    , mkCell 3 "let z = 3"
                    ]
                (result, _) = selectAffectedTopo 2 cells
                orderedIds = map cellId (trOrdered result)
            orderedIds `shouldBe` [2]

        it "re-executes the edited root and the cell using multiple roots" $ do
            let cells =
                    [ mkCell 1 "let a = 1"
                    , mkCell 2 "let b = 2"
                    , mkCell 3 "let c = a + b"
                    ]
                (result, _) = selectAffectedTopo 1 cells
                orderedIds = map cellId (trOrdered result)
            orderedIds `shouldContain` [1]
            orderedIds `shouldContain` [3]
            orderedIds `shouldNotContain` [2]

    describe "cellNames — variable scoping" $ do
        it "does not treat indented lines as definitions" $ do
            let (defs, _) = cellNames "let x = 1\n  let y = 2"
            defs `shouldBe` S.fromList ["x"]

        it "extracts multiple definitions from a multi-line cell" $ do
            let (defs, _) = cellNames "let a = 1\nlet b = 2"
            defs `shouldBe` S.fromList ["a", "b"]

        it "tracks data type definitions" $ do
            let (defs, _) = cellNames "data Foo = Bar | Baz"
            S.member "Foo" defs `shouldBe` True

        it "tracks type alias definitions" $ do
            let (defs, _) = cellNames "type Name = String"
            S.member "Name" defs `shouldBe` True

        it "tracks newtype definitions" $ do
            let (defs, _) = cellNames "newtype Wrapper = Wrap Int"
            S.member "Wrapper" defs `shouldBe` True

        it "tracks class definitions" $ do
            let (defs, _) = cellNames "class MyShow a where"
            S.member "MyShow" defs `shouldBe` True

        it "produces no defs from a comment-only cell" $ do
            let (defs, _) = cellNames "-- just a comment"
            defs `shouldBe` S.empty

        it "does not treat import lines as definitions" $ do
            let (defs, _) = cellNames "import Data.Map"
            defs `shouldBe` S.empty

        it "does not treat pragmas as definitions" $ do
            let (defs, _) = cellNames "{-# LANGUAGE OverloadedStrings #-}"
            defs `shouldBe` S.empty

        it "extracts monadic bind as a definition" $ do
            let (defs, _) = cellNames "x <- readFile \"a\""
            S.member "x" defs `shouldBe` True

        it "treats primed identifiers as distinct names" $ do
            let (defs, uses) = cellNames "x' = x + 1"
            S.member "x'" defs `shouldBe` True
            S.member "x" uses `shouldBe` True
            -- x' and x are separate
            S.member "x" defs `shouldBe` False

    describe "computeTopoOrder — redefinition semantics" $ do
        it "puts redefining cell in redefMap while downstream uses canonical def" $ do
            let cells =
                    [ mkCell 1 "let x = 1"
                    , mkCell 2 "let y = x + 1"
                    , mkCell 3 "let x = 2"
                    ]
                (result, redefMap) = computeTopoOrder cells
                orderedIds = map cellId (trOrdered result)
            M.member 3 redefMap `shouldBe` True
            orderedIds `shouldContain` [1]
            orderedIds `shouldContain` [2]
            orderedIds `shouldNotContain` [3]

        it "puts all later redefinitions in redefMap for three-way redef" $ do
            let cells =
                    [ mkCell 1 "let x = 1"
                    , mkCell 2 "let x = 2"
                    , mkCell 3 "let x = 3"
                    ]
                (result, redefMap) = computeTopoOrder cells
            M.member 2 redefMap `shouldBe` True
            M.member 3 redefMap `shouldBe` True
            map cellId (trOrdered result) `shouldBe` [1]

        it "marks cell as redef even if it also defines unique names" $ do
            let cells =
                    [ mkCell 1 "let x = 1"
                    , mkCell 2 "let x = 2\nlet y = 1"
                    ]
                (_, redefMap) = computeTopoOrder cells
            M.member 2 redefMap `shouldBe` True

    describe "computeTopoOrder — cycle edge cases" $ do
        it "detects a three-cell cycle" $ do
            let cells =
                    [ mkCell 1 "let a = b"
                    , mkCell 2 "let b = c"
                    , mkCell 3 "let c = a"
                    ]
                (result, _) = computeTopoOrder cells
            trCycleIds result `shouldBe` S.fromList [1, 2, 3]
            trOrdered result `shouldBe` []

        it "does not block unrelated affected cells when a cycle exists" $ do
            let cells =
                    [ mkCell 1 "let x = 1"
                    , mkCell 2 "let y = x + 1"
                    , mkCell 3 "let a = b"
                    , mkCell 4 "let b = a"
                    ]
                (result, _) = selectAffectedTopo 1 cells
                orderedIds = map cellId (trOrdered result)
            orderedIds `shouldContain` [1]
            orderedIds `shouldContain` [2]
            orderedIds `shouldNotContain` [3]
            orderedIds `shouldNotContain` [4]

        it "does not mark a self-referencing cell as a cycle (self-deps removed)" $ do
            let cells = [mkCell 1 "let x = x + 1"]
                (result, _) = computeTopoOrder cells
            trCycleIds result `shouldBe` S.empty
            map cellId (trOrdered result) `shouldBe` [1]

    describe "computeTopoOrder — edge cases" $ do
        it "handles a single cell" $ do
            let cells = [mkCell 1 "let x = 1"]
                (result, redefMap) = computeTopoOrder cells
            map cellId (trOrdered result) `shouldBe` [1]
            trCycleIds result `shouldBe` S.empty
            redefMap `shouldBe` M.empty

        it "handles an empty cell list" $ do
            let (result, redefMap) = computeTopoOrder []
            trOrdered result `shouldBe` []
            trCycleIds result `shouldBe` S.empty
            redefMap `shouldBe` M.empty

        it "preserves notebook order for independent cells (stable sort)" $ do
            let cells =
                    [ mkCell 1 "let x = 1"
                    , mkCell 2 "let y = 2"
                    , mkCell 3 "let z = 3"
                    ]
                (result, _) = computeTopoOrder cells
            map cellId (trOrdered result) `shouldBe` [1, 2, 3]
