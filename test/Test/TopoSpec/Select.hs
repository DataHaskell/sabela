{-# LANGUAGE OverloadedStrings #-}

-- | Tests for 'selectAffectedTopo' and the DAG (dependency-graph) edges.
module Test.TopoSpec.Select (spec) where

import Data.List (elemIndex)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Sabela.Model (Cell (..))
import Sabela.Topo
import Test.Hspec
import Test.TopoSpec.Helpers (mkCell)

spec :: Spec
spec = do
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

    describe "DAG: function-scoped variables across cells" $ do
        -- These cases exercise scope-aware analysis that the prior
        -- textual heuristic could not get right consistently.
        it "two cells each binding x do not produce a dependency edge" $ do
            let cells =
                    [ mkCell 1 "f x = x + 1"
                    , mkCell 2 "g x = x * 2"
                    ]
                (defMap, _) = buildDefMap cells
                deps = buildDepGraph defMap cells
            -- defMap only contains f and g; x is a local param in both.
            S.member "x" (M.keysSet defMap) `shouldBe` False
            M.findWithDefault S.empty 1 deps `shouldBe` S.empty
            M.findWithDefault S.empty 2 deps `shouldBe` S.empty

        it "where-clause locals do not create cross-cell edges" $ do
            let cells =
                    [ mkCell 1 "shout msg = greet msg\n  where greet m = m"
                    , mkCell 2 "describe greet = greet 1"
                    ]
                (defMap, _) = buildDefMap cells
                deps = buildDepGraph defMap cells
            -- Cell 1's `greet` is a where-binder, not a top-level def.
            -- Cell 2 binds `greet` as a function param. Neither is a
            -- top-level name; no edge should connect them.
            M.lookup "greet" defMap `shouldBe` Nothing
            M.findWithDefault S.empty 1 deps `shouldBe` S.empty
            M.findWithDefault S.empty 2 deps `shouldBe` S.empty

        it "do-binders do not shadow a top-level def in a sibling cell" $ do
            let cells =
                    [ mkCell 1 "msg = \"hello\""
                    , mkCell 2 "act = do\n  msg <- getLine\n  putStrLn msg"
                    ]
                (defMap, _) = buildDefMap cells
                deps = buildDepGraph defMap cells
            -- The `msg <- getLine` in cell 2 binds `msg` locally to the
            -- do-block. Cell 2's top-level def is `act`. There must be
            -- no edge from cell 2 to cell 1.
            M.lookup "msg" defMap `shouldBe` Just 1
            S.member 1 (M.findWithDefault S.empty 2 deps) `shouldBe` False

        it "list-comp generators do not create false edges" $ do
            let cells =
                    [ mkCell 1 "x = 99"
                    , mkCell 2 "evens = [n * 2 | n <- [1, 2, 3], let x = n + 1]"
                    ]
                (defMap, _) = buildDefMap cells
                deps = buildDepGraph defMap cells
            M.lookup "x" defMap `shouldBe` Just 1
            -- Cell 2's `let x = n + 1` is a comprehension-local binder;
            -- it does NOT pull in cell 1's x.
            S.member 1 (M.findWithDefault S.empty 2 deps) `shouldBe` False

    describe "DAG: imports and pragmas do not enter the graph" $ do
        it "an `import` line produces no defs and no deps" $ do
            let cells = [mkCell 1 "import Data.Map (Map)"]
                (defMap, _) = buildDefMap cells
                deps = buildDepGraph defMap cells
            defMap `shouldBe` M.empty
            M.findWithDefault S.empty 1 deps `shouldBe` S.empty

        it "a `{-# LANGUAGE ... #-}` pragma cell is empty in the DAG" $ do
            let cells = [mkCell 1 "{-# LANGUAGE OverloadedStrings #-}"]
                (defMap, _) = buildDefMap cells
                deps = buildDepGraph defMap cells
            defMap `shouldBe` M.empty
            M.findWithDefault S.empty 1 deps `shouldBe` S.empty

        it "a `:set -X...` GHCi directive cell is empty in the DAG" $ do
            let cells = [mkCell 1 ":set -XTypeApplications"]
                (defMap, _) = buildDefMap cells
                deps = buildDepGraph defMap cells
            defMap `shouldBe` M.empty
            M.findWithDefault S.empty 1 deps `shouldBe` S.empty

        it "imports + decl: defMap captures only the decl's name" $ do
            let cells =
                    [mkCell 1 "import Data.Text (Text)\ngreet name = name"]
                (defMap, _) = buildDefMap cells
            defMap `shouldBe` M.fromList [("greet", 1)]
