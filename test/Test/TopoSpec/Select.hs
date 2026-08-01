{-# LANGUAGE OverloadedStrings #-}

module Test.TopoSpec.Select (spec) where

import Data.List (elemIndex)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Sabela.Model (Cell (..))
import Sabela.Topo
import Test.CellFixture (mkCell)
import Test.Hspec

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
            orderedIds `shouldContain` [1]
            orderedIds `shouldContain` [2]
            orderedIds `shouldNotContain` [3]

        it "finds affected cells even when the dep appears later in notebook order" $ do
            let cells =
                    [ mkCell 1 "let z = y + 1"
                    , mkCell 2 "let y = 1"
                    ]
                (result, _) = selectAffectedTopo 2 cells
                orderedIds = map cellId (trOrdered result)
            orderedIds `shouldContain` [1]
            orderedIds `shouldContain` [2]
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
        it "two cells each binding x do not produce a dependency edge" $ do
            let cells =
                    [ mkCell 1 "f x = x + 1"
                    , mkCell 2 "g x = x * 2"
                    ]
                (defMap, _) = buildDefMap cells
                deps = buildDepGraph defMap cells
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

    describe "DAG: typeclass instance reactivity" $ do
        let classCell = mkCell 1 "class Rand a where\n  rand' :: a -> a"
            instCell = mkCell 2 "instance Rand Int where\n  rand' x = x * 2"
            useCell = mkCell 3 "rand' (9 :: Int)"
            cells = [classCell, instCell, useCell]

        it "the instance depends on its class" $ do
            let (defMap, _) = buildDefMap cells
                deps = buildDepGraph defMap cells
            S.member 1 (M.findWithDefault S.empty 2 deps) `shouldBe` True

        it "the consumer depends on both class and instance" $ do
            let (defMap, _) = buildDefMap cells
                deps = buildDepGraph defMap cells
            M.findWithDefault S.empty 3 deps `shouldBe` S.fromList [1, 2]

        it "editing the class re-runs the instance and the consumer" $ do
            let (result, _) = selectAffectedTopo 1 cells
                ids = map cellId (trOrdered result)
            ids `shouldContain` [1]
            ids `shouldContain` [2]
            ids `shouldContain` [3]

        it "editing the instance re-runs the consumer" $ do
            let (result, _) = selectAffectedTopo 2 cells
                ids = map cellId (trOrdered result)
            ids `shouldContain` [2]
            ids `shouldContain` [3]
            ids `shouldNotContain` [1]

        it "topo order: class before instance before consumer" $ do
            let (result, _) = selectAffectedTopo 1 cells
                ids = map cellId (trOrdered result)
            case (elemIndex 1 ids, elemIndex 2 ids, elemIndex 3 ids) of
                (Just a, Just b, Just c) -> do
                    a `shouldSatisfy` (< b)
                    b `shouldSatisfy` (< c)
                _ -> expectationFailure "all three cells should be in trOrdered"

        it "an instance of a Prelude class does not over-connect method users" $ do
            let cs =
                    [ mkCell 1 "data T = T"
                    , mkCell 2 "instance Show T where\n  show _ = \"T\""
                    , mkCell 3 "show (5 :: Int)"
                    ]
                (defMap, _) = buildDefMap cs
                deps = buildDepGraph defMap cs
            S.member 2 (M.findWithDefault S.empty 3 deps) `shouldBe` False
