{-# LANGUAGE OverloadedStrings #-}

-- | Tests for 'computeTopoOrder' and the Untitled.md redefinition scenario.
module Test.TopoSpec.Order (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Sabela.Model (Cell (..))
import Sabela.Topo
import Test.Hspec
import Test.TopoSpec.Helpers (mkCell)

spec :: Spec
spec = do
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

        it "flags the LATER cell when two cells redefine the same name (first-wins)" $ do
            let cells = [mkCell 1 "let x = 1", mkCell 2 "let x = 2"]
                (_, redefMap) = computeTopoOrder cells
            -- Cell 1 owns x. Cell 2 is flagged as a redefinition of x;
            -- it will be skipped at execution time and a redef error
            -- will surface on it.
            redefMap `shouldBe` M.fromList [(2, ["x"])]

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

    describe "computeTopoOrder — redefinition semantics (first-wins)" $ do
        it "later cell redefining a used name is flagged; downstream binds to first" $ do
            let cells =
                    [ mkCell 1 "let x = 1"
                    , mkCell 2 "let y = x + 1"
                    , mkCell 3 "let x = 2"
                    ]
                (_, redefMap) = computeTopoOrder cells
                (defMap, _) = buildDefMap cells
            -- Cell 1 owns x (first-wins). Cell 2's `y` depends on cell
            -- 1, not cell 3. Cell 3 is flagged.
            M.lookup "x" defMap `shouldBe` Just 1
            M.lookup "y" defMap `shouldBe` Just 2
            redefMap `shouldBe` M.fromList [(3, ["x"])]

        it "three-way redef: only the first cell owns the name; others flagged" $ do
            let cells =
                    [ mkCell 1 "let x = 1"
                    , mkCell 2 "let x = 2"
                    , mkCell 3 "let x = 3"
                    ]
                (defMap, _) = buildDefMap cells
                (_, redefMap) = computeTopoOrder cells
            M.lookup "x" defMap `shouldBe` Just 1
            redefMap `shouldBe` M.fromList [(2, ["x"]), (3, ["x"])]

        it "redef cell drops ALL its defs (even genuinely new ones)" $ do
            -- Cell 2 redefines x (already owned by cell 1) AND
            -- introduces y. Since cell 2 won't run, neither x nor y
            -- ends up in the session — so y must NOT be in defMap.
            let cells =
                    [ mkCell 1 "let x = 1"
                    , mkCell 2 "let x = 2\nlet y = 1"
                    ]
                (defMap, _) = buildDefMap cells
                (_, redefMap) = computeTopoOrder cells
            redefMap `shouldBe` M.fromList [(2, ["x"])]
            M.lookup "x" defMap `shouldBe` Just 1
            M.lookup "y" defMap `shouldBe` Nothing

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

        it
            "two cells each binding `x` do not form a cycle (function params are scope-local)" $ do
            let cells =
                    [ mkCell 1 "isPrime x = x * 2"
                    , mkCell 2 "f x = x + 1"
                    ]
                (result, _) = computeTopoOrder cells
            trCycleIds result `shouldBe` S.empty

    describe "Untitled.md scenario: redefining f x in a separate cell" $ do
        -- Mirrors the actual Untitled.md notebook: two cells use (f . g),
        -- two cells define f (the second redefines), one cell defines g.
        -- Under first-wins, cell 2 owns f, cell 3 owns g, cell 5 is
        -- flagged as a redefinition error.
        let cells =
                [ mkCell 1 "(f . g) 45"
                , mkCell 2 "f x = x + 5"
                , mkCell 3 "g x = x + 25"
                , mkCell 4 "(f . g) 50"
                , mkCell 5 "f x = x + 10"
                ]
        it "cell 5's redefinition of f is flagged" $ do
            let (_, redefMap) = computeTopoOrder cells
            redefMap `shouldBe` M.fromList [(5, ["f"])]

        it "cell 2 (the first definer) owns f in defMap" $ do
            let (defMap, _) = buildDefMap cells
            M.lookup "f" defMap `shouldBe` Just 2
            M.lookup "g" defMap `shouldBe` Just 3

        it "downstream cells (1, 4) depend on cell 2 for f, not cell 5" $ do
            let (defMap, _) = buildDefMap cells
                deps = buildDepGraph defMap cells
            M.findWithDefault S.empty 1 deps
                `shouldBe` S.fromList [2, 3]
            M.findWithDefault S.empty 4 deps
                `shouldBe` S.fromList [2, 3]

        it "editing the redef cell (5) flags it but runs nothing else" $ do
            let (result, redefMap) = selectAffectedTopo 5 cells
            redefMap `shouldBe` M.fromList [(5, ["f"])]
            map cellId (trOrdered result) `shouldBe` [5]

        it "editing the canonical definer (cell 2) propagates to cells 1 and 4" $ do
            let (result, _) = selectAffectedTopo 2 cells
                ids = map cellId (trOrdered result)
            ids `shouldContain` [2]
            ids `shouldContain` [1]
            ids `shouldContain` [4]
            ids `shouldNotContain` [5]

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
