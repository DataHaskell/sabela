{-# LANGUAGE OverloadedStrings #-}

{- | Specs for 'Sabela.Compiled' (module grouping, rendering, violations)
and the compiled-cell fields of 'Sabela.Reactivity.ExecutionPlan'.
-}
module Test.CompiledPlanSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Compiled
import Sabela.Errors (parseCompiledErrors)
import Sabela.Model (Cell (..), CellError (..), Notebook (..))
import Sabela.Reactivity (
    ExecutionPlan (..),
    computeExecutionPlan,
    computeFullExecutionPlan,
 )
import Test.Hspec
import Test.TopoSpec.Helpers (mkCell)

nb :: [Cell] -> Notebook
nb cells = Notebook{nbTitle = "test", nbCells = cells}

hasInfix :: Text -> Text -> Bool
hasInfix needle hay = needle `T.isInfixOf` hay

spec :: Spec
spec = describe "Sabela.Compiled" $ do
    describe "planCompiledModules" $ do
        it "ignores cells without a directive" $ do
            let p = planCompiledModules M.empty [mkCell 1 "f x = x + 1"]
            cpModules p `shouldBe` M.empty
            cpCellModule p `shouldBe` M.empty

        it "bare directive lands in the default module" $ do
            let p = planCompiledModules M.empty [mkCell 1 "-- compile\nf x = x + 1"]
            M.keys (cpModules p) `shouldBe` [defaultModuleName]
            cpCellModule p `shouldBe` M.fromList [(1, defaultModuleName)]

        it "named directive gets its own module" $ do
            let p = planCompiledModules M.empty [mkCell 1 "-- compile: Training\nf x = x + 1"]
            M.keys (cpModules p) `shouldBe` ["Training"]

        it "cells sharing a name merge into one module" $ do
            let p =
                    planCompiledModules
                        M.empty
                        [ mkCell 1 "-- compile: Training\nf x = x + 1"
                        , mkCell 2 "-- compile: Training\ng y = f y"
                        ]
                src = cpModules p M.! "Training"
            M.keys (cpModules p) `shouldBe` ["Training"]
            src `shouldSatisfy` hasInfix "f x = x + 1"
            src `shouldSatisfy` hasInfix "g y = f y"

        it "module source carries the default extensions and LINE pragmas" $ do
            let p = planCompiledModules M.empty [mkCell 9 "-- compile\nf x = x + 1"]
                src = cpModules p M.! defaultModuleName
            src `shouldSatisfy` hasInfix "{-# LANGUAGE OverloadedStrings #-}"
            src `shouldSatisfy` hasInfix "sabela-cell-9"
            -- the decl is on line 2 of the cell (after the directive line)
            src `shouldSatisfy` hasInfix "{-# LINE 2 \"sabela-cell-9\" #-}"

        it "cross-module use generates an import edge and import line" $ do
            let p =
                    planCompiledModules
                        M.empty
                        [ mkCell 1 "-- compile: Model\nstep x = x + 1"
                        , mkCell 2 "-- compile: Training\ntrain y = step y"
                        ]
            cpModuleDeps p M.! "Training" `shouldBe` S.singleton "Model"
            (cpModules p M.! "Training") `shouldSatisfy` hasInfix "import Model"

        it "compiled cell using an interpreted name is a violation" $ do
            let p =
                    planCompiledModules
                        M.empty
                        [ mkCell 1 "rate = 3"
                        , mkCell 2 "-- compile\ntrain y = y * rate"
                        ]
            M.keys (cpViolations p) `shouldBe` [2]
            let msgs = map ceMessage (cpViolations p M.! 2)
            T.concat msgs `shouldSatisfy` hasInfix "rate"
            -- the violating cell contributes nothing compilable
            M.lookup 2 (cpCellModule p) `shouldBe` Nothing

        it "invalid module name is a violation" $ do
            let p = planCompiledModules M.empty [mkCell 1 "-- compile: training\nf x = x"]
            M.keys (cpViolations p) `shouldBe` [1]

        it "non-declaration content is a violation with its line" $ do
            let p = planCompiledModules M.empty [mkCell 1 "-- compile\nf x = x\n\nprint 3"]
            map ceLine (cpViolations p M.! 1) `shouldBe` [Just 4]

        it "cross-module cycles are violations on every involved cell" $ do
            let p =
                    planCompiledModules
                        M.empty
                        [ mkCell 1 "-- compile: A\nfa x = gb x"
                        , mkCell 2 "-- compile: B\ngb x = fa x"
                        ]
            M.keys (cpViolations p) `shouldBe` [1, 2]

        it "same-module mutual recursion is fine" $ do
            let p =
                    planCompiledModules
                        M.empty
                        [ mkCell 1 "-- compile: A\nfa x = gb x"
                        , mkCell 2 "-- compile: A\ngb x = fa x"
                        ]
            cpViolations p `shouldBe` M.empty
            M.keys (cpModules p) `shouldBe` ["A"]

        it "bridge values are off limits in compiled cells" $ do
            let p = planCompiledModules M.empty [mkCell 1 "-- compile\nf x = x + _bridge_n"]
            M.keys (cpViolations p) `shouldBe` [1]

    describe "moduleFilePath" $ do
        it "flat name" $ moduleFilePath "Training" `shouldBe` "Training.hs"
        it "dotted name maps to a subdirectory" $
            moduleFilePath "Training.Core" `shouldBe` "Training/Core.hs"

    describe "ExecutionPlan with compiled cells" $ do
        let cells =
                [ mkCell 1 "-- compile: Model\nstep x = x + 1"
                , mkCell 2 "-- compile: Training\ntrain y = step y"
                , mkCell 3 "result = train 5"
                , mkCell 4 "other = 42"
                ]

        it "partitions compiled and interpreted cells" $ do
            let plan = computeFullExecutionPlan cells (nb cells)
            map cellId (epCompileCells plan) `shouldBe` [1, 2]
            map cellId (epCellsToRun plan) `shouldBe` [3, 4]

        it "editing a compiled cell re-runs importing modules' dependents" $ do
            -- Editing Model (cell 1) recompiles Training too (it imports
            -- Model), so Training's interpreted dependent (cell 3) re-runs.
            let plan = computeExecutionPlan 1 cells (nb cells)
            map cellId (epCompileCells plan) `shouldBe` [1, 2]
            map cellId (epCellsToRun plan) `shouldBe` [3]

        it "editing an interpreted cell never triggers a compile" $ do
            let plan = computeExecutionPlan 4 cells (nb cells)
            epCompileCells plan `shouldBe` []
            map cellId (epCellsToRun plan) `shouldBe` [4]

        it "editing a leaf module leaves unrelated modules alone" $ do
            -- Editing Training does not touch Model: Model has no dependents
            -- among modules, so only Training and its dependent re-run.
            let plan = computeExecutionPlan 2 cells (nb cells)
            map cellId (epCompileCells plan) `shouldBe` [2]
            map cellId (epCellsToRun plan) `shouldBe` [3]

        it "violating cells are skipped, others still planned" $ do
            let cs =
                    [ mkCell 1 "rate = 3"
                    , mkCell 2 "-- compile\ntrain y = y * rate"
                    , mkCell 3 "-- compile: Ok\nok x = x"
                    ]
                plan = computeFullExecutionPlan cs (nb cs)
            map cellId (epCompileCells plan) `shouldBe` [3]
            M.keys (cpViolations (epCompilePlan plan)) `shouldBe` [2]

        it "same-module mutual recursion is not reported as a cycle" $ do
            let cs =
                    [ mkCell 1 "-- compile: A\nfa x = gb x"
                    , mkCell 2 "-- compile: A\ngb x = fa x"
                    , mkCell 3 "use = fa 1"
                    ]
                plan = computeFullExecutionPlan cs (nb cs)
            epCycleIds plan `shouldBe` S.empty
            map cellId (epCompileCells plan) `shouldBe` [1, 2]
            map cellId (epCellsToRun plan) `shouldBe` [3]

    describe "parseCompiledErrors" $ do
        let sampleErr =
                T.unlines
                    [ "sabela-cell-12:4:7: error: [GHC-83865]"
                    , "    • Couldn't match expected type ‘Int’ with actual type ‘[Char]’"
                    , "    • In the expression: \"oops\""
                    ]

        it "routes a tagged error block to its cell with cell-relative line" $ do
            let (perCell, loose) = parseCompiledErrors sampleErr
            M.keys perCell `shouldBe` [12]
            loose `shouldBe` []
            let [ce] = perCell M.! 12
            ceLine ce `shouldBe` Just 4
            ceCol ce `shouldBe` Just 7
            ceMessage ce `shouldSatisfy` hasInfix "Couldn't match"

        it "scrubs the tag from the message" $ do
            let (perCell, _) = parseCompiledErrors sampleErr
                [ce] = perCell M.! 12
            ceMessage ce `shouldSatisfy` hasInfix "line 4:7:"
            ceMessage ce `shouldSatisfy` (not . hasInfix "sabela-cell-12")

        it "splits and routes blocks for different cells" $ do
            let two =
                    sampleErr
                        <> T.unlines
                            [ "sabela-cell-7:1:1: error:"
                            , "    Variable not in scope: zorp"
                            ]
                (perCell, loose) = parseCompiledErrors two
            M.keys perCell `shouldBe` [7, 12]
            loose `shouldBe` []

        it "ignores warning-only blocks" $ do
            let warn =
                    T.unlines
                        [ "sabela-cell-3:2:1: warning: [GHC-38417]"
                        , "    Defined but not used: helper"
                        ]
            parseCompiledErrors warn `shouldBe` (M.empty, [])

        it "keeps untagged error blocks as loose" $ do
            let (perCell, loose) =
                    parseCompiledErrors "ghc: panic! error: the impossible happened"
            perCell `shouldBe` M.empty
            length loose `shouldBe` 1
