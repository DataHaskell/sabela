{-# LANGUAGE OverloadedStrings #-}

-- | @reactivity.md@, "Core Value-Level Reactivity".
module Test.ReactivityDoc.Values (spec) where

import qualified Data.Set as S
import qualified Data.Text as T
import Sabela.Model (Cell (..))
import Sabela.Reactivity (ExecutionPlan (..), cycleErrorMsg)
import Test.CellFixture (dirty, errored, mkCell)
import Test.Hspec
import Test.ReactivityDoc.Fixture

spec :: Spec
spec = describe "reactivity.md: core value-level reactivity" $ do
    describe "topological sorting: linear and multiple dependencies" $ do
        let cells =
                [ mkCell 1 "x = 21"
                , mkCell 2 "y = x * 2"
                , mkCell 3 "first = \"Ada\""
                , mkCell 4 "last' = \"Lovelace\""
                , mkCell 5 "msg = first ++ \" \" ++ last'"
                ]
        it "changing x re-runs y and nothing from the string chain" $
            planFor cells 1 `shouldBe` [1, 2]
        it "changing first re-runs msg only" $
            planFor cells 3 `shouldBe` [3, 5]
        it "changing last re-runs msg only" $
            planFor cells 4 `shouldBe` [4, 5]
        it "changing the sink re-runs just the sink" $
            planFor cells 5 `shouldBe` [5]

    describe "glitch prevention: the diamond" $ do
        let cells =
                [ mkCell 1 "a = 5"
                , mkCell 2 "b = a + 2"
                , mkCell 3 "c = a * 3"
                , mkCell 4 "d = b + c"
                ]
            ids = planFor cells 1
        it "changing a schedules d exactly once" $
            length (filter (== 4) ids) `shouldBe` 1
        it "d runs only after both b and c" $ do
            let precedesD x = (<) <$> indexOf x ids <*> indexOf 4 ids
            precedesD 2 `shouldBe` Just True
            precedesD 3 `shouldBe` Just True
        it "the plan is a, b, c, d in notebook order" $
            ids `shouldBe` [1, 2, 3, 4]
        it "a diamond declared out of notebook order still joins last" $ do
            let shuffled =
                    [ mkCell 1 "d = b + c"
                    , mkCell 2 "c = a * 3"
                    , mkCell 3 "b = a + 2"
                    , mkCell 4 "a = 5"
                    ]
            planFor shuffled 4 `shouldBe` [4, 2, 3, 1]

    describe "lexical scope and closures" $ do
        let cells =
                [ mkCell 1 "factor = 3"
                , mkCell 2 "multiplyByFactor x = x * factor"
                , mkCell 3 "result = multiplyByFactor 10"
                , mkCell 4 "other = multiplyByFactor 1 + result"
                , mkCell 5 "unrelated = 0"
                ]
        it "changing factor invalidates the closure and every caller" $
            planFor cells 1 `shouldBe` [1, 2, 3, 4]
        it "the parameter x is local and makes no edge" $
            planFor cells 5 `shouldBe` [5]
        it "a closure over a where-bound name is not an upstream edge" $ do
            let cs =
                    [ mkCell 1 "factor = 3"
                    , mkCell 2 "scale x = x * factor\n  where factor = 2"
                    ]
            planFor cs 1 `shouldBe` [1]

    describe "structural edge cases: cyclic dependencies" $ do
        let cells =
                [ mkCell 1 "foo = bar + 1"
                , mkCell 2 "bar = foo * 2"
                , mkCell 3 "z = 1"
                ]
            plan = planOf cells 1
        it "the cycle is detected statically from the edited cell" $
            epCycleIds plan `shouldBe` S.fromList [1, 2]
        it "neither cycle member is executed" $
            map cellId (epCellsToRun plan) `shouldBe` []
        it "editing the other member reports the same cycle" $
            epCycleIds (planOf cells 2) `shouldBe` S.fromList [1, 2]
        it "a cell outside the cycle still runs on its own edit" $
            planFor cells 3 `shouldBe` [3]
        it "the cycle error names the cells and the variables" $ do
            let msg =
                    cycleErrorMsg
                        (epCellPositions plan)
                        (epCycleIds plan)
                        cells
                        (epDefMap plan)
            msg `shouldSatisfy` T.isInfixOf "circular dependency"
            msg `shouldSatisfy` T.isInfixOf "foo"
            msg `shouldSatisfy` T.isInfixOf "bar"
        it "a dependent of the cycle is blocked as well" $ do
            let cs = cells ++ [mkCell 4 "w = foo + 1"]
                p = planOf cs 1
            map cellId (epCellsToRun p) `shouldNotContain` [4]
            epBlockedByCycle p `shouldBe` S.fromList [4]
        it "a dependent of the cycle is not itself reported as a cycle member" $ do
            let cs = cells ++ [mkCell 4 "w = foo + 1"]
            epCycleIds (planOf cs 1) `shouldBe` S.fromList [1, 2]

    describe "structural edge cases: upstream failures" $ do
        let cells =
                [ mkCell 1 "x = 1"
                , errored (mkCell 2 "y = x + \"string\"")
                , mkCell 3 "z = y * 2"
                ]
        it "an errored cell is settled: run-all does not retry it" $
            stalePlanFor cells `shouldBe` []
        it "editing the broken cell re-plans it and its dependents" $
            planFor cells 2 `shouldBe` [2, 3]
        it "fixing the upstream error resumes the suspended consumer" $ do
            let fixedCells =
                    [ mkCell 1 "x = 1"
                    , dirty (mkCell 2 "y = x + 1")
                    , mkCell 3 "z = y * 2"
                    ]
            stalePlanFor fixedCells `shouldBe` [2, 3]
