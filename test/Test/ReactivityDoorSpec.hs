{-# LANGUAGE OverloadedStrings #-}

module Test.ReactivityDoorSpec (spec) where

import qualified Data.Set as S
import Sabela.Model (Cell (..), Notebook (..))
import Sabela.Reactivity (
    EnvState (..),
    ExecutionPlan (..),
    ModuleState (..),
    computeRootedExecutionPlan,
    computeStaleExecutionPlanIn,
 )
import Test.CellFixture (dirty, mkCell, proseCell)
import Test.Hspec (Spec, describe, it, shouldBe)

nbOf :: [Cell] -> Notebook
nbOf cs = Notebook{nbTitle = "t", nbCells = cs}

prose :: Cell
prose = proseCell 0 "# Heading"

code :: [Cell]
code = [mkCell 1 "a = 1", mkCell 2 "b = a + 1", mkCell 3 "c = 99"]

nb :: Notebook
nb = nbOf (prose : code)

planFrom :: EnvState -> ModuleState -> S.Set Int -> ExecutionPlan
planFrom env mods roots = computeRootedExecutionPlan env mods roots code nb

runIds :: ExecutionPlan -> [Int]
runIds = map cellId . epCellsToRun

spec :: Spec
spec = describe "stale-state doors stay shut for edits that reach no code" $ do
    describe "a rooted plan whose roots reach no code cell" $ do
        it "runs nothing when the environment is stale" $ do
            let plan = planFrom EnvStale ModulesLoaded (S.singleton 0)
            runIds plan `shouldBe` []
            epRunEnv plan `shouldBe` False

        it "runs nothing when a module reload is pending" $ do
            let plan = planFrom EnvFresh ModulesWiped (S.singleton 0)
            runIds plan `shouldBe` []

        it "runs nothing when both doors are open" $ do
            let plan = planFrom EnvStale ModulesWiped (S.singleton 0)
            runIds plan `shouldBe` []
            epRunEnv plan `shouldBe` False

    describe "a rooted plan whose roots reach code keeps the reactive rule" $ do
        it "a stale environment still makes every code cell a root" $ do
            let plan = planFrom EnvStale ModulesLoaded (S.singleton 1)
            runIds plan `shouldBe` [1, 2, 3]
            epRunEnv plan `shouldBe` True

        it "a pending reload still re-runs every interpreted cell" $ do
            let plan = planFrom EnvFresh ModulesWiped (S.singleton 3)
            runIds plan `shouldBe` [1, 2, 3]

        it "a fresh environment keeps ordinary reachability" $ do
            let plan = planFrom EnvFresh ModulesLoaded (S.singleton 1)
            runIds plan `shouldBe` [1, 2]

    describe "the run-all path is unchanged" $ do
        it "a stale environment with stale cells runs everything" $ do
            let cs = [dirty (mkCell 1 "a = 1"), mkCell 2 "b = 2"]
                plan = computeStaleExecutionPlanIn EnvStale ModulesLoaded cs (nbOf cs)
            map cellId (epCellsToRun plan) `shouldBe` [1, 2]
            epRunEnv plan `shouldBe` True

        it
            "a stale environment with NO stale cells still rebuilds and runs\
            \ everything: run-all reconciles, an edit does not"
            $ do
                let cs = [mkCell 1 "a = 1", mkCell 2 "b = 2"]
                    plan = computeStaleExecutionPlanIn EnvStale ModulesLoaded cs (nbOf cs)
                map cellId (epCellsToRun plan) `shouldBe` [1, 2]
                epRunEnv plan `shouldBe` True
