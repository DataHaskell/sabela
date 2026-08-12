{-# LANGUAGE OverloadedStrings #-}

{- | Dependency edges beyond plain def/use: shared-ref mutation reaches
readers, data constructors reach pattern matches and signatures, instances
reach users of the type — and a bare IO action stays a leaf.
-}
module Test.GraphEdgesSpec (spec) where

import Sabela.Model (Cell (..), Notebook (..))
import Sabela.Reactivity (ExecutionPlan (..), computeExecutionPlan)
import Test.CellFixture (mkCell)
import Test.Hspec (Spec, describe, it, shouldBe)

nbOf :: [Cell] -> Notebook
nbOf cs = Notebook{nbTitle = "t", nbCells = cs}

runFor :: [Cell] -> Int -> [Int]
runFor cs cid = map cellId (epCellsToRun (computeExecutionPlan cid cs (nbOf cs)))

spec :: Spec
spec = describe "dependency edges beyond plain def/use" $ do
    describe "a bare IO action is a leaf" $ do
        let cells =
                [ mkCell 1 "x = 41"
                , mkCell 2 "print (x + 1)"
                , mkCell 3 "print 99"
                ]
        it "editing the action re-runs only the action" $
            runFor cells 2 `shouldBe` [2]

        it "an action reading nothing shared stays alone" $
            runFor cells 3 `shouldBe` [3]

        it "editing the definition still reaches its reader" $
            runFor cells 1 `shouldBe` [1, 2]

    describe "mutating a shared IORef triggers its readers" $ do
        let cells =
                [ mkCell 1 "ref <- newIORef (0 :: Int)"
                , mkCell 2 "writeIORef ref 5"
                , mkCell 3 "v <- readIORef ref\nprint v"
                , mkCell 4 "modifyIORef ref (+ 1)"
                ]
        it "editing a writer re-runs the readers" $
            runFor cells 2 `shouldBe` [2, 3]

        it "writers do not chain to other writers (no cycle)" $
            runFor cells 4 `shouldBe` [4, 3]

        it "editing the definition reaches writers and readers" $
            runFor cells 1 `shouldBe` [1, 2, 4, 3]

        it "editing a reader re-runs only the reader" $
            runFor cells 3 `shouldBe` [3]

    describe "a write to a ref nobody we know reads stays local" $ do
        let cells =
                [ mkCell 1 "counter <- newIORef (0 :: Int)"
                , mkCell 2 "other <- newIORef (0 :: Int)"
                , mkCell 3 "writeIORef other 9"
                , mkCell 4 "v <- readIORef counter\nprint v"
                ]
        it "editing the writer of the other ref runs nothing else" $
            runFor cells 3 `shouldBe` [3]

    describe "data constructors reach their use sites" $ do
        let cells =
                [ mkCell 1 "data Color = Red | Blue"
                , mkCell
                    2
                    "describe c = case c of\n\
                    \    Red -> \"warm\"\n\
                    \    Blue -> \"cool\""
                , mkCell 3 "area :: Color -> Double\narea _ = 1.0"
                ]
        it "a pattern match on the constructors is a dependency" $
            runFor cells 1 `shouldBe` [1, 2, 3]

        it "a type signature mentioning the type is a dependency" $
            runFor cells 3 `shouldBe` [3]

    describe "instances reach users of the type" $ do
        let cells =
                [ mkCell 1 "data Pt = MkPt Int"
                , mkCell 2 "instance Show Pt where\n    show _ = \"pt\""
                , mkCell 3 "render p = show (p :: Pt)"
                , mkCell 4 "instance Eq Pt where\n    (==) _ _ = True"
                ]
        it "editing an instance re-runs users of the type" $
            runFor cells 2 `shouldBe` [2, 3]

        it "instances of one type do not depend on each other" $
            runFor cells 4 `shouldBe` [4, 3]

        it "editing the data cell reaches instances and users" $
            runFor cells 1 `shouldBe` [1, 2, 4, 3]
