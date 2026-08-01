{-# LANGUAGE OverloadedStrings #-}

module Test.RestartModeSpec (spec) where

import Sabela.Model (Cell (..), Notebook (..))
import Sabela.Reactivity (RestartMode (..), applyRestart)
import Test.CellFixture (mkCell, proseCell, withOutput)
import Test.Hspec

nb :: Notebook
nb =
    Notebook
        "t"
        [ proseCell 0 "some prose"
        , withOutput "42" (mkCell 1 "x = 42")
        , withOutput "chart" (mkCell 2 "y = x + 1")
        ]

codeCells :: Notebook -> [Cell]
codeCells n = [c | c <- nbCells n, cellId c /= 0]

spec :: Spec
spec = describe "applyRestart (what a restart does to the notebook)" $ do
    it
        "invalidates every code cell in every mode: the kernel comes back empty,\
        \ so no cell's output is still current"
        $ mapM_
            ( \mode ->
                map cellDirty (codeCells (applyRestart mode nb))
                    `shouldBe` [True, True]
            )
            [RestartOnly, RestartRunAll, RestartClear]

    it
        "keeps outputs on a plain restart: a user restarting a wedged kernel\
        \ should not lose the charts already on screen"
        $ map cellOutputs (codeCells (applyRestart RestartOnly nb))
            `shouldBe` map cellOutputs (codeCells nb)

    it "keeps outputs when restarting to run everything again" $
        map (null . cellOutputs) (codeCells (applyRestart RestartRunAll nb))
            `shouldBe` [False, False]

    it "drops outputs only when the user asked to clear them" $
        map (null . cellOutputs) (codeCells (applyRestart RestartClear nb))
            `shouldBe` [True, True]

    it
        "leaves prose alone in every mode: prose has no kernel state to lose,\
        \ and marking it stale would queue it for execution"
        $ mapM_
            ( \mode -> case nbCells (applyRestart mode nb) of
                (p : _) -> cellDirty p `shouldBe` False
                [] -> expectationFailure "expected the prose cell"
            )
            [RestartOnly, RestartRunAll, RestartClear]
