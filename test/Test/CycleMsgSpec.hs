{-# LANGUAGE OverloadedStrings #-}

module Test.CycleMsgSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.Model (Cell (..), CellType (..))
import Sabela.Reactivity (cycleErrorMsg)
import Sabela.SessionTypes (CellLang (..))
import qualified Sabela.Topo as Topo

mkCell :: Int -> Text -> Cell
mkCell cid src =
    Cell
        { cellId = cid
        , cellType = CodeCell
        , cellLang = Haskell
        , cellSource = src
        , cellOutputs = []
        , cellError = Nothing
        , cellDirty = False
        }

spec :: Spec
spec = do
    describe "cycleErrorMsg (B: actionable cycle messages)" $ do
        it "names the cells by position" $ do
            let cells = [mkCell 10 "a = b", mkCell 20 "b = a"]
                posMap = M.fromList [(10, 1), (20, 2)]
                (defMap, _) = Topo.buildDefMap cells
                cycleIds = S.fromList [10, 20]
                msg = cycleErrorMsg posMap cycleIds cells defMap
            T.isInfixOf "1" msg `shouldBe` True
            T.isInfixOf "2" msg `shouldBe` True

        it "names the variables forming the cycle" $ do
            let cells = [mkCell 10 "alpha = beta", mkCell 20 "beta = alpha"]
                posMap = M.fromList [(10, 1), (20, 2)]
                (defMap, _) = Topo.buildDefMap cells
                cycleIds = S.fromList [10, 20]
                msg = cycleErrorMsg posMap cycleIds cells defMap
            T.isInfixOf "alpha" msg `shouldBe` True
            T.isInfixOf "beta" msg `shouldBe` True
            T.isInfixOf "Variables" msg `shouldBe` True

        it "suggests concrete resolutions" $ do
            let cells = [mkCell 1 "x = y", mkCell 2 "y = x"]
                posMap = M.fromList [(1, 1), (2, 2)]
                (defMap, _) = Topo.buildDefMap cells
                msg = cycleErrorMsg posMap (S.fromList [1, 2]) cells defMap
            T.isInfixOf "rename" (T.toLower msg) `shouldBe` True
            T.isInfixOf "delete" (T.toLower msg) `shouldBe` True
            T.isInfixOf "merge" (T.toLower msg) `shouldBe` True

        it "omits the Variables line when no cycle-forming names are found" $ do
            -- Edge case: empty cycle set → message should still be well-formed.
            let msg = cycleErrorMsg M.empty S.empty [] M.empty
            T.isInfixOf "cycle" (T.toLower msg) `shouldBe` True
            T.isInfixOf "Variables" msg `shouldBe` False
