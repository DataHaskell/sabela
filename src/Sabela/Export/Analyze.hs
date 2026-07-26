{-# LANGUAGE OverloadedStrings #-}

module Sabela.Export.Analyze (
    NotebookGraph (..),
    buildNotebookGraph,
    backwardSlice,
    widgetConstructors,
) where

import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)

import Sabela.Model (Cell (..), Notebook)
import Sabela.Parse (cellNames)
import Sabela.Reactivity (haskellCodeCells)
import qualified Sabela.Topo as Topo

data NotebookGraph = NotebookGraph
    { ngOrdered :: [Cell]
    , ngDefMap :: M.Map Text Int
    , ngDepGraph :: M.Map Int (S.Set Int)
    , ngRedefIds :: Set Int
    , ngWidgetCells :: Set Int
    , ngReactiveSet :: Set Int
    }
    deriving (Eq, Show)

widgetConstructors :: Set Text
widgetConstructors =
    S.fromList ["slider", "dropdown", "checkbox", "textInput", "button"]

buildNotebookGraph :: Notebook -> NotebookGraph
buildNotebookGraph nb =
    let cells = haskellCodeCells nb
        (defMap, redefMap) = Topo.buildDefMap cells
        depGraph = Topo.buildDepGraph defMap cells
        widgetCells =
            S.fromList
                [ cellId c
                | c <- cells
                , let (_, uses) = cellNames (cellSource c)
                , not (S.null (S.intersection uses widgetConstructors))
                ]
        reactiveSet = Topo.reachableFrom widgetCells (Topo.reverseDeps depGraph)
     in NotebookGraph
            { ngOrdered = cells
            , ngDefMap = defMap
            , ngDepGraph = depGraph
            , ngRedefIds = M.keysSet redefMap
            , ngWidgetCells = widgetCells
            , ngReactiveSet = reactiveSet
            }

backwardSlice :: Int -> NotebookGraph -> [Cell]
backwardSlice target ng =
    let reach = Topo.reachableFrom (S.singleton target) (ngDepGraph ng)
        keep c = S.member (cellId c) reach && not (S.member (cellId c) (ngRedefIds ng))
     in filter keep (ngOrdered ng)
