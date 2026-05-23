{-# LANGUAGE OverloadedStrings #-}

{- | Shared notebook-graph analysis for the exporters.

Both the static pipeline exporter ("Sabela.Export") and the (future) reactive
exporter need the same view of a notebook: its Haskell code cells in document
order, the dependency graph between them, which cells redefine earlier names,
which cells instantiate widgets, and which cells are downstream of a widget.
This module computes that once.
-}
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
    -- ^ Haskell code cells in document order.
    , ngDefMap :: M.Map Text Int
    -- ^ Name → id of the cell that canonically defines it (first-wins).
    , ngDepGraph :: M.Map Int (S.Set Int)
    -- ^ Cell id → set of cell ids it depends on.
    , ngRedefIds :: Set Int
    -- ^ Cells flagged as redefinitions; excluded from slices.
    , ngWidgetCells :: Set Int
    -- ^ Cells that instantiate a widget constructor.
    , ngReactiveSet :: Set Int
    -- ^ Cells transitively downstream of a widget cell (the widget cells too).
    }
    deriving (Show, Eq)

-- | Widget constructors from the Sabela display prelude (see "Sabela.Output").
widgetConstructors :: Set Text
widgetConstructors =
    S.fromList ["slider", "dropdown", "checkbox", "textInput", "button"]

-- | Compute the dependency graph and widget/reactive partition for a notebook.
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

{- | The pipeline that produces a target cell: the target plus its transitive
dependencies, in document order, excluding redefinition-flagged cells. If the
target is not a Haskell code cell, the slice is empty.
-}
backwardSlice :: Int -> NotebookGraph -> [Cell]
backwardSlice target ng =
    let reach = Topo.reachableFrom (S.singleton target) (ngDepGraph ng)
        keep c = S.member (cellId c) reach && not (S.member (cellId c) (ngRedefIds ng))
     in filter keep (ngOrdered ng)
