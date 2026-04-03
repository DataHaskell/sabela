{-# LANGUAGE OverloadedStrings #-}

module Sabela.Reactivity (
    ExecutionPlan (..),
    computeExecutionPlan,
    computeFullExecutionPlan,
    haskellCodeCells,
    cellPositionMap,
    redefinitionErrorMsg,
    cycleErrorMsg,
) where

import Data.Containers.ListUtils (nubOrdOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import qualified Sabela.SessionTypes as ST
import qualified Sabela.Topo as Topo

data ExecutionPlan = ExecutionPlan
    { epCellsToRun :: [Cell]
    -- ^ Cells in dependency order that should be executed.
    , epCycleIds :: S.Set Int
    -- ^ Cell IDs that are part of circular dependencies (should get error).
    , epRedefErrors :: M.Map Int [Text]
    -- ^ Cell ID → list of names that conflict with earlier definitions.
    , epDefMap :: M.Map Text Int
    -- ^ Name → cell ID of the first definition (for error messages).
    , epCellPositions :: M.Map Int Int
    -- ^ Cell ID → 1-based position in the notebook (for error messages).
    }

computeExecutionPlan :: Int -> [Cell] -> Notebook -> ExecutionPlan
computeExecutionPlan editedCid allCode nb =
    let (defMap, _) = Topo.buildDefMap allCode
        posMap = cellPositionMap nb
        (topoResult, redefMap) = Topo.selectAffectedTopo editedCid allCode
        skipIds = Topo.trCycleIds topoResult `S.union` M.keysSet redefMap
        toRun =
            filter (\c -> not (S.member (cellId c) skipIds)) (Topo.trOrdered topoResult)
     in ExecutionPlan
            { epCellsToRun = toRun
            , epCycleIds = Topo.trCycleIds topoResult
            , epRedefErrors = redefMap
            , epDefMap = defMap
            , epCellPositions = posMap
            }

computeFullExecutionPlan :: [Cell] -> Notebook -> ExecutionPlan
computeFullExecutionPlan allCode nb =
    let (defMap, _) = Topo.buildDefMap allCode
        posMap = cellPositionMap nb
        (topoResult, redefMap) = Topo.computeTopoOrder allCode
        skipIds = Topo.trCycleIds topoResult `S.union` M.keysSet redefMap
        toRun =
            nubOrdOn cellId $
                filter (\c -> not (S.member (cellId c) skipIds)) (Topo.trOrdered topoResult)
     in ExecutionPlan
            { epCellsToRun = toRun
            , epCycleIds = Topo.trCycleIds topoResult
            , epRedefErrors = redefMap
            , epDefMap = defMap
            , epCellPositions = posMap
            }

haskellCodeCells :: Notebook -> [Cell]
haskellCodeCells nb =
    filter (\c -> cellType c == CodeCell && cellLang c == ST.Haskell) (nbCells nb)

cellPositionMap :: Notebook -> M.Map Int Int
cellPositionMap nb =
    M.fromList (zip (map cellId (nbCells nb)) [1 ..])

redefinitionErrorMsg ::
    -- | defMap: name → first-defining cell ID
    M.Map Text Int ->
    -- | posMap: cell ID → 1-based position
    M.Map Int Int ->
    -- | cell ID with the redefinition
    Int ->
    -- | names that are redefined
    [Text] ->
    Text
redefinitionErrorMsg defMap posMap _cid names =
    let msgs =
            [ "'"
                <> name
                <> "' is already defined in cell "
                <> T.pack (show (M.findWithDefault origCid origCid posMap))
                <> " (which takes precedence)"
            | name <- names
            , Just origCid <- [M.lookup name defMap]
            ]
     in "Duplicate definition"
            <> (if length names > 1 then "s" else "")
            <> ": "
            <> T.intercalate "; " msgs
            <> ". Remove the duplicate to resolve this conflict."

cycleErrorMsg :: M.Map Int Int -> S.Set Int -> Text
cycleErrorMsg posMap cycleIds =
    let cids = S.toList cycleIds
        cycleMsg =
            T.intercalate
                ", "
                (map (\c -> T.pack (show (M.findWithDefault c c posMap))) cids)
     in "This cell is part of a circular dependency and cannot be executed."
            <> " Cells in the cycle: ["
            <> cycleMsg
            <> "]."
