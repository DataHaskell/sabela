{-# LANGUAGE OverloadedStrings #-}

module Sabela.Reactivity (
    ExecutionPlan (..),
    cellStale,
    runAllNeedsRun,
    computeExecutionPlan,
    markDependentsDirty,
    markAllDirty,
    markAllInterpretedDirty,
    computeFullExecutionPlan,
    computeStaleExecutionPlan,
    escalatedCellsToRun,
    haskellCodeCells,
    cellPositionMap,
    redefinitionErrorMsg,
    cycleErrorMsg,
) where

import Data.Containers.ListUtils (nubOrdOn)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Compiled (
    CompilePlan (..),
    compiledRootExpansion,
    planCompiledModules,
    pruneIntraModuleDeps,
 )
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import qualified Sabela.SessionTypes as ST
import qualified Sabela.Topo as Topo

data ExecutionPlan = ExecutionPlan
    { epCellsToRun :: [Cell]
    , epCompileCells :: [Cell]
    , epCompilePlan :: CompilePlan
    , epCycleIds :: S.Set Int
    , epRedefErrors :: M.Map Int [Text]
    , epDefMap :: M.Map Text Int
    , epCellPositions :: M.Map Int Int
    }

computeExecutionPlan :: Int -> [Cell] -> Notebook -> ExecutionPlan
computeExecutionPlan editedCid = computePlanCore (Just (S.singleton editedCid))

computeFullExecutionPlan :: [Cell] -> Notebook -> ExecutionPlan
computeFullExecutionPlan = computePlanCore Nothing

cellStale :: Cell -> Bool
cellStale c = cellDirty c || isJust (cellError c)

runAllNeedsRun :: Bool -> Bool -> [Cell] -> Notebook -> Bool
runAllNeedsRun building ready allCode nb
    | building = False
    | not ready = True
    | otherwise = not (null (epCellsToRun (computeStaleExecutionPlan allCode nb)))

computeStaleExecutionPlan :: [Cell] -> Notebook -> ExecutionPlan
computeStaleExecutionPlan allCode =
    computePlanCore
        (Just (S.fromList (map cellId (filter cellStale allCode))))
        allCode

computePlanCore :: Maybe (S.Set Int) -> [Cell] -> Notebook -> ExecutionPlan
computePlanCore mRoots allCode nb =
    let posMap = cellPositionMap nb
        cplan = planCompiledModules posMap allCode
        (defMap, redefMap) = Topo.buildDefMap allCode
        deps =
            pruneIntraModuleDeps
                (cpCellModule cplan)
                (Topo.buildDepGraph defMap allCode)
        revDeps = Topo.reverseDeps deps
        allIds = S.fromList (map cellId allCode)
        affected = case mRoots of
            Nothing -> allIds
            Just roots ->
                let affected0 = Topo.reachableFrom roots revDeps
                    roots' = roots `S.union` compiledRootExpansion cplan affected0
                 in Topo.reachableFrom roots' revDeps
        toSort = filter (\c -> S.member (cellId c) affected) allCode
        topoResult = Topo.topoSort toSort deps
        skipIds =
            Topo.trCycleIds topoResult
                `S.union` M.keysSet redefMap
                `S.union` M.keysSet (cpViolations cplan)
        isCompiledId cid = M.member cid (cpCellModule cplan)
        keep c = not (S.member (cellId c) skipIds)
        interp =
            nubOrdOn cellId $
                filter
                    (\c -> keep c && not (isCompiledId (cellId c)))
                    (Topo.trOrdered topoResult)
        compiledCells =
            [ c
            | c <- allCode
            , S.member (cellId c) affected
            , isCompiledId (cellId c)
            , keep c
            ]
     in ExecutionPlan
            { epCellsToRun = interp
            , epCompileCells = compiledCells
            , epCompilePlan = cplan
            , epCycleIds = Topo.trCycleIds topoResult
            , epRedefErrors = redefMap
            , epDefMap = defMap
            , epCellPositions = posMap
            }

markAllDirty :: Notebook -> Notebook
markAllDirty nb = nb{nbCells = map dirty (nbCells nb)}
  where
    dirty c = c{cellDirty = True}

markDependentsDirty :: Int -> Notebook -> Notebook
markDependentsDirty cid nb =
    let code = haskellCodeCells nb
        (defMap, _) = Topo.buildDefMap code
        deps = Topo.buildDepGraph defMap code
        affected = Topo.reachableFrom (S.singleton cid) (Topo.reverseDeps deps)
        markIds = S.delete cid affected
        upd c
            | S.member (cellId c) markIds = c{cellDirty = True}
            | otherwise = c
     in nb{nbCells = map upd (nbCells nb)}

markAllInterpretedDirty :: Notebook -> Notebook
markAllInterpretedDirty nb =
    let code = haskellCodeCells nb
        cplan = planCompiledModules (cellPositionMap nb) code
        interpIds =
            S.fromList
                [ cellId c
                | c <- code
                , not (M.member (cellId c) (cpCellModule cplan))
                ]
        upd c
            | S.member (cellId c) interpIds = c{cellDirty = True}
            | otherwise = c
     in nb{nbCells = map upd (nbCells nb)}

escalatedCellsToRun :: Notebook -> [Cell]
escalatedCellsToRun nb =
    epCellsToRun (computeFullExecutionPlan (haskellCodeCells nb) nb)

haskellCodeCells :: Notebook -> [Cell]
haskellCodeCells nb =
    filter (\c -> cellType c == CodeCell && cellLang c == ST.Haskell) (nbCells nb)

cellPositionMap :: Notebook -> M.Map Int Int
cellPositionMap nb =
    M.fromList (zip (map cellId (nbCells nb)) [1 ..])

redefinitionErrorMsg ::
    M.Map Text Int ->
    M.Map Int Int ->
    Int ->
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

cycleErrorMsg ::
    M.Map Int Int ->
    S.Set Int ->
    [Cell] ->
    M.Map Text Int ->
    Text
cycleErrorMsg posMap cycleIds cells defMap =
    let cids = S.toList cycleIds
        positions =
            map (\c -> T.pack (show (M.findWithDefault c c posMap))) cids
        cycleList = T.intercalate ", " positions
        vars = cycleVariables cycleIds cells defMap
        varLine =
            if null vars
                then ""
                else
                    " Variables forming the cycle: {"
                        <> T.intercalate ", " vars
                        <> "}."
     in "This cell is part of a circular dependency and cannot execute. "
            <> "Cells in the cycle (by position): ["
            <> cycleList
            <> "]."
            <> varLine
            <> " To resolve: (1) rename one of those variables in the cell that"
            <> " introduces the loop, (2) delete one of the mutually-referencing"
            <> " cells, or (3) merge the definitions into a single cell."
            <> " Tokens inside string literals / comments are NOT counted, so"
            <> " this is a real reference loop in the code."

cycleVariables :: S.Set Int -> [Cell] -> M.Map Text Int -> [Text]
cycleVariables cycleIds cells defMap =
    let cellById = M.fromList [(cellId c, c) | c <- cells]
        cycleCells = [c | cid <- S.toList cycleIds, Just c <- [M.lookup cid cellById]]
        nameCreatesCycleEdge name =
            case M.lookup name defMap of
                Nothing -> False
                Just definerCid -> S.member definerCid cycleIds
        namesForCell c =
            let (_, uses) = Topo.cellNames (cellSource c)
             in S.filter
                    ( \n ->
                        nameCreatesCycleEdge n
                            && M.lookup n defMap /= Just (cellId c)
                    )
                    uses
        allVars = S.unions (map namesForCell cycleCells)
     in S.toAscList allVars
