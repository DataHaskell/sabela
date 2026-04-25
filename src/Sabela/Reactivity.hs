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

{- | Human-readable explanation of a detected cycle. The message names both the
cells involved (by 1-based position) AND the specific variables that create
the mutual references, so the agent can jump straight to the offending
identifiers instead of inspecting every line.

If @cells@ / @defMap@ are passed, the variable list is included; otherwise
the fallback message only lists cell positions. The positional fallback is
kept for any older call sites that haven't been updated yet.
-}
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

{- | Identify the set of variable names that connect cells in a cycle. A name
@x@ is "cycle-forming" if it is defined in one cycle cell and used by another
cycle cell. Order in output is lexicographic for determinism.
-}
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
