module Sabela.Topo (
    TopoResult (..),
    buildDefMap,
    buildDepGraph,
    topoSort,
    computeTopoOrder,
    selectAffectedTopo,
    selectAffectedTopoFrom,
    computeAffectedSet,
    reachableFrom,
    reverseDeps,
    cellNames,
) where

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Text (Text)
import Sabela.Model (Cell (..))
import Sabela.Parse (cellNames)

data TopoResult = TopoResult
    { trOrdered :: [Cell]
    -- ^ Cells in safe execution order (no redef cells, no cycle cells)
    , trCycleIds :: S.Set Int
    -- ^ Cell IDs that are part of a circular dependency
    }

{- | Build @defMap@ = name → canonical-cell-id and a @redefMap@ identifying
cells that try to redefine names already owned. Uses **first-wins**
semantics: the EARLIEST cell (in notebook order) to define a name owns
it. Any later cell whose @defs@ overlap with already-owned names is
flagged in @redefMap@ and contributes nothing to @defMap@ — its non-redef
defs are dropped too, since the cell will be skipped at execution time
and those names would never actually be bound in the GHCi session.

The flagged cells are surfaced to the user via 'Sabela.Reactivity.redefinitionErrorMsg'
and skipped by the execution-plan layer (see 'Reactivity.computeExecutionPlan').
This makes a redefinition a hard editor-visible error rather than a silent
shadow — fixing the duplicate is a one-line edit.
-}
buildDefMap :: [Cell] -> (M.Map Text Int, M.Map Int [Text])
buildDefMap = foldl step (M.empty, M.empty)
  where
    step ::
        (M.Map Text Int, M.Map Int [Text]) -> Cell -> (M.Map Text Int, M.Map Int [Text])
    step (defMap, redefMap) cell =
        let cid = cellId cell
            (defs, _) = cellNames (cellSource cell)
            redefs = [n | n <- S.toAscList defs, M.member n defMap]
         in if null redefs
                then
                    let defMap' = S.foldl' (\m n -> M.insert n cid m) defMap defs
                     in (defMap', redefMap)
                else (defMap, M.insert cid redefs redefMap)

{- | Build dependency graph: cell ID -> set of cell IDs it depends on.
Based on defMap: a cell depends on the cell that canonically defines each name it uses.
-}
buildDepGraph :: M.Map Text Int -> [Cell] -> M.Map Int (S.Set Int)
buildDepGraph defMap cells = M.fromList [(cellId c, depsOf c) | c <- cells]
  where
    depsOf c =
        let (_, uses) = cellNames (cellSource c)
            cid = cellId c
         in S.delete cid $
                S.fromList
                    [depCid | name <- S.toList uses, Just depCid <- [M.lookup name defMap]]

data TopoState = TopoState
    { tsFilteredDeps :: M.Map Int (S.Set Int)
    , tsRevDeps :: M.Map Int (S.Set Int)
    , tsPositions :: M.Map Int Int
    , tsCellById :: M.Map Int Cell
    }

{- | Kahn's topological sort.
Only processes the provided cells; deps to cells outside this set are ignored
(treated as already satisfied). Leftover cells with unresolved in-degree are cycles.
-}
topoSort :: [Cell] -> M.Map Int (S.Set Int) -> TopoResult
topoSort cells deps =
    let st = buildTopoState cells deps
        inDegree =
            M.fromList
                [ (cellId c, S.size (M.findWithDefault S.empty (cellId c) (tsFilteredDeps st)))
                | c <- cells
                ]
        initQueue =
            S.fromList
                [ (pos, cellId c)
                | c <- cells
                , M.findWithDefault 0 (cellId c) inDegree == 0
                , let pos = M.findWithDefault maxBound (cellId c) (tsPositions st)
                ]
        cids = S.fromList (map cellId cells)
        (orderedSeq, finalDeg) = runKahns st initQueue inDegree Seq.empty
        cycleIds = S.fromList [cid | cid <- S.toList cids, M.findWithDefault 0 cid finalDeg > 0]
     in TopoResult{trOrdered = F.toList orderedSeq, trCycleIds = cycleIds}

buildTopoState :: [Cell] -> M.Map Int (S.Set Int) -> TopoState
buildTopoState cells deps =
    let cids = S.fromList (map cellId cells)
        filteredDeps =
            M.fromList
                [ (cellId c, S.intersection cids (M.findWithDefault S.empty (cellId c) deps))
                | c <- cells
                ]
        revDeps =
            M.fromListWith
                S.union
                [ (dep, S.singleton cid)
                | (cid, depSet) <- M.toList filteredDeps
                , dep <- S.toList depSet
                ]
     in TopoState
            { tsFilteredDeps = filteredDeps
            , tsRevDeps = revDeps
            , tsPositions = M.fromList (zip (map cellId cells) [0 :: Int ..])
            , tsCellById = M.fromList [(cellId c, c) | c <- cells]
            }

{- | Kahn's main loop. The queue is a 'Set' keyed by @(position, cellId)@
so the lowest-position-first dequeue is O(log n) via 'S.deleteMin'; the
accumulator is a 'Seq' so each snoc is O(1). The old @[Int]@ queue +
@[Cell]@ accumulator were jointly O(N²).
-}
runKahns ::
    TopoState ->
    S.Set (Int, Int) ->
    M.Map Int Int ->
    Seq.Seq Cell ->
    (Seq.Seq Cell, M.Map Int Int)
runKahns st queue deg acc = case S.minView queue of
    Nothing -> (acc, deg)
    Just ((_, cid), queue') ->
        let dependents = M.findWithDefault S.empty cid (tsRevDeps st)
            (newlyUnblocked, deg') = S.foldl' (unblock (tsPositions st)) (S.empty, deg) dependents
            queue'' = queue' `S.union` newlyUnblocked
            cell = tsCellById st M.! cid
         in runKahns st queue'' deg' (acc Seq.|> cell)

unblock ::
    M.Map Int Int ->
    (S.Set (Int, Int), M.Map Int Int) ->
    Int ->
    (S.Set (Int, Int), M.Map Int Int)
unblock positions (unblocked, d) depCid =
    let d' = M.adjust (subtract 1) depCid d
        newDeg = M.findWithDefault 0 depCid d'
        pos = M.findWithDefault maxBound depCid positions
     in if newDeg == 0
            then (S.insert (pos, depCid) unblocked, d')
            else (unblocked, d')

{- | Compose buildDefMap, buildDepGraph, topoSort. The @redefMap@
identifies cells that redefine an earlier cell's names; the execution
layer skips them.
-}
computeTopoOrder :: [Cell] -> (TopoResult, M.Map Int [Text])
computeTopoOrder cells =
    let (defMap, redefMap) = buildDefMap cells
        deps = buildDepGraph defMap cells
        result = topoSort cells deps
     in (result, redefMap)

{- | Find cells transitively downstream of editedCid and return a scoped
TopoResult plus the cell-wide @redefMap@. The edited cell itself is
included in the topo result; redef-flagged cells are filtered out at
execution time by 'Reactivity.computeExecutionPlan'.
-}
selectAffectedTopo :: Int -> [Cell] -> (TopoResult, M.Map Int [Text])
selectAffectedTopo editedCid = selectAffectedTopoFrom (S.singleton editedCid)

{- | Multi-root variant: cells transitively downstream of ANY root (roots
included), as one scoped TopoResult. Used by the stale run-all plan.
-}
selectAffectedTopoFrom :: S.Set Int -> [Cell] -> (TopoResult, M.Map Int [Text])
selectAffectedTopoFrom roots cells =
    let (defMap, redefMap) = buildDefMap cells
        deps = buildDepGraph defMap cells
        affected = reachableFrom roots (reverseDeps deps)
        toSort = filter (\c -> S.member (cellId c) affected) cells
        result = topoSort toSort deps
     in (result, redefMap)

computeAffectedSet :: Int -> M.Map Int (S.Set Int) -> S.Set Int
computeAffectedSet editedCid deps =
    reachableFrom (S.singleton editedCid) (reverseDeps deps)

{- | All cell IDs reachable from @seeds@ by following @graph@ edges (seeds
included). With a forward dep graph this gives a node's transitive
dependencies; with a reversed graph (see 'reverseDeps'), its transitive
dependents.
-}
reachableFrom :: S.Set Int -> M.Map Int (S.Set Int) -> S.Set Int
reachableFrom seeds = bfsAffected seeds seeds

-- | Reverse every edge of a dependency map.
reverseDeps :: M.Map Int (S.Set Int) -> M.Map Int (S.Set Int)
reverseDeps deps =
    M.fromListWith
        S.union
        [ (dep, S.singleton cid)
        | (cid, depSet) <- M.toList deps
        , dep <- S.toList depSet
        ]

bfsAffected :: S.Set Int -> S.Set Int -> M.Map Int (S.Set Int) -> S.Set Int
bfsAffected visited frontier revDeps
    | S.null frontier = visited
    | otherwise =
        let next = S.unions [M.findWithDefault S.empty n revDeps | n <- S.toList frontier]
            newNext = S.difference next visited
         in bfsAffected (S.union visited newNext) newNext revDeps
