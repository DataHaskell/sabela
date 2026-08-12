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
    cellSymbols,
) where

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Text (Text)
import Sabela.Model (Cell (..))
import Sabela.Parse (CellSymbols (..), cellNames, cellSymbols)

data TopoResult = TopoResult
    { trOrdered :: [Cell]
    , trCycleIds :: S.Set Int
    }

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

{- | Edges: def\/use ownership, notebook-class method providers, instance
providers for a used type, and mutators of a used binding. Providers of a kind
never depend on fellow providers of the same name, or they would form cycles.
-}
buildDepGraph :: M.Map Text Int -> [Cell] -> M.Map Int (S.Set Int)
buildDepGraph defMap cells = M.fromList [(cellId c, depsOf c) | c <- cells]
  where
    symsById = M.fromList [(cellId c, cellSymbols (cellSource c)) | c <- cells]
    notebookMethods = S.unions [csClassMethods s | s <- M.elems symsById]
    providerMap =
        M.fromListWith
            S.union
            [ (name, S.singleton cid)
            | (cid, s) <- M.toList symsById
            , name <- S.toList (csProvides s `S.intersection` notebookMethods)
            ]
    mutatorMap =
        M.fromListWith
            S.union
            [ (name, S.singleton cid)
            | (cid, s) <- M.toList symsById
            , name <- S.toList (csMutates s)
            ]
    instanceMap =
        M.fromListWith
            S.union
            [ (name, S.singleton cid)
            | (cid, s) <- M.toList symsById
            , name <- S.toList (csInstanceTypes s)
            ]
    depsOf c =
        let s = symsById M.! cellId c
            cid = cellId c
            ownerDeps =
                [depCid | name <- S.toList (csUses s), Just depCid <- [M.lookup name defMap]]
            instanceDeps =
                [ pc
                | name <- S.toList (csUses s)
                , pc <- S.toList (M.findWithDefault S.empty name providerMap)
                ]
            mutationDeps =
                providersOf mutatorMap (csMutates s) (csUses s)
            typeInstanceDeps =
                providersOf instanceMap (csInstanceTypes s) (csUses s)
         in S.delete cid $
                S.fromList
                    (ownerDeps ++ instanceDeps ++ mutationDeps ++ typeInstanceDeps)

-- | Cells providing @name@ for each used name, unless this cell provides it too.
providersOf :: M.Map Text (S.Set Int) -> S.Set Text -> S.Set Text -> [Int]
providersOf providers ownProvides uses =
    [ pc
    | name <- S.toList uses
    , not (S.member name ownProvides)
    , pc <- S.toList (M.findWithDefault S.empty name providers)
    ]

data TopoState = TopoState
    { tsFilteredDeps :: M.Map Int (S.Set Int)
    , tsRevDeps :: M.Map Int (S.Set Int)
    , tsPositions :: M.Map Int Int
    , tsCellById :: M.Map Int Cell
    }

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

computeTopoOrder :: [Cell] -> (TopoResult, M.Map Int [Text])
computeTopoOrder cells =
    let (defMap, redefMap) = buildDefMap cells
        deps = buildDepGraph defMap cells
        result = topoSort cells deps
     in (result, redefMap)

selectAffectedTopo :: Int -> [Cell] -> (TopoResult, M.Map Int [Text])
selectAffectedTopo editedCid = selectAffectedTopoFrom (S.singleton editedCid)

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

reachableFrom :: S.Set Int -> M.Map Int (S.Set Int) -> S.Set Int
reachableFrom seeds = bfsAffected seeds seeds

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
