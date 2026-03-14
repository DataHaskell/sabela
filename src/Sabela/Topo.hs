{-# LANGUAGE OverloadedStrings #-}

module Sabela.Topo (
    TopoResult (..),
    buildDefMap,
    buildDepGraph,
    topoSort,
    computeTopoOrder,
    selectAffectedTopo,
    cellNames,
) where

import Data.Char (isAlpha, isAlphaNum, isAsciiUpper)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Model (Cell (..))

data TopoResult = TopoResult
    { trOrdered :: [Cell]
    -- ^ Cells in safe execution order (no redef cells, no cycle cells)
    , trCycleIds :: S.Set Int
    -- ^ Cell IDs that are part of a circular dependency
    }

-- | Extract (definitions, uses) from cell source text.
cellNames :: Text -> (S.Set Text, S.Set Text)
cellNames src = (defs, uses)
  where
    ls = T.lines src
    defs = S.fromList $ concatMap extractDefs ls
    uses = S.fromList $ concatMap extractTokens ls

extractDefs :: Text -> [Text]
extractDefs line
    | isIndented = []
    | T.null s = []
    | T.isPrefixOf "--" s = []
    | T.isPrefixOf ":" s = []
    | T.isPrefixOf "import " s = []
    | T.isPrefixOf "{-#" s = []
    | Just rest <- stripKW "let" s = firstLowerIdent rest
    | Just rest <- stripKW "data" s = firstAnyIdent rest
    | Just rest <- stripKW "type" s = firstAnyIdent rest
    | Just rest <- stripKW "newtype" s = firstAnyIdent rest
    | Just rest <- stripKW "class" s = firstAnyIdent rest
    | otherwise =
        let toks = T.words s
         in case toks of
                (w : rest)
                    | isLowerIdent w
                    , any (\t -> t == "=" || t == "<-") (take 8 rest) ->
                        [w]
                _ -> []
  where
    s = T.strip line
    isIndented = not (T.null line) && (T.head line == ' ' || T.head line == '\t')

stripKW :: Text -> Text -> Maybe Text
stripKW kw t = case T.stripPrefix kw t of
    Just rest
        | T.null rest -> Nothing
        | not (isIdentChar (T.head rest)) -> Just (T.stripStart rest)
    _ -> Nothing

firstLowerIdent :: Text -> [Text]
firstLowerIdent t =
    let w = T.takeWhile isIdentChar (T.stripStart t)
     in [w | isLowerIdent w]

firstAnyIdent :: Text -> [Text]
firstAnyIdent t =
    let w = T.takeWhile isIdentChar (T.stripStart t)
     in [w | not (T.null w), isAlpha (T.head w) || T.head w == '_']

isLowerIdent :: Text -> Bool
isLowerIdent t =
    not (T.null t)
        && let c = T.head t in (isAlpha c && not (isAsciiUpper c)) || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''

extractTokens :: Text -> [Text]
extractTokens = filter isIdent . T.split (not . isIdentChar)
  where
    isIdent t = not (T.null t) && (isAlpha (T.head t) || T.head t == '_')

{- | Build defMap (name → first cell ID) and redefMap (later cell ID → [redefined names]).
First-wins: the first cell in notebook order to define a name is canonical.
Later cells that try to define the same name are collected in redefMap.
-}
buildDefMap :: [Cell] -> (M.Map Text Int, M.Map Int [Text])
buildDefMap = foldl step (M.empty, M.empty)
  where
    step (defMap, redefMap) cell =
        let cid = cellId cell
            (defs, _) = cellNames (cellSource cell)
            (newDefMap, redefs) = S.foldl' (processName cid) (defMap, []) defs
            newRedefMap =
                if null redefs
                    then redefMap
                    else M.insert cid redefs redefMap
         in (newDefMap, newRedefMap)

    processName cid (defMap, redefs) name
        | M.member name defMap = (defMap, name : redefs)
        | otherwise = (M.insert name cid defMap, redefs)

{- | Build dependency graph: cell ID → set of cell IDs it depends on.
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

{- | Kahn's topological sort.
Only processes the provided cells; deps to cells outside this set are ignored
(treated as already satisfied). Leftover cells with unresolved in-degree are cycles.
-}
topoSort :: [Cell] -> M.Map Int (S.Set Int) -> TopoResult
topoSort cells deps =
    let cids = S.fromList (map cellId cells)

        -- restrict deps to only within this cell set
        filteredDeps =
            M.fromList
                [ (cellId c, S.intersection cids (M.findWithDefault S.empty (cellId c) deps))
                | c <- cells
                ]

        -- reverse graph: dep → set of cells that depend on it
        revDeps =
            M.fromListWith
                S.union
                [ (dep, S.singleton cid)
                | (cid, depSet) <- M.toList filteredDeps
                , dep <- S.toList depSet
                ]

        -- in-degree for each cell (number of dependencies within the set)
        inDegree =
            M.fromList
                [ (cellId c, S.size (M.findWithDefault S.empty (cellId c) filteredDeps))
                | c <- cells
                ]

        -- notebook position for stable tiebreaking
        positions = M.fromList (zip (map cellId cells) [0 :: Int ..])

        -- cell lookup by ID
        cellById = M.fromList [(cellId c, c) | c <- cells]

        go [] deg acc = (acc, deg)
        go queue deg acc =
            let cid = minimumByPos queue positions
                queue' = filter (/= cid) queue
                dependents = S.toList (M.findWithDefault S.empty cid revDeps)
                (newlyUnblocked, deg') = foldl unblock ([], deg) dependents
                queue'' = queue' ++ newlyUnblocked
                cell = cellById M.! cid
             in go queue'' deg' (acc ++ [cell])

        unblock (unblocked, d) depCid =
            let d' = M.adjust (subtract 1) depCid d
                newDeg = M.findWithDefault 0 depCid d'
             in if newDeg == 0 then (depCid : unblocked, d') else (unblocked, d')

        initQueue = [cellId c | c <- cells, M.findWithDefault 0 (cellId c) inDegree == 0]
        (ordered, finalDeg) = go initQueue inDegree []
        cycleIds = S.fromList [cid | cid <- S.toList cids, M.findWithDefault 0 cid finalDeg > 0]
     in TopoResult
            { trOrdered = ordered
            , trCycleIds = cycleIds
            }

minimumByPos :: [Int] -> M.Map Int Int -> Int
minimumByPos xs positions =
    foldl1 (\a b -> if pos a <= pos b then a else b) xs
  where
    pos x = M.findWithDefault maxBound x positions

{- | Compose buildDefMap, buildDepGraph, topoSort.
Redefining cells are excluded from trOrdered (they appear only in redefMap).
-}
computeTopoOrder :: [Cell] -> (TopoResult, M.Map Int [Text])
computeTopoOrder cells =
    let (defMap, redefMap) = buildDefMap cells
        deps = buildDepGraph defMap cells
        toSort = filter (\c -> not (M.member (cellId c) redefMap)) cells
        result = topoSort toSort deps
     in (result, redefMap)

{- | Find cells transitively downstream of editedCid and return a scoped TopoResult.
The edited cell itself is included in the result.
-}
selectAffectedTopo :: Int -> [Cell] -> (TopoResult, M.Map Int [Text])
selectAffectedTopo editedCid cells =
    let (defMap, redefMap) = buildDefMap cells
        deps = buildDepGraph defMap cells

        -- reverse dep graph: cell → set of cells that depend on it
        revDeps =
            M.fromListWith
                S.union
                [ (dep, S.singleton cid)
                | (cid, depSet) <- M.toList deps
                , dep <- S.toList depSet
                ]

        affected = bfsAffected (S.singleton editedCid) (S.singleton editedCid) revDeps

        toSort =
            filter
                (\c -> S.member (cellId c) affected && not (M.member (cellId c) redefMap))
                cells

        result = topoSort toSort deps
     in (result, redefMap)

bfsAffected :: S.Set Int -> S.Set Int -> M.Map Int (S.Set Int) -> S.Set Int
bfsAffected visited frontier revDeps
    | S.null frontier = visited
    | otherwise =
        let next = S.unions [M.findWithDefault S.empty n revDeps | n <- S.toList frontier]
            newNext = S.difference next visited
         in bfsAffected (S.union visited newNext) newNext revDeps
