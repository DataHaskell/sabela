module Sabela.Reactivity (
    EnvState (..),
    ExecutionPlan (..),
    computeExecutionPlanIn,
    computeStaleExecutionPlanIn,
    ModuleState (..),
    computeRootedExecutionPlan,
    bridgeConsumers,
    changedBridgeValues,
    cellStale,
    cellSettled,
    clearCellResult,
    runAllNeedsRun,
    computeExecutionPlan,
    markDependentsDirty,
    markAllDirty,
    RestartMode (..),
    applyRestart,
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
import Data.Maybe (isNothing)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Bridge (bridgeIdentifier)
import Sabela.Compiled (
    CompilePlan (..),
    compiledRootExpansion,
    planCompiledModules,
    pruneIntraModuleDeps,
 )
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.Reactivity.Errors (
    cellPositionMap,
    cycleErrorMsg,
    redefinitionErrorMsg,
 )
import qualified Sabela.SessionTypes as ST
import qualified Sabela.Topo as Topo

{- | Whether the running kernel is still the one the notebook needs. The planner
is pure, so this is decided in IO by @envStale@ and passed in.
-}
data EnvState = EnvFresh | EnvStale
    deriving (Eq, Show)

{- | Whether the next compile will @:load@, which wipes every interpreted
binding in the kernel. Known before running rather than discovered after, so the
cells it invalidates are roots of the same plan instead of a second pass.
-}
data ModuleState = ModulesLoaded | ModulesWiped
    deriving (Eq, Show)

data ExecutionPlan = ExecutionPlan
    { epRunEnv :: Bool
    , epCellsToRun :: [Cell]
    , epCompileCells :: [Cell]
    , epCompilePlan :: CompilePlan
    , epCycleIds :: S.Set Int
    , epRedefErrors :: M.Map Int [Text]
    , epDefMap :: M.Map Text Int
    , epCellPositions :: M.Map Int Int
    }

computeExecutionPlan :: Int -> [Cell] -> Notebook -> ExecutionPlan
computeExecutionPlan editedCid =
    computePlanCore EnvFresh ModulesLoaded (Just (S.singleton editedCid))

computeExecutionPlanIn ::
    EnvState -> ModuleState -> Int -> [Cell] -> Notebook -> ExecutionPlan
computeExecutionPlanIn env mods editedCid =
    computePlanCore env mods (Just (S.singleton editedCid))

computeFullExecutionPlan :: [Cell] -> Notebook -> ExecutionPlan
computeFullExecutionPlan = computePlanCore EnvFresh ModulesLoaded Nothing

-- | A plan rooted at an explicit set of cells, expanded by the usual reachability.
computeRootedExecutionPlan ::
    EnvState -> ModuleState -> S.Set Int -> [Cell] -> Notebook -> ExecutionPlan
computeRootedExecutionPlan env mods roots = computePlanCore env mods (Just roots)

{- | Does this cell need running? A failure is /settled/, not stale: re-running
unchanged inputs only reproduces it, so an errored cell stops being an automatic
execution root until its source changes. Contrast 'cellSettled'.
-}
cellStale :: Cell -> Bool
cellStale = cellDirty

{- | Does the kernel hold what this cell claims? True only for a cell that ran to
completion without error, so unlike 'cellStale' a failure counts against it.
-}
cellSettled :: Cell -> Bool
cellSettled c = not (cellDirty c) && isNothing (cellError c)

{- | Drop whatever a cell was showing. A code cell with no result cannot be
current, so it comes back invalidated; prose has nothing to run and stays clean.
Shared by Clear, Reset and a language switch, which all discard a result.
-}
clearCellResult :: Cell -> Cell
clearCellResult c =
    c
        { cellOutputs = []
        , cellError = Nothing
        , cellDirty = cellType c == CodeCell
        }

{- | A rebuild counts as work even when no cell is stale, so a notebook whose
environment changed is never reported as having nothing to do.
-}
runAllNeedsRun :: Bool -> Bool -> [Cell] -> Notebook -> Bool
runAllNeedsRun building ready allCode nb
    | building = False
    | otherwise = epRunEnv plan || not (null (epCellsToRun plan))
  where
    plan = computeStaleExecutionPlanIn (envStateOf ready) ModulesLoaded allCode nb

envStateOf :: Bool -> EnvState
envStateOf ready = if ready then EnvFresh else EnvStale

computeStaleExecutionPlan :: [Cell] -> Notebook -> ExecutionPlan
computeStaleExecutionPlan = computeStaleExecutionPlanIn EnvFresh ModulesLoaded

computeStaleExecutionPlanIn ::
    EnvState -> ModuleState -> [Cell] -> Notebook -> ExecutionPlan
computeStaleExecutionPlanIn env mods allCode =
    computePlanCore
        env
        mods
        (Just (S.fromList (map cellId (filter cellStale allCode))))
        allCode

{- | Replacing the kernel invalidates every binding it held, so a stale
environment makes every code cell a root. Explicit roots that reach no code
(a prose edit) open no door: nothing runs and nothing rebuilds.
-}
computePlanCore ::
    EnvState ->
    ModuleState ->
    Maybe (S.Set Int) ->
    [Cell] ->
    Notebook ->
    ExecutionPlan
computePlanCore env mods mRoots allCode nb =
    let posMap = cellPositionMap nb
        cplan = planCompiledModules posMap allCode
        (defMap, redefMap) = Topo.buildDefMap allCode
        deps =
            pruneIntraModuleDeps
                (cpCellModule cplan)
                (Topo.buildDepGraph defMap allCode)
        revDeps = Topo.reverseDeps deps
        allIds = S.fromList (map cellId allCode)
        reachOf roots =
            let affected0 = Topo.reachableFrom roots revDeps
                roots' = roots `S.union` compiledRootExpansion cplan affected0
             in Topo.reachableFrom roots' revDeps
        deadRooted = case mRoots of
            Just roots ->
                not (S.null roots)
                    && S.null (reachOf roots `S.intersection` allIds)
            Nothing -> False
        affected
            | deadRooted = S.empty
            | otherwise = case (env, mods, mRoots) of
                (EnvStale, _, _) -> allIds
                (_, ModulesWiped, _) -> allIds
                (_, _, Nothing) -> allIds
                (_, _, Just roots) -> reachOf roots
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
            { epRunEnv = env == EnvStale && not deadRooted
            , epCellsToRun = interp
            , epCompileCells = compiledCells
            , epCompilePlan = cplan
            , epCycleIds = Topo.trCycleIds topoResult
            , epRedefErrors = redefMap
            , epDefMap = defMap
            , epCellPositions = posMap
            }

{- | Which exported values differ between two snapshots of the bridge store,
counting additions and removals. Per value, so re-exporting one name does not
invalidate consumers of the others.
-}
changedBridgeValues :: M.Map Text Text -> M.Map Text Text -> S.Set Text
changedBridgeValues before after =
    M.keysSet (M.differenceWith drop' before after)
        `S.union` M.keysSet (M.difference after before)
  where
    drop' old new = if old == new then Nothing else Just old

{- | Cells that use any of these bridge values. Uses come from the parser, so a
mention inside a string or a comment is not a dependency — the same rule every
other edge in the graph is built from.
-}
bridgeConsumers :: S.Set Text -> [Cell] -> S.Set Int
bridgeConsumers changed cells
    | S.null changed = S.empty
    | otherwise =
        S.fromList
            [ cellId c
            | c <- cells
            , let (_, uses) = Topo.cellNames (cellSource c)
            , not (S.disjoint uses wanted)
            ]
  where
    wanted = S.map bridgeIdentifier changed

{- | Invalidate everything a kernel could have held. Prose is excluded because
it never runs: marking it produced a cell that reported itself out of date
forever, since nothing ever cleared it.
-}
markAllDirty :: Notebook -> Notebook
markAllDirty nb = nb{nbCells = map dirty (nbCells nb)}
  where
    dirty c
        | cellType c == CodeCell = c{cellDirty = True}
        | otherwise = c

{- | Which restart the user asked for. All three respawn the kernel; they differ
in whether cells re-run afterwards and whether their outputs survive.
-}
data RestartMode = RestartOnly | RestartRunAll | RestartClear
    deriving (Eq, Show)

{- | Every mode invalidates each code cell, because the kernel comes back empty
and nothing it held is still current. Only 'RestartClear' discards outputs.
-}
applyRestart :: RestartMode -> Notebook -> Notebook
applyRestart RestartClear nb = nb{nbCells = map clearCellResult (nbCells nb)}
applyRestart _ nb = markAllDirty nb

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

escalatedCellsToRun :: Notebook -> [Cell]
escalatedCellsToRun nb =
    epCellsToRun (computeFullExecutionPlan (haskellCodeCells nb) nb)

haskellCodeCells :: Notebook -> [Cell]
haskellCodeCells nb =
    filter (\c -> cellType c == CodeCell && cellLang c == ST.Haskell) (nbCells nb)
