{-# LANGUAGE OverloadedStrings #-}

{- | Pure planning for compile mode: which cells are compiled, how they group
into generated modules, the rendered module sources (with cross-module
imports inferred from the dependency graph), and the plan-time violations
that make a cell uncompilable. The IO side lives in
'Sabela.Handlers.Compile'.
-}
module Sabela.Compiled (
    defaultModuleName,
    compiledDirective,
    isCompiledCell,
    CompilePlan (..),
    emptyCompilePlan,
    planCompiledModules,
    moduleFilePath,
    pruneIntraModuleDeps,
    compiledRootExpansion,
) where

import qualified Data.Map.Strict as M
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Deps (sabelaDefaultExts)
import Sabela.Model (Cell (..), CellError (..))
import qualified Sabela.Topo as Topo
import ScriptHs.Compiled (
    CellChunk (..),
    CompileIssue (..),
    checkCompilable,
    isValidModuleName,
    linePragmaTag,
    renderCompiledModule,
 )
import ScriptHs.Parser (
    CabalMeta (..),
    CompileDirective (..),
    ScriptFile (..),
    parseScriptNumbered,
 )

-- | Where bare @-- compile@ cells land.
defaultModuleName :: Text
defaultModuleName = "SabelaCompiled"

compiledDirective :: Cell -> Maybe CompileDirective
compiledDirective = scriptCompile . fst . parseScriptNumbered . cellSource

isCompiledCell :: Cell -> Bool
isCompiledCell = isJust . compiledDirective

directiveModule :: CompileDirective -> Text
directiveModule CompileDefault = defaultModuleName
directiveModule (CompileNamed n) = n

{- | The pure result of planning all compiled cells of a notebook. Violating
cells appear only in 'cpViolations'; they contribute nothing to any module.
-}
data CompilePlan = CompilePlan
    { cpModules :: M.Map Text Text
    -- ^ Module name → rendered module source.
    , cpCellModule :: M.Map Int Text
    -- ^ Compiled cell id → the module it belongs to.
    , cpModuleDeps :: M.Map Text (S.Set Text)
    -- ^ Module → modules it imports (inferred from name uses).
    , cpViolations :: M.Map Int [CellError]
    -- ^ Cell id → why it cannot compile (skipped at execution time).
    }
    deriving (Show, Eq)

emptyCompilePlan :: CompilePlan
emptyCompilePlan = CompilePlan M.empty M.empty M.empty M.empty

-- | Module name → its generated file path, relative to the repl project dir.
moduleFilePath :: Text -> FilePath
moduleFilePath name = T.unpack (T.replace "." "/" name) <> ".hs"

{- | Plan all compiled cells: validate (module name, declarations-only, no
prompt-state uses, no cross-module cycles), group by module, infer
inter-module imports, render sources. @posMap@ maps cell ids to 1-based
notebook positions for error messages.
-}
planCompiledModules :: M.Map Int Int -> [Cell] -> CompilePlan
planCompiledModules posMap allCode =
    let (defMap, redefMap) = Topo.buildDefMap allCode
        candidates =
            [ (c, directiveModule dir)
            | c <- allCode
            , not (M.member (cellId c) redefMap)
            , Just dir <- [compiledDirective c]
            ]
        violations =
            M.fromListWith (++) (concatMap (cellViolations posMap defMap candidates) candidates)
        valid = [cm | cm@(c, _) <- candidates, not (M.member (cellId c) violations)]
        cellModule = M.fromList [(cellId c, m) | (c, m) <- valid]
        modDeps = moduleDeps defMap cellModule valid
        cycleViolations = moduleCycleViolations posMap cellModule modDeps valid
        violations' = M.unionWith (++) violations cycleViolations
        valid' = [cm | cm@(c, _) <- valid, not (M.member (cellId c) violations')]
        cellModule' = M.fromList [(cellId c, m) | (c, m) <- valid']
        modDeps' = moduleDeps defMap cellModule' valid'
     in CompilePlan
            { cpModules = renderModules modDeps' valid'
            , cpCellModule = cellModule'
            , cpModuleDeps = modDeps'
            , cpViolations = violations'
            }

cellViolations ::
    M.Map Int Int ->
    M.Map Text Int ->
    [(Cell, Text)] ->
    (Cell, Text) ->
    [(Int, [CellError])]
cellViolations posMap defMap candidates (c, modName) =
    [(cellId c, errs) | not (null errs)]
  where
    compiledIds = S.fromList [cellId cc | (cc, _) <- candidates]
    (_, numbered) = parseScriptNumbered (cellSource c)
    (_, uses) = Topo.cellNames (cellSource c)
    nameErrs =
        [ planError ("invalid module name '" <> modName <> "' — use dot-separated capitalized segments, e.g. 'Training.Core'")
        | not (isValidModuleName modName)
        ]
    declErrs =
        [ CellError (Just (ciLine i)) Nothing (ciReason i)
        | i <- checkCompilable numbered
        ]
    bridgeErrs =
        [ planError "compiled cells cannot use bridge values (_bridge_*) — they exist only at the prompt; read them in an interpreted cell and pass the result as an argument"
        | any ("_bridge_" `T.isPrefixOf`) (S.toList uses)
        ]
    interpErrs =
        [ planError
            ( "'"
                <> name
                <> "' is defined in interpreted cell "
                <> position posMap definer
                <> "; a compiled cell cannot use prompt definitions. Mark that cell '-- compile' or pass the value as an argument."
            )
        | name <- S.toList uses
        , Just definer <- [M.lookup name defMap]
        , definer /= cellId c
        , not (S.member definer compiledIds)
        ]
    errs = nameErrs ++ declErrs ++ bridgeErrs ++ interpErrs

planError :: Text -> CellError
planError = CellError Nothing Nothing

position :: M.Map Int Int -> Int -> Text
position posMap cid = T.pack (show (M.findWithDefault cid cid posMap))

-- | Module → modules it depends on, from cross-module name uses.
moduleDeps ::
    M.Map Text Int -> M.Map Int Text -> [(Cell, Text)] -> M.Map Text (S.Set Text)
moduleDeps defMap cellModule valid =
    M.fromListWith
        S.union
        ( [(m, S.empty) | (_, m) <- valid]
            ++ [ (m, S.singleton depMod)
               | (c, m) <- valid
               , let (_, uses) = Topo.cellNames (cellSource c)
               , name <- S.toList uses
               , Just definer <- [M.lookup name defMap]
               , Just depMod <- [M.lookup definer cellModule]
               , depMod /= m
               ]
        )

{- | Modules on an import cycle make every involved cell a violation (GHC
cannot compile them; the fix is to merge them under one module name).
-}
moduleCycleViolations ::
    M.Map Int Int ->
    M.Map Int Text ->
    M.Map Text (S.Set Text) ->
    [(Cell, Text)] ->
    M.Map Int [CellError]
moduleCycleViolations _posMap _cellModule modDeps valid =
    let cyclic = cyclicModules modDeps
        msg =
            "modules {"
                <> T.intercalate ", " (S.toAscList cyclic)
                <> "} import each other in a cycle — give the mutually recursive cells the same '-- compile:' module name"
     in M.fromList
            [ (cellId c, [planError msg])
            | (c, m) <- valid
            , S.member m cyclic
            ]
  where
    cyclicModules deps =
        let mods = M.keys deps
            shrink ms =
                let leaves = [m | m <- S.toList ms, S.null (S.intersection ms (M.findWithDefault S.empty m deps))]
                 in if null leaves then ms else shrink (ms `S.difference` S.fromList leaves)
         in shrink (S.fromList mods)

-- | Render every module's source, cells in notebook order.
renderModules :: M.Map Text (S.Set Text) -> [(Cell, Text)] -> M.Map Text Text
renderModules modDeps valid =
    M.fromList
        [ (m, renderOne m cellsOf)
        | m <- M.keys grouped
        , let cellsOf = grouped M.! m
        ]
  where
    grouped = M.fromListWith (flip (++)) [(m, [c]) | (c, m) <- valid]
    renderOne m cellsOf =
        let exts = sabelaDefaultExts ++ concatMap cellExts cellsOf
            imports =
                ["import " <> dep | dep <- S.toAscList (M.findWithDefault S.empty m modDeps)]
            chunks = map toChunk cellsOf
         in renderCompiledModule m exts imports chunks
    cellExts = metaExts . scriptMeta . fst . parseScriptNumbered . cellSource
    toChunk c =
        CellChunk
            { ccTag = linePragmaTag (cellId c)
            , ccLines = snd (parseScriptNumbered (cellSource c))
            }

{- | Drop dependency edges between cells of the same module: declaration
order inside a module is irrelevant, so same-module mutual recursion must
not register as a cycle (nor impose an execution order).
-}
pruneIntraModuleDeps ::
    M.Map Int Text -> M.Map Int (S.Set Int) -> M.Map Int (S.Set Int)
pruneIntraModuleDeps cellModule =
    M.mapWithKey $ \cid deps -> case M.lookup cid cellModule of
        Nothing -> deps
        Just m -> S.filter (\d -> M.lookup d cellModule /= Just m) deps

{- | Extra root cells implied by a compiled-cell edit: rebuilding a module
recompiles every module that (transitively) imports it, invalidating prompt
bindings downstream of ALL their cells — so all those cells join the roots.
-}
compiledRootExpansion :: CompilePlan -> S.Set Int -> S.Set Int
compiledRootExpansion cplan affected =
    let touched =
            S.fromList
                [m | (cid, m) <- M.toList (cpCellModule cplan), S.member cid affected]
        importers = reverseModuleDeps (cpModuleDeps cplan)
        closure = moduleClosure touched importers
     in S.fromList
            [cid | (cid, m) <- M.toList (cpCellModule cplan), S.member m closure]

reverseModuleDeps :: M.Map Text (S.Set Text) -> M.Map Text (S.Set Text)
reverseModuleDeps deps =
    M.fromListWith
        S.union
        [ (dep, S.singleton m)
        | (m, ds) <- M.toList deps
        , dep <- S.toList ds
        ]

moduleClosure :: S.Set Text -> M.Map Text (S.Set Text) -> S.Set Text
moduleClosure seeds importers = go seeds seeds
  where
    go visited frontier
        | S.null frontier = visited
        | otherwise =
            let next =
                    S.unions
                        [M.findWithDefault S.empty m importers | m <- S.toList frontier]
                fresh = next `S.difference` visited
             in go (visited `S.union` fresh) fresh
