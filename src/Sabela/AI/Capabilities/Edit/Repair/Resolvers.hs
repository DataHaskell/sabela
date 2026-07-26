{-# LANGUAGE OverloadedStrings #-}

{- | The flag-gated resolver tiers of self_heal (module-index rename,
add-import, hoogle, ambiguous-occurrence, path near-miss, type-directed
discovery): each returns candidate source rewrites, vetted and gate-checked
by the caller ("Sabela.AI.Capabilities.Edit.Run") — never applied here. Split
out of "Sabela.AI.Capabilities.Edit.Repair" to keep both under the size cap.
-}
module Sabela.AI.Capabilities.Edit.Repair.Resolvers (
    moduleResolveCandidates,
    importResolveCandidates,
    qualifiedImportCandidates,
    ambiguousResolveCandidates,
    ambiguousCandidates,
    hoogleCandidates,
    pathNearMissCandidates,
    typeDiscoverCandidates,
) where

import Control.Monad (filterM)
import Data.List (nub)
import Data.Maybe (isNothing, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.Repair (
    goalOfName,
    notInScopeNames,
    resultErrorText,
 )
import Sabela.AI.Capabilities.Edit.ScratchVet (scratchVet)
import Sabela.AI.Capabilities.ModuleCard (storeModuleNames)
import Sabela.AI.Capabilities.ModuleSearch (interesting, resolveNameToModules)
import Sabela.AI.Capabilities.Util (featureEnabled, featureOptIn)
import Sabela.AI.Capability (Capability (..))
import Sabela.AI.CellEco (FitCand (..), cellEco, rankFits)
import Sabela.AI.DepRepair (addBuildDepend)
import Sabela.AI.HoleRepair (goalSpans, substituteNameAt)
import Sabela.AI.HoogleResolve (HoogleHit (..), hoogleQuery, hoogleResolveTopK)
import Sabela.AI.ImportRepair (
    addQualifiedImport,
    addScopedImport,
    renameModule,
    unboundAliasUses,
 )
import Sabela.AI.ModuleResolve (closestModules, isOutOfScopePackage)
import Sabela.AI.PathRepair (pathNearMissFix)
import Sabela.AI.Store (AIStore)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Diagnose (ambiguousOccurrence, couldNotFindModule, misnamedModule)
import Sabela.Diagnose.Packages (table)
import Sabela.Model (CellError (..))
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), getHaskellSession)
import Sabela.State.Environment (envWorkDir)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

{- | Local-Hoogle candidates, tried after every pure fixer declines: resolve the
not-in-scope name to up to K @(package, module)@ pairs, each adding a @-- cabal:@
dep + scoped import. Every pair is scratch-vetted against the goal type before
it may execute live. Default ON; K via @SABELA_HOOGLE_TOP_K@, off with
@SABELA_HOOGLE_RESOLVE=0@.
-}
hoogleCandidates ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
hoogleCandidates app store res src = do
    enabled <- featureEnabled "SABELA_HOOGLE_RESOLVE"
    if not enabled
        then pure []
        else do
            k <- topKFromEnv
            concat <$> mapM (candidatesFor k) (notInScopeNames src res)
  where
    candidatesFor k name = do
        resolved <- hoogleResolveTopK k name (goalOfName res name)
        vetted <-
            filterM
                ( \(pkg, modul) ->
                    scratchVet app store src [pkg] modul name (goalOfName res name)
                )
                resolved
        pure
            [ src'
            | (pkg, modul) <- vetted
            , let src' = addScopedImport modul name (addBuildDepend pkg src)
            , src' /= src
            ]

{- | Type-directed discovery for a not-in-scope name: query hoogle BY THE GOAL
TYPE (not the name), rank the hits within the cell's ecosystem ('rankFits'), and
propose renaming the wrong name to a type-fitting one (+ its import and dep). So a
wrong-named call heals to a differently-named function that FITS THE TYPE,
preferring the cell's own libraries and declining a type-incompatible outsider.
Vetted (run + kept-iff-improves) by the caller.

OPT-IN via @SABELA_TYPE_RESOLVE@ (default OFF): empirically near-inert on
class-polymorphic goals (hoogle cannot bridge type families), so it must prove
itself on a gate before shipping enabled. Substitution is span-localized —
a global replace can corrupt a string literal that still compiles.
-}
typeDiscoverCandidates ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
typeDiscoverCandidates app store res src = do
    enabled <- featureOptIn "SABELA_TYPE_RESOLVE"
    if not enabled
        then pure []
        else concat <$> mapM candidatesAt (goalSpans res)
  where
    candidatesAt (_, _, Nothing) = pure []
    candidatesAt (wrong, goal, Just sp) = do
        k <- topKFromEnv
        hits <- hoogleQuery k goal
        let cands =
                [ FitCand (hhName h) (hhType h) (hhModule h) (hhPackage h)
                | h <- hits
                , not (isOutOfScopePackage (hhPackage h))
                ]
        vetted <-
            filterM
                ( \c ->
                    scratchVet
                        app
                        store
                        src
                        [fcPackage c]
                        (fcModule c)
                        (fcName c)
                        (Just goal)
                )
                (rankFits goal (cellEco src) cands)
        pure
            [ src'
            | c <- vetted
            , Just renamed <- [substituteNameAt sp wrong (fcName c) src]
            , let src' =
                    addScopedImport
                        (fcModule c)
                        (fcName c)
                        (addBuildDepend (fcPackage c) renamed)
            , src' /= src
            ]

{- | Module-not-found repair, phase 2: rename the wrong import to the closest
INSTALLED module by trigram similarity (its package declared by phase 1
'Sabela.AI.Capabilities.Edit.Repair.moduleDepFix'). Fires only on the
no-hint case; @SABELA_MODULE_RESOLVE=0@ off.
-}
moduleResolveCandidates ::
    App -> Either Text ExecutionResult -> Text -> IO [Text]
moduleResolveCandidates app res src = do
    enabled <- featureEnabled "SABELA_MODULE_RESOLVE"
    mBackend <- getHaskellSession (appSessions app)
    case (enabled, mBackend, noHintModule) of
        (True, Just backend, Just wrong) -> do
            installed <- ST.sbQueryComplete backend "import "
            store <- storeModuleNames
            k <- topKFromEnv
            -- Pool = curated known modules, the live installed list, and the
            -- STORE's modules (hidden packages included): live completion only
            -- lists exposed modules, so a misspelling of a hidden module
            -- (Data.Frame for DataFrame) had no candidate anywhere and this
            -- fixer never fired; the renamed cell's hidden-package failure
            -- then walks the dependency row, which serves the -- cabal: line
            -- without applying it (live_test40). 'table' stays for the
            -- post-restart window when completion is not yet warm.
            let pool = nub (map fst table ++ filter interesting (installed ++ store))
            pure
                [ src'
                | cand <- closestModules k moduleFuzzyThreshold wrong pool
                , let src' = renameModule wrong cand src
                , src' /= src
                ]
        _ -> pure []
  where
    errText = resultErrorText res
    noHintModule = case couldNotFindModule errText of
        Just m | isNothing (misnamedModule errText) -> Just m
        _ -> Nothing

{- | Minimum trigram similarity for a fuzzy module-name match (mirrors the
package-token threshold in "Sabela.Diagnose.Packages").
-}
moduleFuzzyThreshold :: Double
moduleFuzzyThreshold = 0.2

{- | Add-import repair: a not-in-scope name that an installed but UNIMPORTED
module exports gains a scoped import — the builtin case the hoogle tier misses
(no new package needed). Each module is scratch-vetted against the goal type
first — a keyword match alone can cross-import a type-incompatible module.
Default ON; @SABELA_IMPORT_RESOLVE=0@ disables.
-}
importResolveCandidates ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
importResolveCandidates app store res src = do
    enabled <- featureEnabled "SABELA_IMPORT_RESOLVE"
    if not enabled
        then pure []
        else concat <$> mapM candidatesFor (notInScopeNames src res)
  where
    candidatesFor name = do
        caps <- resolveNameToModules app name
        vetted <-
            filterM
                ( \cap ->
                    scratchVet app store src [] (capModule cap) name (goalOfName res name)
                )
                caps
        pure
            [ src'
            | cap <- vetted
            , let src' = addScopedImport (capModule cap) name src
            , src' /= src
            ]

{- | Qualified-alias repair: a use of @T.lines@ in a cell that never bound
@T@. The alias is not guessed — GHC names it as unimported, and the BARE name
resolves against the session's own browse index, so the added
@import qualified Data.Text as T@ rests on the same evidence
'importResolveCandidates' uses. Shares its @SABELA_IMPORT_RESOLVE@ gate.

live_test35_wine hit this six times; no mitigation row fitted, because the
not-in-scope name GHC reports is the QUALIFIED one and no capability is
indexed under it.
-}
qualifiedImportCandidates ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
qualifiedImportCandidates app store res src = do
    enabled <- featureEnabled "SABELA_IMPORT_RESOLVE"
    if not enabled
        then pure []
        else concat <$> mapM candidatesFor (unboundAliasUses (resultErrorText res))
  where
    candidatesFor (alias, name) = do
        caps <- resolveNameToModules app name
        vetted <-
            filterM
                ( \cap ->
                    scratchVet app store src [] (capModule cap) name (goalOfName res name)
                )
                caps
        pure
            [ src'
            | cap <- vetted
            , let src' = addQualifiedImport (capModule cap) alias src
            , src' /= src
            ]

{- | Ambiguous-occurrence repair (e.g. @Prelude.take@ vs @DataFrame.take@): the
env-gated wrapper over 'ambiguousCandidates'. GHC names both candidates, so no
session query is needed. Default ON; @SABELA_AMBIGUOUS_RESOLVE=0@ disables.
-}
ambiguousResolveCandidates :: Either Text ExecutionResult -> Text -> IO [Text]
ambiguousResolveCandidates res src = do
    enabled <- featureEnabled "SABELA_AMBIGUOUS_RESOLVE"
    pure (if enabled then ambiguousCandidates res src else [])

{- | Qualify the ambiguous name at each use-site span GHC reports, leaving the
same token in strings and comments alone. Empty without a span: a global replace
could corrupt a literal that still compiles, so no candidate beats a risky one.
-}
ambiguousCandidates :: Either Text ExecutionResult -> Text -> [Text]
ambiguousCandidates res src = case ambiguousOccurrence (resultErrorText res) of
    Nothing -> []
    Just (name, cands) ->
        nub
            [ src'
            | sp <- ambiguousSpans res
            , qual <- cands
            , Just src' <- [substituteNameAt sp name qual src]
            , src' /= src
            ]

{- | The 1-based @(line, col)@ use-site spans of the ambiguous-occurrence
diagnostics — one per structured error that names an ambiguous occurrence and
carries a span. A holistic error (no span) contributes none.
-}
ambiguousSpans :: Either Text ExecutionResult -> [(Int, Int)]
ambiguousSpans (Left _) = []
ambiguousSpans (Right er) =
    [ (l, c)
    | ce <- erErrors er
    , "Ambiguous occurrence" `T.isInfixOf` ceMessage ce
    , Just l <- [ceLine ce]
    , Just c <- [ceCol ce]
    ]

{- | File-not-found near-miss repair (C4-1): a wrong path that uniquely
near-misses a real file under the work dir. Default ON;
@SABELA_PATH_RESOLVE=0@ disables.
-}
pathNearMissCandidates ::
    App -> Either Text ExecutionResult -> Text -> IO [Text]
pathNearMissCandidates app res src = do
    enabled <- featureEnabled "SABELA_PATH_RESOLVE"
    if not enabled
        then pure []
        else maybeToList <$> pathNearMissFix (envWorkDir (appEnv app)) res src

-- | Default top-K candidates the hoogle resolver tries per repair attempt.
defaultTopK :: Int
defaultTopK = 3

-- | The candidate budget K, from @SABELA_HOOGLE_TOP_K@ or 'defaultTopK'.
topKFromEnv :: IO Int
topKFromEnv = do
    mk <- lookupEnv "SABELA_HOOGLE_TOP_K"
    pure (maybe defaultTopK (max 1) (mk >>= readMaybe))
