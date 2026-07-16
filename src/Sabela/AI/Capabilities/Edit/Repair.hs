{-# LANGUAGE OverloadedStrings #-}

{- | The repair candidate sources for 'Sabela.AI.Capabilities.Edit.Run': the
deterministic fixers plus the flag-gated resolver tiers (module-index rename,
add-import, hoogle, speculative hole-fit). Each returns candidate source
rewrites; the caller vets every one by APPLY → RE-RUN → keep-iff-improves, so
these never decide on their own. Split out of @Edit.Run@ to keep both modules
under the size cap; none of these touch the cell-execution machinery.
-}
module Sabela.AI.Capabilities.Edit.Repair (
    firstFix,
    moduleDepStep,
    moduleResolveCandidates,
    importResolveCandidates,
    hoogleCandidates,
    holeFitCandidates,
    selectByTypeCheck,
) where

import Control.Monad (guard)
import Data.List (nub)
import Data.Maybe (isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Sabela.AI.Capabilities.ModuleSearch (interesting, resolveNameToModules)
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.Capability (Capability (..))
import Sabela.AI.DepRepair (addBuildDepend, depFromResult)
import Sabela.AI.ExtRepair (addExtension, extFromResult)
import Sabela.AI.Health (healthOfTypeQuery, isClean)
import Sabela.AI.HoleRepair (
    goalFromError,
    holeFitNames,
    orderBySimilarity,
    substituteName,
    substituteNameAt,
    suggestedNames,
 )
import Sabela.AI.HoogleResolve (hoogleResolveTopK)
import Sabela.AI.ImportRepair (addScopedImport, moduleRenameFix, renameModule)
import Sabela.AI.ModuleResolve (closestModules)
import Sabela.AI.Repair (firstJustM)
import Sabela.Diagnose.Packages (packageForModule, table)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Diagnose (
    couldNotFindModule,
    misnamedModule,
    notInScopeName,
 )
import Sabela.Errors.Json (parseJsonInteractive)
import Sabela.Model (CellError (..))
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), getHaskellSession)

{- | The flag-gated local-Hoogle candidate sources, tried only after every pure
fixer declines. With @SABELA_HOOGLE_RESOLVE@ unset this is the empty list, so the
default repair path is byte-identical to before. When set, resolve the
not-in-scope name to up to K (package, module) candidates via the LOCAL hoogle
CLI and, for each, add the @-- cabal:@ build-depend plus a SCOPED @import M
(name)@. K defaults to 'defaultTopK', override via @SABELA_HOOGLE_TOP_K@. Each
candidate is a kernel-restart+install, so K is the cost knob.
-}
hoogleCandidates :: Either Text ExecutionResult -> Text -> IO [Text]
hoogleCandidates res src = do
    enabled <- lookupEnv "SABELA_HOOGLE_RESOLVE"
    case enabled >> notInScopeFromResult res of
        Nothing -> pure []
        Just name -> do
            k <- topKFromEnv
            resolved <- hoogleResolveTopK k name
            pure
                [ src'
                | (pkg, modul) <- resolved
                , let src' = addScopedImport modul name (addBuildDepend pkg src)
                , src' /= src
                ]

{- | Module-not-found repair, phase 2 (B1). Renames the wrong import to the closest
INSTALLED module by trigram similarity, minus base namespaces so a wrong name never
snaps to a stray base module. Fires only when GHC could not find the module and gave
no "Perhaps you meant" (the pure 'moduleRenameFix' handles the with-hint case). The
package the correct module needs is declared separately by 'moduleDepFix' (phase 1),
so by the time this renames, the target is installed and the verify re-run needs no
kernel restart. Each candidate is vetted by verify-and-revert. Gated by
@SABELA_MODULE_RESOLVE@ (unset = empty list = byte-identical default path).
-}
moduleResolveCandidates ::
    App -> Either Text ExecutionResult -> Text -> IO [Text]
moduleResolveCandidates app res src = do
    enabled <- lookupEnv "SABELA_MODULE_RESOLVE"
    mBackend <- getHaskellSession (appSessions app)
    case (enabled, mBackend, noHintModule) of
        (Just _, Just backend, Just wrong) -> do
            installed <- ST.sbQueryComplete backend "import "
            k <- topKFromEnv
            -- Pool = the curated known modules plus the live (interesting) installed
            -- list. Including 'table' keeps the correct target a candidate even when
            -- the post-restart completion list is not yet warm; verify-and-revert
            -- confirms it actually resolves (its package was declared by phase 1).
            let pool = nub (map fst table ++ filter interesting installed)
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

{- | Add-import repair (B2): when GHC reports a not-in-scope name that an installed
but UNIMPORTED module exports (e.g. @Picture@ from @Sabela.Notebook@), add a scoped
import of it. Covers the builtin case the hoogle tier misses — the symbol needs no
new package, only an import. Each candidate is vetted by verify-and-revert. Gated by
@SABELA_IMPORT_RESOLVE@ (unset = empty list = byte-identical default path).
-}
importResolveCandidates ::
    App -> Either Text ExecutionResult -> Text -> IO [Text]
importResolveCandidates app res src = do
    enabled <- lookupEnv "SABELA_IMPORT_RESOLVE"
    case enabled >> notInScopeFromResult res of
        Nothing -> pure []
        Just name -> do
            caps <- resolveNameToModules app name
            pure
                [ src'
                | cap <- caps
                , let src' = addScopedImport (capModule cap) name src
                , src' /= src
                ]

-- | Default top-K candidates the hoogle resolver tries per repair attempt.
defaultTopK :: Int
defaultTopK = 3

-- | The candidate budget K, from @SABELA_HOOGLE_TOP_K@ or 'defaultTopK'.
topKFromEnv :: IO Int
topKFromEnv = do
    mk <- lookupEnv "SABELA_HOOGLE_TOP_K"
    pure (maybe defaultTopK (max 1) (mk >>= readMaybe))

{- | Speculative hole-fit candidates for a red not-in-scope-with-type cell: query
GHC hole fits for the inferred goal type and substitute the wrong name. Checked
compile-only before any is committed; empty unless the error carries a goal type.
-}
holeFitCandidates :: App -> Either Text ExecutionResult -> Text -> IO [Text]
holeFitCandidates app res src = do
    on <- featureEnabled "SABELA_HOLE_FIT"
    mBackend <- getHaskellSession (appSessions app)
    case (on, mBackend, goalSpans res) of
        (True, Just backend, goals@(_ : _)) ->
            nub . concat <$> mapM (candidatesFor backend) goals
        _ -> pure []
  where
    errText = resultErrorText res
    candidatesFor backend (wrong, ty, mspan) = do
        blob <- ST.sbQueryHoleFits backend ("_ :: " <> ty)
        -- The live session emits -fdiagnostics-as-json, so the fits arrive
        -- JSON-escaped; decode to the plain text holeFitNames parses.
        let (errs, _, rest) = parseJsonInteractive blob
            fitText = T.unlines (map ceMessage errs) <> rest
            -- Did-you-mean first, then hole fits, ranked by spelling closeness
            -- so a typo heals to the nearest valid name.
            names =
                orderBySimilarity
                    wrong
                    (nub (suggestedNames errText ++ holeFitNames fitText))
        pure [s | n <- names, s <- rewrites mspan wrong n, s /= src]
    -- Span-localized rewrite first (precise), then the global replace as a
    -- fallback; both are compile-checked before any is committed.
    rewrites mspan wrong n =
        maybe [] (\sp -> maybeToList (substituteNameAt sp wrong n src)) mspan
            ++ [substituteName wrong n src]

{- | The @(wrong, goalType, span)@ triples a failed run implies, one per
not-in-scope diagnostic. A holistic error carries no span, so only the global
rewrite applies to it.
-}
goalSpans :: Either Text ExecutionResult -> [(Text, Text, Maybe (Int, Int))]
goalSpans (Left e) = [(w, t, Nothing) | Just (w, t) <- [goalFromError e]]
goalSpans (Right er) =
    [ (w, t, (,) <$> ceLine ce <*> ceCol ce)
    | ce <- diags
    , Just (w, t) <- [goalFromError (ceMessage ce)]
    ]
  where
    diags =
        maybe [] (\m -> [CellError Nothing Nothing m]) (erError er)
            ++ erErrors er

{- | The first candidate whose expression @:type@-checks clean, WITHOUT running
it. 'Nothing' when none check clean (or there is no session).
-}
selectByTypeCheck :: App -> [Text] -> IO (Maybe Text)
selectByTypeCheck _ [] = pure Nothing
selectByTypeCheck app cands = do
    mBackend <- getHaskellSession (appSessions app)
    case mBackend of
        Nothing -> pure Nothing
        Just backend -> fmap fst <$> firstJustM (checkClean backend) cands
  where
    checkClean backend c = do
        out <- ST.sbQueryType backend (typeCheckTarget c)
        pure (if isClean (healthOfTypeQuery out) then Just () else Nothing)

{- | The expression to @:type@ for a candidate source: the RHS of a simple
@x = expr@ binding, else the whole stripped source. A non-checkable candidate
fails the check and is skipped, so at worst a repair is missed, never kept.
-}
typeCheckTarget :: Text -> Text
typeCheckTarget src
    | T.count "\n" stripped == 0
    , (_, rhs) <- T.breakOn " = " stripped
    , not (T.null rhs) =
        T.strip (T.drop 3 rhs)
    | otherwise = stripped
  where
    stripped = T.strip src

-- | A failed run's error text (holistic error plus structured messages).
resultErrorText :: Either Text ExecutionResult -> Text
resultErrorText (Left e) = e
resultErrorText (Right er) =
    T.unlines (maybe [] pure (erError er) ++ map ceMessage (erErrors er))

-- | The first not-in-scope name across a failed run's error and error list.
notInScopeFromResult :: Either Text ExecutionResult -> Maybe Text
notInScopeFromResult (Left _) = Nothing
notInScopeFromResult (Right er) =
    listToMaybe (concatMap (mapMaybe notInScopeName . T.lines) errorTexts)
  where
    errorTexts = maybe [] pure (erError er) ++ map ceMessage (erErrors er)

{- | The deterministic source fixers, tried in order: the first whose rewrite
actually changes the source wins this round. Each maps a failed run plus the cell
source to a repaired source, or Nothing when it does not apply.
-}
repairFixers :: [Either Text ExecutionResult -> Text -> Maybe Text]
repairFixers =
    [ \res src -> (`addBuildDepend` src) <$> depFromResult res
    , \res src -> (`addExtension` src) <$> extFromResult res
    , moduleRenameFix
    ]

{- | Module-not-found repair, phase 1 (B1), gated by @SABELA_MODULE_RESOLVE@. The
caller commits the result via the restart-surviving 'applyAndLoop' path (not
verify-and-revert), because adding a dep restarts the kernel; 'moduleResolveCandidates'
(phase 2) renames the import once the package is installed. Nothing when the flag is
off or no dep applies, so the default path is unchanged.
-}
moduleDepStep :: Either Text ExecutionResult -> Text -> IO (Maybe Text)
moduleDepStep res src = do
    enabled <- lookupEnv "SABELA_MODULE_RESOLVE"
    pure (enabled >> moduleDepFix res src)

{- | The pure core of phase 1: when GHC could not find a module and gave no hint, and
the wrong name is trigram-close to a KNOWN packaged module ('table'), declare that
module's package. The import rename itself is phase 2 ('moduleResolveCandidates');
splitting the two lets the rename be verified without a kernel restart aborting its
own check.
-}
moduleDepFix :: Either Text ExecutionResult -> Text -> Maybe Text
moduleDepFix res src = do
    wrong <- couldNotFindModule errText
    guard (isNothing (misnamedModule errText))
    best <- listToMaybe (closestModules 1 moduleFuzzyThreshold wrong (map fst table))
    pkg <- packageForModule best
    let src' = addBuildDepend pkg src
    guard (src' /= src)
    pure src'
  where
    errText = resultErrorText res

-- | The first fixer's rewrite that actually changes the source.
firstFix :: Either Text ExecutionResult -> Text -> Maybe Text
firstFix res src =
    listToMaybe [s | fixer <- repairFixers, Just s <- [fixer res src], s /= src]
