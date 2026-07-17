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
    ambiguousResolveCandidates,
    ambiguousCandidates,
    hoogleCandidates,
    resultErrorText,
) where

import Control.Monad (guard)
import Data.List (nub)
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Sabela.AI.Capabilities.ModuleSearch (interesting, resolveNameToModules)
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.Capability (Capability (..))
import Sabela.AI.DepRepair (addBuildDepend, depFromResult)
import Sabela.AI.ExtRepair (addExtension, extFromResult)
import Sabela.AI.HoleRepair (substituteNameAt)
import Sabela.AI.HoogleResolve (hoogleResolveTopK)
import Sabela.AI.ImportRepair (addScopedImport, moduleRenameFix, renameModule)
import Sabela.AI.ModuleResolve (closestModules)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Diagnose (
    ambiguousOccurrence,
    couldNotFindModule,
    misnamedModule,
    notInScopeName,
 )
import Sabela.Diagnose.Packages (packageForModule, table)
import Sabela.Model (CellError (..))
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), getHaskellSession)

{- | Local-Hoogle candidates, tried after every pure fixer declines: resolve the
not-in-scope name to up to K @(package, module)@ pairs, each adding a @-- cabal:@
dep + scoped import. Default ON; K via @SABELA_HOOGLE_TOP_K@, off with @SABELA_HOOGLE_RESOLVE=0@.
-}
hoogleCandidates :: Either Text ExecutionResult -> Text -> IO [Text]
hoogleCandidates res src = do
    enabled <- featureEnabled "SABELA_HOOGLE_RESOLVE"
    case guard enabled >> notInScopeFromResult res of
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

{- | Module-not-found repair, phase 2: rename the wrong import to the closest
INSTALLED module by trigram similarity (its package declared by phase 1
'moduleDepFix'). Fires only on the no-hint case; @SABELA_MODULE_RESOLVE=0@ off.
-}
moduleResolveCandidates ::
    App -> Either Text ExecutionResult -> Text -> IO [Text]
moduleResolveCandidates app res src = do
    enabled <- featureEnabled "SABELA_MODULE_RESOLVE"
    mBackend <- getHaskellSession (appSessions app)
    case (enabled, mBackend, noHintModule) of
        (True, Just backend, Just wrong) -> do
            installed <- ST.sbQueryComplete backend "import "
            k <- topKFromEnv
            -- Pool = curated known modules plus the live installed list. Keeping
            -- 'table' in keeps the target a candidate when the post-restart
            -- completion list is not yet warm; verify-and-revert confirms it resolves.
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

{- | Add-import repair: a not-in-scope name that an installed but UNIMPORTED
module exports gains a scoped import — the builtin case the hoogle tier misses
(no new package needed). Default ON; @SABELA_IMPORT_RESOLVE=0@ disables.
-}
importResolveCandidates ::
    App -> Either Text ExecutionResult -> Text -> IO [Text]
importResolveCandidates app res src = do
    enabled <- featureEnabled "SABELA_IMPORT_RESOLVE"
    case guard enabled >> notInScopeFromResult res of
        Nothing -> pure []
        Just name -> do
            caps <- resolveNameToModules app name
            pure
                [ src'
                | cap <- caps
                , let src' = addScopedImport (capModule cap) name src
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

-- | Default top-K candidates the hoogle resolver tries per repair attempt.
defaultTopK :: Int
defaultTopK = 3

-- | The candidate budget K, from @SABELA_HOOGLE_TOP_K@ or 'defaultTopK'.
topKFromEnv :: IO Int
topKFromEnv = do
    mk <- lookupEnv "SABELA_HOOGLE_TOP_K"
    pure (maybe defaultTopK (max 1) (mk >>= readMaybe))

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

{- | Module-not-found repair, phase 1: declare the missing package. The caller
commits this via 'applyAndLoop', not verify-and-revert — a dep add restarts the
kernel. Default ON; @SABELA_MODULE_RESOLVE=0@ disables.
-}
moduleDepStep :: Either Text ExecutionResult -> Text -> IO (Maybe Text)
moduleDepStep res src = do
    enabled <- featureEnabled "SABELA_MODULE_RESOLVE"
    pure (guard enabled >> moduleDepFix res src)

{- | The pure core of phase 1: a hintless not-found module that is trigram-close
to a known packaged module ('table') gains that package. Splitting the rename off
into phase 2 lets it verify without a kernel restart aborting its own check.
-}
moduleDepFix :: Either Text ExecutionResult -> Text -> Maybe Text
moduleDepFix res src = do
    wrong <- couldNotFindModule errText
    guard (isNothing (misnamedModule errText))
    best <-
        listToMaybe (closestModules 1 moduleFuzzyThreshold wrong (map fst table))
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
