{-# LANGUAGE OverloadedStrings #-}

{- | The deterministic repair candidates for 'Sabela.AI.Capabilities.Edit.Run':
the pure fixers (extension, module-rename) plus the shared diagnostic-parsing
helpers the flag-gated resolver tiers in
"Sabela.AI.Capabilities.Edit.Repair.Resolvers" also draw on. Each returns
candidate source rewrites; the caller gate-verifies every one before it may
ever touch the notebook (G2) — these never decide on their own. Split out of
@Edit.Run@ to keep both modules under the size cap; none of these touch the
cell-execution machinery.
-}
module Sabela.AI.Capabilities.Edit.Repair (
    firstFix,
    dependencySuggestion,
    moduleDepStep,
    goalOfName,
    notInScopeNames,
    resultErrorText,
) where

import Control.Monad (guard)
import Data.List (nub)
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Set as S
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.DepRepair (addBuildDepend, depFromResult)
import Sabela.AI.ExtRepair (addExtension, extFromResult)
import Sabela.AI.HoleRepair (goalSpans)
import Sabela.AI.ImportRepair (moduleRenameFix)
import Sabela.AI.ModuleResolve (closestModules)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Diagnose (couldNotFindModule, misnamedModule, notInScopeName)
import Sabela.Diagnose.Packages (packageForModule, table)
import Sabela.Model (CellError (..))
import Sabela.Parse (cellNames)

{- | The goal type GHC inferred for a name, so the resolvers can vet a candidate
against it. Searches EVERY diagnostic — matching only the first error's name
left later names goal-less, and a goal-less candidate bypasses the vet.
-}
goalOfName :: Either Text ExecutionResult -> Text -> Maybe Text
goalOfName res name =
    listToMaybe [ty | (w, ty, _) <- goalSpans res, w == name]

{- | Minimum trigram similarity for a fuzzy module-name match (mirrors the
package-token threshold in "Sabela.Diagnose.Packages").
-}
moduleFuzzyThreshold :: Double
moduleFuzzyThreshold = 0.2

-- | A failed run's error text (holistic error plus structured messages).
resultErrorText :: Either Text ExecutionResult -> Text
resultErrorText (Left e) = e
resultErrorText (Right er) =
    T.unlines (maybe [] pure (erError er) ++ map ceMessage (erErrors er))

{- | All distinct not-in-scope names across a failed run's error and error list,
so the resolvers can fix a later name (@isDigit@) even when the first
(@takeWhile1@, no home module) is unresolvable — one round fixes one, the loop
re-runs and takes the next. Harvests BOTH the single-line form (per line) and
GHC's multi-line form (via 'goalSpans') — the multi-line form is the common
one, and missing it silently no-ops the whole resolver tier.
-}
notInScopeNames :: Text -> Either Text ExecutionResult -> [Text]
notInScopeNames src res@(Right er) =
    nub . filter usable $
        concatMap (mapMaybe notInScopeName . T.lines) errorTexts
            ++ [w | (w, _, _) <- goalSpans res]
  where
    errorTexts = maybe [] pure (erError er) ++ map ceMessage (erErrors er)
    -- A name the CELL defines is a knock-on casualty, never a repair target:
    -- hunting an import for it commits a foreign package for a name that
    -- resolves itself once the root causes are fixed.
    usable w = not (T.null w) && not (w `S.member` defs)
    defs = fst (cellNames src)
notInScopeNames _ (Left _) = []

{- | The deterministic source fixers, tried in order: the first whose rewrite
actually changes the source wins this round. Each maps a failed run plus the cell
source to a repaired source, or Nothing when it does not apply. The dependency
fixer is NOT here (G2 hard rule 1): see 'dependencySuggestion', which the
caller only ever discloses, never applies.
-}
repairFixers :: [Either Text ExecutionResult -> Text -> Maybe Text]
repairFixers =
    [ \res src -> (`addExtension` src) <$> extFromResult res
    , moduleRenameFix
    ]

{- | The (package, rewrite) a hidden-package failure implies, if adding it
actually changes the source. G2 hard rule 1: a @-- cabal: build-depends:@
line the model did not write is never applied automatically, however green
it gate-verifies — the caller discloses this as a suggestion only.
-}
dependencySuggestion ::
    Either Text ExecutionResult -> Text -> Maybe (Text, Text)
dependencySuggestion res src = do
    pkg <- depFromResult res
    let src' = addBuildDepend pkg src
    if src' == src then Nothing else Just (pkg, src')

{- | Module-not-found repair, phase 1: declare the missing package, as a
DISCLOSED SUGGESTION only (G2 hard rule 1) — the caller gate-verifies it and
never commits it automatically, since a dep add restarts the kernel.
Default ON; @SABELA_MODULE_RESOLVE=0@ disables.
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
