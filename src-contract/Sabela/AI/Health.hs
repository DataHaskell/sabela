{-# LANGUAGE OverloadedStrings #-}

{- | A cell's compile health as a SET of normalized diagnostics, not an error
count. Acceptance compares sets, so a fix that trades one error for another is
rejected; shared so the product engine and eval harness judge a repair alike.
-}
module Sabela.AI.Health (
    DiagnosticKey (..),
    Health (..),
    healthOfResult,
    healthOfTypeQuery,
    healthOfCellError,
    isClean,
    improvesHealth,
    improvesHealthFor,
    healthMsgsFor,
    normalizeMsg,
) where

import Data.Maybe (listToMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Model (CellError (..), bareCellError)

{- | A stable identity for one diagnostic: location plus whitespace-normalized
message, so incidental reflowing does not make the same error compare unequal.
-}
data DiagnosticKey = DiagnosticKey
    { dkLine :: Maybe Int
    , dkCol :: Maybe Int
    , dkMsg :: Text
    }
    deriving (Eq, Ord, Show)

-- | A cell's health: whether it compiled clean, and the set of its diagnostics.
data Health = Health
    { healthCompileOk :: Bool
    , healthDiagnostics :: Set DiagnosticKey
    }
    deriving (Eq, Show)

-- | Collapse runs of whitespace so message identity is stable across reflows.
normalizeMsg :: Text -> Text
normalizeMsg = T.unwords . T.words

diagKey :: CellError -> DiagnosticKey
diagKey ce = DiagnosticKey (ceLine ce) (ceCol ce) (stableMsg (ceMessage ce))

{- | The inference-VOLATILE parts of a diagnostic dropped from its key: a
not-in-scope error prints the goal type GHC inferred at that moment, which
shifts whenever a sibling error is fixed — the sibling must still read as the
same fact, or every partial heal is rejected as introducing a new diagnostic.
-}
stableMsg :: Text -> Text
stableMsg m
    | "not in scope" `T.isInfixOf` T.toLower n =
        normalizeMsg (fst (T.breakOn "Perhaps" (fst (T.breakOn "::" n))))
    | otherwise = n
  where
    n = normalizeMsg m

{- | Health of a cell-execution outcome. A @Left@ contributes a sentinel
diagnostic so it never looks like a subset improvement; warnings do not count.
-}
healthOfResult :: Either Text ExecutionResult -> Health
healthOfResult (Left e) =
    Health
        False
        ( Set.singleton
            (DiagnosticKey Nothing Nothing (normalizeMsg ("run failed: " <> e)))
        )
healthOfResult (Right er) =
    Health ok (Set.fromList (map diagKey diags))
  where
    -- The holistic message is the JOIN of the structured errors, so it shifts
    -- with any sibling's inferred types; key on the structured set when it
    -- exists and fall back to the holistic text only when it is all we have.
    holistic = [bareCellError Nothing Nothing m | m <- maybeToList (erError er)]
    diags
        | null (erErrors er) = holistic
        | otherwise = erErrors er
    ok = null holistic && null (erErrors er)

{- | Health of a GHCi @:type@ query: a returned signature is clean, an output
carrying a diagnostic is red. Checks a candidate expression without running it.
-}
healthOfTypeQuery :: Text -> Health
healthOfTypeQuery out
    | isErr =
        Health False (Set.singleton (DiagnosticKey Nothing Nothing (normalizeMsg out)))
    | otherwise = Health True mempty
  where
    isErr =
        "\"severity\":\"Error\"" `T.isInfixOf` compact
            || "error:" `T.isInfixOf` T.toLower out
    compact = T.filter (/= ' ') out

{- | Health of a cell from its stored error at accept time: 'Nothing' is clean,
'Just' an error is red. Re-checks an owned cell rather than trusting cached health.
-}
healthOfCellError :: Maybe Text -> Health
healthOfCellError Nothing = Health True mempty
healthOfCellError (Just e) =
    Health False (Set.singleton (DiagnosticKey Nothing Nothing (normalizeMsg e)))

-- | Compiled clean: no errors (warnings allowed).
isClean :: Health -> Bool
isClean h = healthCompileOk h && Set.null (healthDiagnostics h)

{- | Whether @new@ genuinely improves on @old@: clean, or PHASE-ORDERED
progress. While any scope-phase error remains, GHC has not type-checked the
affected declaration groups, so healing a scope error can REVEAL latent type
errors that were always in the code — advancing the scope frontier counts as
progress despite them. With the frontier unchanged, the strict message-set
subset rule governs, so an error trade is still rejected.

Compared by MESSAGE only, not location: a fix that adds or removes a line (an
import, a dep pragma) shifts every surviving error's line number, which must not
read as a brand-new diagnostic and block an otherwise-good partial repair.
-}
improvesHealth :: Health -> Health -> Bool
improvesHealth = improvesHealthFor Set.empty

{- | 'improvesHealth' with the cell's own defined names excluded from the
comparison: a not-in-scope error for a CELL-DEFINED name is knock-on noise that
flaps with which declaration groups compiled — the failing group's root-cause
diagnostic is always separately present, so dropping the echoes loses nothing.
-}
improvesHealthFor :: Set Text -> Health -> Health -> Bool
improvesHealthFor defined old new =
    isClean new
        || (scopeNew `Set.isSubsetOf` scopeOld && scopeNew /= scopeOld)
        || (scopeNew == scopeOld && dnew `Set.isSubsetOf` dold && dnew /= dold)
  where
    dold = healthMsgsFor defined old
    dnew = healthMsgsFor defined new
    scopeOld = Set.filter scopeKey dold
    scopeNew = Set.filter scopeKey dnew

{- | The message set of a health value, knock-on echoes for @defined@ names
excluded — the basis 'improvesHealthFor' and the acceptance law share.
-}
healthMsgsFor :: Set Text -> Health -> Set Text
healthMsgsFor defined =
    Set.filter (not . knockOn) . Set.map dkMsg . healthDiagnostics
  where
    knockOn m = maybe False (`Set.member` defined) (scopeSubject m)

{- | A scope\/renamer-phase diagnostic. While one remains, the type checker has
not run on its groups, so later-phase errors surfacing after a scope heal are
reveals, not regressions.
-}
scopeKey :: Text -> Bool
scopeKey m =
    any
        (`T.isInfixOf` T.toLower m)
        [ "not in scope"
        , "could not find module"
        , "could not load module"
        , "hidden package"
        ]

-- | The subject name of a not-in-scope diagnostic, if it has that shape.
scopeSubject :: Text -> Maybe Text
scopeSubject m = do
    let low = T.toLower (normalizeMsg m)
        (pre, post) = T.breakOn "not in scope:" low
    if T.null post
        then Nothing
        else do
            let rest = T.drop (T.length pre + T.length ("not in scope:" :: Text)) (normalizeMsg m)
            w <- listToMaybe (T.words rest)
            let name = T.dropAround (`elem` ("`'()" :: String)) w
            if T.null name then Nothing else Just name
