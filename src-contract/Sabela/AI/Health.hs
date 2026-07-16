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
    normalizeMsg,
) where

import Data.Maybe (maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Model (CellError (..))

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
diagKey ce = DiagnosticKey (ceLine ce) (ceCol ce) (normalizeMsg (ceMessage ce))

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
    holistic = [CellError Nothing Nothing m | m <- maybeToList (erError er)]
    diags = holistic ++ erErrors er
    ok = null diags

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

{- | Whether @new@ genuinely improves on @old@ for a deterministic fixer: @new@
is clean, or removes a diagnostic while introducing none. Speculative repairs
should require 'isClean' instead.
-}
improvesHealth :: Health -> Health -> Bool
improvesHealth old new =
    isClean new
        || ( dnew `Set.isSubsetOf` dold
                && dnew /= dold
           )
  where
    dold = healthDiagnostics old
    dnew = healthDiagnostics new
