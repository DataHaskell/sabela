{-# LANGUAGE OverloadedStrings #-}

{- | A cell's compile health as a SET of normalized diagnostics, not an error
count. Repair acceptance compares diagnostic sets so a candidate that removes one
error while introducing a different one is rejected (an error count would call
that an improvement). Shared so the product engine and the eval harness judge a
repair the same way.
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

{- | A stable identity for one diagnostic: location plus its message with
whitespace normalized, so incidental reflowing does not make "the same error"
compare unequal across two runs of a cell.
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

{- | Health of a cell-execution outcome. A @Left@ (abort/timeout/superseded)
contributes a distinct sentinel diagnostic, so it can never look like a subset
improvement over a genuine compile error. Warnings do not count.
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

{- | Health of a GHCi @:type@ query result: a returned type signature is clean;
an output carrying a diagnostic (structured @-fdiagnostics-as-json@ error, or a
textual @error:@) is red. Used to check a candidate expression WITHOUT running
it.
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

{- | Health of a cell from its stored error (the live re-verify at accept time):
'Nothing' is clean, 'Just' an error is red. Used to re-check an owned cell's
current state from the notebook rather than trusting cached health.
-}
healthOfCellError :: Maybe Text -> Health
healthOfCellError Nothing = Health True mempty
healthOfCellError (Just e) =
    Health False (Set.singleton (DiagnosticKey Nothing Nothing (normalizeMsg e)))

-- | Compiled clean: no errors (warnings allowed).
isClean :: Health -> Bool
isClean h = healthCompileOk h && Set.null (healthDiagnostics h)

{- | Whether @new@ is a genuine improvement over @old@ for a DETERMINISTIC fixer:
either @new@ is clean, or it introduces no diagnostic @old@ lacked and removes at
least one. Speculative repairs should require 'isClean' instead.
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
