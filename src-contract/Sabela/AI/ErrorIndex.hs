{-# LANGUAGE OverloadedStrings #-}

{- | The typed error-index interface handed back to the model: GHC's own
diagnostic code (the @GHC-NNNNN@ of <https://errors.haskell.org>) plus how
Sabela remediates that class — automatically, by surfacing candidates, or
manually.

GHC ≥ 9.10 stamps every coded diagnostic with a @code@ in its
@-fdiagnostics-as-json@ output ("Sabela.Errors.Json"), and the textual path
scrapes @[GHC-NNNNN]@ ("Sabela.Errors"); either way the code rides on
'Sabela.Model.ceCode'. This module turns that raw number into the reference URL
and the remediation class, so the model reads a stable taxonomy key instead of
re-parsing English prose.
-}
module Sabela.AI.ErrorIndex (
    GhcCode (..),
    renderGhcCode,
    errorIndexUrl,
    Remediation (..),
    remediationFor,
    ErrorInfo (..),
    errorInfoOf,
    errorInfoForCell,
    errorInfoPairs,
    withErrorInfo,
) where

import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.List (nubBy)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.CellResult (CellOutcome (..), CellResult (..))
import Sabela.Model (CellError (..))

-- | A GHC diagnostic code — the @NNNNN@ of @GHC-NNNNN@.
newtype GhcCode = GhcCode Int
    deriving (Eq, Ord, Show)

{- | The canonical @GHC-NNNNN@ rendering, zero-padded to five digits to match the
errors.haskell.org path scheme (@GHC-00001@, not @GHC-1@).
-}
renderGhcCode :: GhcCode -> Text
renderGhcCode (GhcCode n) =
    "GHC-" <> T.justifyRight 5 '0' (T.pack (show n))

-- | The errors.haskell.org message page for a code.
errorIndexUrl :: GhcCode -> Text
errorIndexUrl c =
    "https://errors.haskell.org/messages/" <> renderGhcCode c <> "/"

{- | How Sabela remediates a diagnostic class — the "automatic or manual" axis
of the error index, made typed:

* 'AutoRepaired' — a deterministic fixer GHC's own message names (add the
  dependency, enable the extension). Applied server-side and re-run; the model
  never sees the raw error. Carries the fixer id.
* 'ModelCandidates' — apply-ready candidate sources are synthesised (hole-fits,
  import\/module resolvers) and surfaced; the model picks. Never auto-applied.
* 'ManualFix' — no automation; the model reasons from the message and the
  reference page.
-}
data Remediation
    = AutoRepaired Text
    | ModelCandidates
    | ManualFix
    deriving (Eq, Show)

{- | The remediation class for a code. A curated registry seeded from the codes
Sabela's fixers already handle and the diagnostics its bench corpus surfaces;
every unknown code defaults to 'ManualFix', so a newly-seen diagnostic is never
mis-promised an automatic fix. Extend it as fixers grow, not the prose scrapers.
-}
remediationFor :: GhcCode -> Remediation
remediationFor (GhcCode n) = case n of
    -- Could not load module … it is a member of the hidden package.
    87110 -> AutoRepaired "dependency"
    -- Could not find module … (resolved to a package via the module table).
    35235 -> AutoRepaired "dependency"
    -- Variable not in scope → hole-fits / import resolvers, model picks.
    88464 -> ModelCandidates
    -- Couldn't match type: no deterministic fix; coercion synthesis is deferred.
    83865 -> ManualFix
    -- Duplicate instance declarations.
    59692 -> ManualFix
    -- Defined but not used (a warning; no action needed).
    40910 -> ManualFix
    _ -> ManualFix

{- | The typed diagnostic for one 'CellError': its code, reference page, and
remediation class. 'Nothing' when GHC left the diagnostic uncoded.
-}
data ErrorInfo = ErrorInfo
    { eiCode :: GhcCode
    , eiReference :: Text
    , eiRemediation :: Remediation
    }
    deriving (Eq, Show)

-- | Build the typed diagnostic from a cell error carrying a GHC code.
errorInfoOf :: CellError -> Maybe ErrorInfo
errorInfoOf ce = do
    n <- ceCode ce
    let code = GhcCode n
    pure (ErrorInfo code (errorIndexUrl code) (remediationFor code))

{- | The typed diagnostics for a rejected cell, one per distinct code. Runtime
('Raised') and aborted outcomes carry no compiler code; warnings are surfaced
separately on @crWarnings@ and are omitted here to keep the error channel clean.
-}
errorInfoForCell :: CellResult -> [ErrorInfo]
errorInfoForCell = dedupByCode . mapMaybe errorInfoOf . rejectedDiags . crOutcome
  where
    rejectedDiags (Rejected ds) = ds
    rejectedDiags _ = []
    dedupByCode = nubBy (\a b -> eiCode a == eiCode b)

-- | A @"diagnostics"@ wire pair for 'mergeToolOk', omitted when empty.
errorInfoPairs :: [ErrorInfo] -> [Pair]
errorInfoPairs [] = []
errorInfoPairs eis = ["diagnostics" .= eis]

{- | Merge the typed diagnostics into a cell-result JSON object under
@diagnostics@ when there are any, for the @execution@ summary embedded by the
mutating tools. Mirrors 'Sabela.Diagnose.cellResultWithGuidance'.
-}
withErrorInfo :: CellResult -> Value -> Value
withErrorInfo cr v = case (v, errorInfoForCell cr) of
    (Object o, eis@(_ : _)) -> Object (KM.insert "diagnostics" (toJSON eis) o)
    _ -> v

instance ToJSON ErrorInfo where
    toJSON ei =
        object
            [ "code" .= renderGhcCode (eiCode ei)
            , "reference" .= eiReference ei
            , "remediation" .= eiRemediation ei
            ]

instance ToJSON Remediation where
    toJSON (AutoRepaired f) =
        object ["kind" .= ("automatic" :: Text), "fixer" .= f]
    toJSON ModelCandidates = object ["kind" .= ("candidates" :: Text)]
    toJSON ManualFix = object ["kind" .= ("manual" :: Text)]
