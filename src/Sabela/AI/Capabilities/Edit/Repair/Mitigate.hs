{-# LANGUAGE OverloadedStrings #-}

{- | G6: the diagnostic-class mitigation table (see 'mitigationTable'). The
selection\/iteration driver lives in this module's @Loop@ sibling.
-}
module Sabela.AI.Capabilities.Edit.Repair.Mitigate (
    MitigationRow (..),
    mitigationTable,
    rootErrors,
    fractionalIntCandidates,
) where

import Data.List (nub, sortOn)
import Data.Maybe (maybeToList)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.Repair (resultErrorText)
import Sabela.AI.Capabilities.Edit.Repair.Resolvers (importResolveCandidates)
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.ExtRepair (addExtension, extFromResult)
import Sabela.AI.Health (scopeSubject)
import Sabela.AI.HoleRepair (goalFromError, orderBySimilarity, substituteName, suggestedNames)
import Sabela.AI.Store (AIStore)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Diagnose (ambiguousOccurrence, notInScopeName)
import Sabela.Model
import Sabela.Parse (cellNames)
import Sabela.State (App)

-- | One diagnostic-class row: a name (for telemetry/disclosure), a detector
-- over a single root diagnostic, and a candidate-source generator.
data MitigationRow = MitigationRow
    { mitClass :: Text
    , mitDetect :: CellError -> Bool
    , mitGenerate :: App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
    }

{- | The seed table (G6 task 1): growth means adding a row here, never a
branch elsewhere. Each row's generator haddock says what it reuses vs fresh.
-}
mitigationTable :: [MitigationRow]
mitigationTable =
    [ MitigationRow "missing-extension" detectExtension extensionGenerate
    , MitigationRow "ambiguous-occurrence" detectAmbiguous ambiguousGenerate
    , MitigationRow "did-you-mean" detectDidYouMean didYouMeanGenerate
    , MitigationRow "missing-import" detectMissingImport missingImportGenerate
    , MitigationRow "fractional-int" detectFractionalInt fractionalIntGenerate
    ]

detectExtension :: CellError -> Bool
detectExtension ce = case extFromResult (Right (bareResult ce)) of
    Just _ -> True
    Nothing -> False

detectAmbiguous :: CellError -> Bool
detectAmbiguous ce = "Ambiguous occurrence" `T.isInfixOf` ceMessage ce

detectDidYouMean :: CellError -> Bool
detectDidYouMean ce =
    isNotInScope ce && not (null (suggestedNames (ceMessage ce)))

detectMissingImport :: CellError -> Bool
detectMissingImport = isNotInScope

isNotInScope :: CellError -> Bool
isNotInScope ce = "not in scope" `T.isInfixOf` T.toLower (ceMessage ce)

detectFractionalInt :: CellError -> Bool
detectFractionalInt ce =
    "No instance for" `T.isInfixOf` msg
        && ("Fractional" `T.isInfixOf` msg || "Floating" `T.isInfixOf` msg)
  where
    msg = ceMessage ce

-- | A one-error 'ExecutionResult' so the single-diagnostic detectors can
-- reuse the whole-result generators ('extFromResult') without duplicating them.
bareResult :: CellError -> ExecutionResult
bareResult ce = ExecutionResult [] Nothing [ce] []

extensionGenerate :: App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
extensionGenerate _ _ res src =
    pure (maybeToList ((`addExtension` src) <$> extFromResult res))

{- | Ambiguous-occurrence, healed by whole-cell qualification (GHC names both
readings via 'ambiguousOccurrence'): span-localized substitution needs a
cell-relative line\/col a disposable probe cannot give (see @Loop@'s haddock).
-}
ambiguousGenerate :: App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
ambiguousGenerate _ _ res src = do
    enabled <- featureEnabled "SABELA_AMBIGUOUS_RESOLVE"
    pure $
        if not enabled
            then []
            else case ambiguousOccurrence (resultErrorText res) of
                Nothing -> []
                Just (name, cands) ->
                    nub [s | qual <- cands, let s = substituteNameInCode name qual src, s /= src]

{- | 'importResolveCandidates', narrowed to the most specific (longest)
import: Sabela's own modules re-export submodules, so several resolving
imports often name the same entity — task 3's sanctioned tie-break.
-}
missingImportGenerate :: App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
missingImportGenerate app store res src = do
    cands <- importResolveCandidates app store res src
    pure (take 1 (sortOn (Down . T.length) cands))

{- | Not-in-scope, healed from GHC's own "Perhaps use" suggestion: never a
cross-scope invention, since every candidate is a name GHC itself already
sees as valid (a bare not-in-scope with no GHC suggestion yields nothing).
-}
didYouMeanGenerate :: App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
didYouMeanGenerate _ _ res src = pure (nub (concatMap candidatesFor (errorsOf res)))
  where
    candidatesFor ce
        | Just wrong <- wrongNameOf (ceMessage ce) =
            [ s
            | n <- orderBySimilarity wrong (nub (suggestedNames (ceMessage ce)))
            , let s = substituteNameInCode wrong n src
            , s /= src
            ]
        | otherwise = []

{- | 'substituteName', skipping @import@ lines: a whole-token replace would
otherwise corrupt @import M (wrong)@ into invalid syntax alongside the real
use site — the one place a global rename must never reach.
-}
substituteNameInCode :: Text -> Text -> Text -> Text
substituteNameInCode wrong fit src = T.intercalate "\n" (map rewriteLine (T.lines src))
  where
    rewriteLine l
        | "import " `T.isPrefixOf` T.stripStart l = l
        | otherwise = substituteName wrong fit l

{- | The not-in-scope name, via 'goalFromError' (handles GHC's one-line and
multi-line forms) — a bare quoted-token scan would wrongly grab the
suggestion's name from a later line instead.
-}
wrongNameOf :: Text -> Maybe Text
wrongNameOf msg = case fst <$> goalFromError msg of
    Just w -> Just w
    Nothing -> notInScopeName (T.takeWhile (/= '\n') msg)

errorsOf :: Either Text ExecutionResult -> [CellError]
errorsOf (Left _) = []
errorsOf (Right er) = erErrors er

{- | Defaulted-literal annotation mismatch (@No instance for Fractional\/
Floating Int@): find every name in the cell annotated @:: Int@\/@Integer@,
and propose re-annotating to @Double@ — the live_test5 sine cell's fix.
-}
fractionalIntGenerate :: App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
fractionalIntGenerate _ _ res src = pure (fractionalIntCandidates src (errorsOf res))

{- | Re-annotating to @Double@ is tried first; the annotation-DROPPED variant
only when that produces nothing, never mixed in the same pool — running both
together would manufacture a false tie (task 3 reserves ties for real ones).
-}
fractionalIntCandidates :: Text -> [CellError] -> [Text]
fractionalIntCandidates src errs
    | not (any detectFractionalInt errs) || null names = []
    | cand1 /= src = [cand1]
    | cand2 /= src = [cand2]
    | otherwise = []
  where
    names = namesAnnotatedInt src
    cand1 = foldr (`rewriteDeclAnnotation` "Double") src names
    cand2 = foldr (`rewriteDeclAnnotation` "") src names

-- | Every name declared anywhere in @src@ as a one-line
-- @name = expr :: Int@ \/ @:: Integer@ binding.
namesAnnotatedInt :: Text -> [Text]
namesAnnotatedInt src =
    nub [name | l <- T.lines src, Just (name, ty) <- [declNameAndType l], ty `elem` ["Int", "Integer"]]
  where
    declNameAndType l = case T.breakOn "=" l of
        (lhsRaw, eqRest)
            | not (T.null eqRest), not (T.null (T.strip lhsRaw)) ->
                let (_, tyPart) = T.breakOn "::" (T.drop 1 eqRest)
                 in if T.null tyPart
                        then Nothing
                        else nonEmptyTy (T.strip lhsRaw) (T.strip (T.drop 2 tyPart))
        _ -> Nothing
    nonEmptyTy name ty = if T.null ty then Nothing else Just (name, ty)

{- | Rewrite @name@'s one-line @:: TY@ declaration, replacing the type with
@newTy@, or dropping the annotation entirely when @newTy@ is empty.
-}
rewriteDeclAnnotation :: Text -> Text -> Text -> Text
rewriteDeclAnnotation name newTy src = T.intercalate "\n" (map go (T.lines src))
  where
    go l = case T.breakOn "=" l of
        (lhsRaw, eqRest)
            | T.strip lhsRaw == name, not (T.null eqRest) ->
                let afterEq = T.drop 1 eqRest
                    (exprPart, tyPart) = T.breakOn "::" afterEq
                 in if T.null tyPart
                        then l
                        else
                            if T.null newTy
                                then lhsRaw <> "=" <> T.stripEnd exprPart
                                else lhsRaw <> "=" <> exprPart <> ":: " <> newTy
        _ -> l

{- | The current diagnostics with cell-defined-name knock-ons folded out
(G6 task 2), reusing 'scopeSubject' — the SAME identity the health law uses
to exclude a failed declaration's own not-in-scope echoes.
-}
rootErrors :: Text -> Either Text ExecutionResult -> [CellError]
rootErrors _ (Left e) = [bareCellError Nothing Nothing e]
rootErrors src (Right er) = filter (not . knockOn) diags
  where
    diags
        | null (erErrors er) = maybe [] (\m -> [bareCellError Nothing Nothing m]) (erError er)
        | otherwise = erErrors er
    defined = fst (cellNames src)
    knockOn ce = maybe False (`Set.member` defined) (scopeSubject (ceMessage ce))
