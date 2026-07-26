{-# LANGUAGE OverloadedStrings #-}

{- | G6: the diagnostic-class mitigation table (see 'mitigationTable'). The
selection\/iteration driver lives in this module's @Loop@ sibling.
-}
module Sabela.AI.Capabilities.Edit.Repair.Mitigate (
    Discharge (..),
    MitigationRow (..),
    mitigationTable,
    rootErrors,
    fractionalIntCandidates,
) where

import Data.List (nub, sortOn)
import Data.Maybe (isJust, mapMaybe, maybeToList)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.Repair (resultErrorText)
import Sabela.AI.Capabilities.Edit.Repair.Resolvers (
    importResolveCandidates,
    qualifiedImportCandidates,
 )
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.DepRepair (addBuildDepend)
import Sabela.AI.ExtRepair (addExtension, extFromResult)
import Sabela.AI.Health (scopeSubject)
import Sabela.AI.HoleRepair (
    goalFromError,
    orderBySimilarity,
    substituteName,
    suggestedNames,
 )
import Sabela.AI.ImportRepair (unboundAliasUses)
import Sabela.AI.Store (AIStore)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Diagnose (ambiguousOccurrence, hiddenPackage, notInScopeName)
import Sabela.Model
import Sabela.Parse (cellNames)
import Sabela.State (App)
import ScriptHs.Render (bindStatementBody)

{- | How a row discharges once its candidate is proven. Most rows apply the
fix; a row whose fix is a DEPENDENCY may not — G2's hard rule keeps that the
model's deliberate act — so it serves the committable source instead.
-}
data Discharge = Apply | ServeAsArtifact
    deriving (Eq, Show)

{- | One diagnostic-class row: a name (for telemetry/disclosure), a detector
over a single root diagnostic, a candidate-source generator, and how a proven
candidate is discharged.
-}
data MitigationRow = MitigationRow
    { mitClass :: Text
    , mitDetect :: CellError -> Bool
    , mitGenerate ::
        App ->
        AIStore ->
        Either Text ExecutionResult ->
        Text ->
        IO [Text]
    , mitDischarge :: Discharge
    }

{- | The seed table (G6 task 1): growth means adding a row here, never a
branch elsewhere. Each row's generator haddock says what it reuses vs fresh.
-}
mitigationTable :: [MitigationRow]
mitigationTable =
    [ MitigationRow "missing-extension" detectExtension extensionGenerate Apply
    , MitigationRow "ambiguous-occurrence" detectAmbiguous ambiguousGenerate Apply
    , MitigationRow "did-you-mean" detectDidYouMean didYouMeanGenerate Apply
    , MitigationRow "missing-import" detectMissingImport missingImportGenerate Apply
    , MitigationRow
        "unbound-qualified-alias"
        detectUnboundAlias
        qualifiedImportGenerate
        Apply
    , MitigationRow "fractional-int" detectFractionalInt fractionalIntGenerate Apply
    , MitigationRow
        "hidden-package"
        detectHiddenPackage
        hiddenPackageGenerate
        ServeAsArtifact
    , MitigationRow
        "unshowable-display"
        detectUnshowable
        unshowableGenerate
        Apply
    ]

{- | A value the notebook can DRAW but GHCi cannot @show@: the cell's final
expression has a display function, and printing it instead is a plain
mistake the compiler can settle. live_test28 wrote
@plot [(x, sin x) | ...]@ — which typechecks, so the gate admits it — and
the cell went red on @No instance for 'Show Picture'@ while the identical
run wrapped in @displayPicture@ succeeded.
-}
detectUnshowable :: CellError -> Bool
detectUnshowable ce =
    "No instance for" `T.isInfixOf` msg
        && any (\t -> ("Show " <> t) `T.isInfixOf` msg) displayableTypes
  where
    msg = ceMessage ce

{- | Types the notebook draws, and the function that draws each. Growth is a
row here, never a branch elsewhere; a type absent from this table falls
through to the plain rejection.
-}
displayFunctions :: [(Text, Text)]
displayFunctions = [("Picture", "displayPicture")]

displayableTypes :: [Text]
displayableTypes = map fst displayFunctions

{- | Wrap the cell's final expression in its display function. Only the last
statement is rewritten, so declarations and imports above it are untouched.
-}
unshowableGenerate ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
unshowableGenerate _ _ res src =
    pure
        [ wrapped
        | fn <- displayFnFor res
        , Just wrapped <- [wrapFinalExpression fn src]
        ]

-- | The display function for whichever displayable type the error named.
displayFnFor :: Either Text ExecutionResult -> [Text]
displayFnFor res =
    take
        1
        [ fn
        | (ty, fn) <- displayFunctions
        , ce <- errorsOf res
        , ("Show " <> ty) `T.isInfixOf` ceMessage ce
        ]

{- | Rewrite the last non-blank line as @fn (that line)@, when it is an
expression rather than a declaration or import.
-}
wrapFinalExpression :: Text -> Text -> Maybe Text
wrapFinalExpression fn src = case reverse (T.lines src) of
    [] -> Nothing
    (lastLine : rest)
        | T.null (T.strip lastLine) -> Nothing
        | isDeclarationLine lastLine -> Nothing
        | otherwise ->
            Just
                ( T.unlines
                    (reverse rest ++ [fn <> " (" <> T.strip lastLine <> ")"])
                )

{- | A line that binds or imports rather than evaluating. The bind test is
'bindStatementBody', the STATEMENT-level notion of @<-@: a bare @" <- "@
scan reads a list comprehension's arrow as a bind and refuses to wrap
@plot [(x, sin x) | x <- ...]@ — the live_test19 class, in a new place.
-}
isDeclarationLine :: Text -> Bool
isDeclarationLine l =
    "import " `T.isPrefixOf` t
        || "{-#" `T.isPrefixOf` t
        || "--" `T.isPrefixOf` t
        || " = " `T.isInfixOf` t
        || isJust (bindStatementBody t)
  where
    t = T.strip l

{- | GHC-87110: the module exists but its package is hidden. Provable, but the
fix is a dependency, so this row never applies — see 'Discharge'.
-}
detectHiddenPackage :: CellError -> Bool
detectHiddenPackage ce =
    "hidden package" `T.isInfixOf` T.toLower (ceMessage ce)

{- | The committable source: the candidate with its @-- cabal: build-depends:@
line prepended. live_test8 received this diagnostic on its final turn with no
move left; the model must be handed the artifact, not prose describing it.
-}
hiddenPackageGenerate ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
hiddenPackageGenerate _ _ res src =
    pure [addBuildDepend pkg src | pkg <- hiddenPackagesOf res]

-- | Every hidden package GHC named across the result's diagnostics.
hiddenPackagesOf :: Either Text ExecutionResult -> [Text]
hiddenPackagesOf (Left _) = []
hiddenPackagesOf (Right r) =
    take 1 (mapMaybe (hiddenPackage . ceMessage) (erErrors r))

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

{- | GHC-76037: the use site names a module alias the cell never bound
(@T.lines@ with no @import qualified Data.Text as T@). Distinct from a plain
not-in-scope: the reported name is QUALIFIED, so no capability is indexed
under it and the missing-import row resolves nothing.
-}
detectUnboundAlias :: CellError -> Bool
detectUnboundAlias ce = not (null (unboundAliasUses (ceMessage ce)))

isNotInScope :: CellError -> Bool
isNotInScope ce = "not in scope" `T.isInfixOf` T.toLower (ceMessage ce)

detectFractionalInt :: CellError -> Bool
detectFractionalInt ce =
    "No instance for" `T.isInfixOf` msg
        && ("Fractional" `T.isInfixOf` msg || "Floating" `T.isInfixOf` msg)
  where
    msg = ceMessage ce

{- | A one-error 'ExecutionResult' so the single-diagnostic detectors can
reuse the whole-result generators ('extFromResult') without duplicating them.
-}
bareResult :: CellError -> ExecutionResult
bareResult ce = ExecutionResult [] Nothing [ce] []

extensionGenerate ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
extensionGenerate _ _ res src =
    pure (maybeToList ((`addExtension` src) <$> extFromResult res))

{- | Ambiguous-occurrence, healed by whole-cell qualification (GHC names both
readings via 'ambiguousOccurrence'): span-localized substitution needs a
cell-relative line\/col a disposable probe cannot give (see @Loop@'s haddock).
-}
ambiguousGenerate ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
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
missingImportGenerate ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
missingImportGenerate app store res src = do
    cands <- importResolveCandidates app store res src
    pure (take 1 (sortOn (Down . T.length) cands))

{- | The qualified-alias import, narrowed the same way 'missingImportGenerate'
narrows: most specific module wins.
-}
qualifiedImportGenerate ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
qualifiedImportGenerate app store res src = do
    cands <- qualifiedImportCandidates app store res src
    pure (take 1 (sortOn (Down . T.length) cands))

{- | Not-in-scope, healed from GHC's own "Perhaps use" suggestion: never a
cross-scope invention, since every candidate is a name GHC itself already
sees as valid (a bare not-in-scope with no GHC suggestion yields nothing).
-}
didYouMeanGenerate ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
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
fractionalIntGenerate ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
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

{- | Every name declared anywhere in @src@ as a one-line
@name = expr :: Int@ \/ @:: Integer@ binding.
-}
namesAnnotatedInt :: Text -> [Text]
namesAnnotatedInt src =
    nub
        [ name
        | l <- T.lines src
        , Just (name, ty) <- [declNameAndType l]
        , ty `elem` ["Int", "Integer"]
        ]
  where
    declNameAndType l = case T.breakOn "=" l of
        (lhsRaw, eqRest)
            | not (T.null eqRest)
            , not (T.null (T.strip lhsRaw)) ->
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
            | T.strip lhsRaw == name
            , not (T.null eqRest) ->
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
        | null (erErrors er) =
            maybe [] (\m -> [bareCellError Nothing Nothing m]) (erError er)
        | otherwise = erErrors er
    defined = fst (cellNames src)
    knockOn ce = maybe False (`Set.member` defined) (scopeSubject (ceMessage ce))
