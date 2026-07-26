{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.Repair.Mitigate (
    Discharge (..),
    MitigationRow (..),
    mitigationTable,
    rootErrors,
    fractionalIntCandidates,
    substituteNameInCode,
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

data Discharge = Apply | ServeAsArtifact
    deriving (Eq, Show)

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

detectUnshowable :: CellError -> Bool
detectUnshowable ce =
    "No instance for" `T.isInfixOf` msg
        && any (\t -> ("Show " <> t) `T.isInfixOf` msg) displayableTypes
  where
    msg = ceMessage ce

displayFunctions :: [(Text, Text)]
displayFunctions = [("Picture", "displayPicture")]

displayableTypes :: [Text]
displayableTypes = map fst displayFunctions

unshowableGenerate ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
unshowableGenerate _ _ res src =
    pure
        [ wrapped
        | fn <- displayFnFor res
        , Just wrapped <- [wrapFinalExpression fn src]
        ]

displayFnFor :: Either Text ExecutionResult -> [Text]
displayFnFor res =
    take
        1
        [ fn
        | (ty, fn) <- displayFunctions
        , ce <- errorsOf res
        , ("Show " <> ty) `T.isInfixOf` ceMessage ce
        ]

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

isDeclarationLine :: Text -> Bool
isDeclarationLine l =
    "import " `T.isPrefixOf` t
        || "{-#" `T.isPrefixOf` t
        || "--" `T.isPrefixOf` t
        || " = " `T.isInfixOf` t
        || isJust (bindStatementBody t)
  where
    t = T.strip l

detectHiddenPackage :: CellError -> Bool
detectHiddenPackage ce =
    "hidden package" `T.isInfixOf` T.toLower (ceMessage ce)

hiddenPackageGenerate ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
hiddenPackageGenerate _ _ res src =
    pure [addBuildDepend pkg src | pkg <- hiddenPackagesOf res]

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

bareResult :: CellError -> ExecutionResult
bareResult ce = ExecutionResult [] Nothing [ce] []

extensionGenerate ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
extensionGenerate _ _ res src =
    pure (maybeToList ((`addExtension` src) <$> extFromResult res))

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

missingImportGenerate ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
missingImportGenerate app store res src = do
    cands <- importResolveCandidates app store res src
    pure (take 1 (sortOn (Down . T.length) cands))

qualifiedImportGenerate ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
qualifiedImportGenerate app store res src = do
    cands <- qualifiedImportCandidates app store res src
    pure (take 1 (sortOn (Down . T.length) cands))

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

substituteNameInCode :: Text -> Text -> Text -> Text
substituteNameInCode wrong fit src = T.intercalate "\n" (map rewriteLine (T.lines src))
  where
    rewriteLine l
        | "import " `T.isPrefixOf` T.stripStart l = l
        | otherwise = substituteName wrong fit l

wrongNameOf :: Text -> Maybe Text
wrongNameOf msg = case fst <$> goalFromError msg of
    Just w -> Just w
    Nothing -> notInScopeName (T.takeWhile (/= '\n') msg)

errorsOf :: Either Text ExecutionResult -> [CellError]
errorsOf (Left _) = []
errorsOf (Right er) = erErrors er

fractionalIntGenerate ::
    App -> AIStore -> Either Text ExecutionResult -> Text -> IO [Text]
fractionalIntGenerate _ _ res src = pure (fractionalIntCandidates src (errorsOf res))

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
