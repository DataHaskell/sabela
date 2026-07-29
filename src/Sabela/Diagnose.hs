{-# LANGUAGE OverloadedStrings #-}

module Sabela.Diagnose (
    Guidance (..),
    diagnose,
    guidanceForCell,
    guidancePairs,
    cellResultWithGuidance,
    cellResultWithExtraGuidance,
    topLevelLetMessage,
    holeFitGoal,
    hiddenPackage,
    hiddenPackages,
    neededExtension,
    misnamedModule,
    couldNotFindModule,
    couldNotFindModules,
    notInScopeName,
    ambiguousOccurrence,
    ambiguousOccurrences,
    packageNeedsFlag,
    GrammarRoute (..),
    routeFailure,
) where

import Data.Aeson (ToJSON (..), Value (..), object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.Char (isAlphaNum, isDigit, isUpper)
import Data.List (nub)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.CellResult (CellOutcome (..), CellResult, crOutcome)
import Sabela.AI.Hints (knownExtensions)
import Sabela.AI.Unshowable (unshowableGuidanceMessage)
import Sabela.Diagnose.Packages (packageForModule, resolvePackageToken)
import Sabela.Model (CellError (..))

data Guidance = Guidance
    { gCategory :: Text
    , gMessage :: Text
    }
    deriving (Eq, Show)

instance ToJSON Guidance where
    toJSON g = object ["category" .= gCategory g, "message" .= gMessage g]

diagnose :: Text -> [Guidance]
diagnose err = nub (concatMap ($ err) rules)

rules :: [Text -> [Guidance]]
rules =
    [ missingModule
    , didYouMean
    , letParse
    , ambiguousType
    , typeMismatch
    , unshowableResult
    ]

unshowableResult :: Text -> [Guidance]
unshowableResult err =
    [Guidance "unshowable-result" m | Just m <- [unshowableGuidanceMessage err]]

guidanceForCell :: CellResult -> [Guidance]
guidanceForCell = nub . concatMap diagnose . outcomeErrors . crOutcome

outcomeErrors :: CellOutcome -> [Text]
outcomeErrors (Raised m) = [m]
outcomeErrors (Rejected ds) = map ceMessage ds
outcomeErrors _ = []

guidancePairs :: [Guidance] -> [Pair]
guidancePairs [] = []
guidancePairs gs = ["guidance" .= gs]

cellResultWithGuidance :: CellResult -> Value
cellResultWithGuidance = cellResultWithExtraGuidance []

cellResultWithExtraGuidance :: [Guidance] -> CellResult -> Value
cellResultWithExtraGuidance extra cr = case (toJSON cr, guidanceForCell cr ++ extra) of
    (Object o, gs@(_ : _)) -> Object (KM.insert "guidance" (toJSON gs) o)
    (v, _) -> v

missingModule :: Text -> [Guidance]
missingModule err
    | Just pkg <- hiddenPackage err =
        [Guidance "missing-dependency" (addDepMessage pkg)]
    | any (`T.isInfixOf` err) findMarkers
    , Just m <- quotedToken err =
        [Guidance "missing-dependency" (resolveMessage m)]
    | otherwise = []
  where
    findMarkers =
        [ "Could not find module"
        , "Failed to load interface for"
        , "is a package, not a module"
        ]
    addDepMessage pkg =
        "Add this as the FIRST line of the cell: -- cabal: build-depends: "
            <> pkg
            <> "  (there is no `cabal install`; Sabela installs declared deps on the next\
               \ run, which restarts the kernel once)."
    resolveMessage m = case packageForModule m of
        Just pkg -> addDepMessage pkg
        Nothing -> case resolvePackageToken m of
            Just pkg -> addDepMessage pkg
            Nothing ->
                "Module "
                    <> m
                    <> " was not found. If it belongs to a package, declare it as\
                       \ the FIRST line of the cell: -- cabal: build-depends: <package>  (not\
                       \ `cabal install`)."

hiddenPackages :: Text -> [Text]
hiddenPackages err =
    nub
        [ packageFromHidden pkg
        | seg <- drop 1 (T.splitOn "hidden package " err)
        , Just pkg <- [quotedToken seg]
        ]

hiddenPackage :: Text -> Maybe Text
hiddenPackage = listToMaybe . hiddenPackages

neededExtension :: Text -> Maybe Text
neededExtension err = firstJust (concatMap fromLine (T.lines err))
  where
    fromLine l = [afterInfix m l >>= extOf | m <- ["intended to use ", "-X"]]
    extOf rest =
        let tok = T.takeWhile isAlphaNum (T.dropWhile (not . isUpper) rest)
         in if tok `elem` knownExtensions then Just tok else Nothing

misnamedModule :: Text -> Maybe (Text, Text)
misnamedModule err = do
    wrong <- afterPhrase "Could not find module " err
    right <- moduleAfter "Perhaps you meant" err
    pure (wrong, right)

couldNotFindModules :: Text -> [Text]
couldNotFindModules err =
    nub
        [ m
        | seg <- drop 1 (T.splitOn "Could not find module " err)
        , Just m <- [quotedToken seg]
        ]

couldNotFindModule :: Text -> Maybe Text
couldNotFindModule = listToMaybe . couldNotFindModules

moduleAfter :: Text -> Text -> Maybe Text
moduleAfter phrase err = do
    rest <- afterInfix phrase err
    let tok = T.takeWhile isModChar (T.dropWhile (not . isUpper) rest)
    if T.null tok then Nothing else Just tok
  where
    isModChar c = isAlphaNum c || c == '.' || c == '_' || c == '\''

packageNeedsFlag :: Text -> Maybe Text
packageNeedsFlag err = do
    rest <- afterInfix "-package-id " err
    let tok = T.takeWhile pkgChar (T.stripStart rest)
    if T.null tok then Nothing else Just (packageFromHidden tok)
  where
    pkgChar c = isAlphaNum c || c == '-' || c == '.'

packageFromHidden :: Text -> Text
packageFromHidden = T.intercalate "-" . takeWhile (not . isVersion) . T.splitOn "-"
  where
    isVersion p = not (T.null p) && T.all (\c -> isDigit c || c == '.') p

afterPhrase :: Text -> Text -> Maybe Text
afterPhrase phrase t = case T.breakOn phrase t of
    (_, rest)
        | not (T.null rest) -> quotedToken (T.drop (T.length phrase) rest)
    _ -> Nothing

didYouMean :: Text -> [Guidance]
didYouMean err = case lineContaining "Perhaps you meant" err of
    Just l -> [Guidance "did-you-mean" (T.strip l)]
    Nothing -> []

letParse :: Text -> [Guidance]
letParse err
    | mentionsLetParseError err =
        [Guidance "top-level-let" topLevelLetMessage]
    | otherwise = []
  where
    mentionsLetParseError t =
        "parse error" `T.isInfixOf` t
            && ("let" `T.isInfixOf` t || ("=" `T.isInfixOf` t && "on input" `T.isInfixOf` t))

topLevelLetMessage :: Text
topLevelLetMessage =
    "GHCi rejected a top-level `let`. Write `x = 1` directly (no `let`). `let ... in\
    \ ...` expressions and `let` inside do/where blocks are fine."

holeFitGoal :: Text -> Maybe Text
holeFitGoal err = firstJust (map fromLine (T.lines err))
  where
    fromLine l = do
        rest <- afterInfix "not in scope:" l
        case T.breakOn "::" rest of
            (_, ty) | not (T.null ty) -> goalOf (T.strip (T.drop 2 ty))
            _ -> Nothing
    goalOf ty = if T.null ty then Nothing else Just ("_ :: " <> ty)

afterInfix :: Text -> Text -> Maybe Text
afterInfix needle t = case T.breakOn needle t of
    (_, rest) | not (T.null rest) -> Just (T.drop (T.length needle) rest)
    _ -> Nothing

afterInfixCI :: Text -> Text -> Maybe Text
afterInfixCI needle t = case T.breakOn needle (T.toLower t) of
    (pre, rest)
        | not (T.null rest) -> Just (T.drop (T.length pre + T.length needle) t)
    _ -> Nothing

data GrammarRoute = KeepRepairing | Rediscover Text
    deriving (Eq, Show)

routeFailure :: [Text] -> Text -> GrammarRoute
routeFailure offered err =
    case firstJust (map implicatedName (T.lines err)) of
        Just name | name `notElem` offered -> Rediscover name
        _ -> KeepRepairing

implicatedName :: Text -> Maybe Text
implicatedName l = firstJust [notInScopeName l, arisingFromName l]

notInScopeName :: Text -> Maybe Text
notInScopeName l = do
    rest <- afterInfixCI "not in scope:" l
    pure (firstToken (T.strip rest))
  where
    firstToken t = case quotedToken t of
        Just q -> q
        Nothing -> T.takeWhile (\c -> c /= ' ' && c /= ':') (T.strip t)

arisingFromName :: Text -> Maybe Text
arisingFromName l
    | "No instance for" `T.isInfixOf` l =
        afterInfix "arising from a use of" l >>= quotedToken
    | otherwise = Nothing

ambiguousOccurrences :: Text -> [(Text, [Text])]
ambiguousOccurrences err =
    [ (name, cands)
    | seg <- drop 1 (T.splitOn "Ambiguous occurrence" err)
    , let cands =
            [ q
            | l <- takeWhile (not . startsNextDiagnostic) (T.lines seg)
            , isCandidateLine l
            , Just q <- [quotedToken l]
            , "." `T.isInfixOf` q
            ]
    , not (null cands)
    , Just name <- [quotedToken (T.takeWhile (/= '\n') seg)]
    ]
  where
    isCandidateLine l =
        let s = T.stripStart l
         in "either " `T.isPrefixOf` s || "or " `T.isPrefixOf` s
    startsNextDiagnostic l =
        T.null (T.strip l)
            || any (`T.isPrefixOf` T.stripStart l) ["<interactive>:", "<no location info>:"]

ambiguousOccurrence :: Text -> Maybe (Text, [Text])
ambiguousOccurrence = listToMaybe . ambiguousOccurrences

ambiguousType :: Text -> [Guidance]
ambiguousType err
    | "Ambiguous type" `T.isInfixOf` err =
        [ Guidance
            "ambiguous-type"
            "Pin the type with an annotation, e.g. `(x :: Int)` or `(x :: Double)`."
        ]
    | otherwise = []

typeMismatch :: Text -> [Guidance]
typeMismatch err = case lineContaining "Couldn't match" err of
    Just l -> [Guidance "type-mismatch" (T.strip l)]
    Nothing -> []

quotedToken :: Text -> Maybe Text
quotedToken t =
    firstJust
        [ between '\8216' '\8217' t
        , between '`' '\'' t
        , between '\'' '\'' t
        ]

between :: Char -> Char -> Text -> Maybe Text
between open close t =
    case T.breakOn (T.singleton open) t of
        (_, rest)
            | not (T.null rest) ->
                let inner = T.drop 1 rest
                 in case T.breakOn (T.singleton close) inner of
                        (tok, after) | not (T.null after) && not (T.null tok) -> Just tok
                        _ -> Nothing
        _ -> Nothing

lineContaining :: Text -> Text -> Maybe Text
lineContaining needle = firstJust . map keep . T.lines
  where
    keep l = if needle `T.isInfixOf` l then Just l else Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust = foldr orElse Nothing
  where
    orElse (Just x) _ = Just x
    orElse Nothing y = y
