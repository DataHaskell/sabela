{-# LANGUAGE OverloadedStrings #-}

{- | Extracting structured facts from GHC diagnostic text: the module that was
not found, the hidden package, the extension it asked for, the ambiguous
occurrence and its candidates.
-}
module Sabela.Diagnose.Parse (
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
    holeFitGoal,
    GrammarRoute (..),
    routeFailure,
    afterInfix,
    lineContaining,
    quotedToken,
    firstJust,
    declaredPackages,
) where

import Data.Char (isAlphaNum, isDigit, isUpper)
import Data.List (nub)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Hints (knownExtensions)

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
packageFromHidden =
    T.intercalate "-" . takeWhile (not . isVersion) . T.splitOn "-"
  where
    isVersion p = not (T.null p) && T.all (\c -> isDigit c || c == '.') p

afterPhrase :: Text -> Text -> Maybe Text
afterPhrase phrase t = case T.breakOn phrase t of
    (_, rest)
        | not (T.null rest) -> quotedToken (T.drop (T.length phrase) rest)
    _ -> Nothing

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
            || any
                (`T.isPrefixOf` T.stripStart l)
                ["<interactive>:", "<no location info>:"]

ambiguousOccurrence :: Text -> Maybe (Text, [Text])
ambiguousOccurrence = listToMaybe . ambiguousOccurrences

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
                        (tok, after)
                            | not (T.null after) && not (T.null tok) -> Just tok
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

{- | The packages a cell declares on its @-- cabal:@ line. Empty when the cell
declares none, which is the signal to fall back to the global pool.
-}
declaredPackages :: Text -> [Text]
declaredPackages src =
    [ pkg
    | l <- T.lines src
    , Just rest <- [afterField l]
    , pkg <- map (T.takeWhile (/= ' ') . T.strip) (T.splitOn "," rest)
    , not (T.null pkg)
    ]
  where
    afterField l = case T.breakOn "build-depends:" l of
        (_, r) | not (T.null r) -> Just (T.drop (T.length "build-depends:") r)
        _ -> Nothing
