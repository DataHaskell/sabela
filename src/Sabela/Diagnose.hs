{-# LANGUAGE OverloadedStrings #-}

module Sabela.Diagnose (
    Guidance (..),
    diagnose,
    guidanceForCell,
    guidancePairs,
    cellResultWithGuidance,
    topLevelLetMessage,
    holeFitGoal,
    hiddenPackage,
    neededExtension,
    misnamedModule,
    couldNotFindModule,
    notInScopeName,
    ambiguousOccurrence,
    packageNeedsFlag,
    GrammarRoute (..),
    routeFailure,
) where

import Data.Aeson (ToJSON (..), Value (..), object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.Char (isAlphaNum, isDigit, isUpper)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.CellResult (CellOutcome (..), CellResult, crOutcome)
import Sabela.Diagnose.Packages (packageForModule, resolvePackageToken)
import Sabela.Model (CellError (..))

-- | One actionable hint: a stable category and a human-readable message.
data Guidance = Guidance
    { gCategory :: Text
    , gMessage :: Text
    }
    deriving (Eq, Show)

instance ToJSON Guidance where
    toJSON g = object ["category" .= gCategory g, "message" .= gMessage g]

-- | Run every rule over one error blob and dedupe the results.
diagnose :: Text -> [Guidance]
diagnose err = nub (concatMap ($ err) rules)

rules :: [Text -> [Guidance]]
rules = [missingModule, didYouMean, letParse, ambiguousType, typeMismatch]

{- | Diagnose every error an outcome carries (a runtime message or each
compile error). 'Succeeded'/'Aborted' carry nothing to diagnose.
-}
guidanceForCell :: CellResult -> [Guidance]
guidanceForCell = nub . concatMap diagnose . outcomeErrors . crOutcome

outcomeErrors :: CellOutcome -> [Text]
outcomeErrors (Raised m) = [m]
outcomeErrors (Rejected ds) = map ceMessage ds
outcomeErrors _ = []

-- | A @"guidance"@ wire pair for 'mergeToolOk', omitted when empty.
guidancePairs :: [Guidance] -> [Pair]
guidancePairs [] = []
guidancePairs gs = ["guidance" .= gs]

{- | The cell-result JSON with a @guidance@ array merged in when there is any,
for the @execution@ summary embedded by @insert_cell@/@replace_cell_source@.
-}
cellResultWithGuidance :: CellResult -> Value
cellResultWithGuidance cr = case (toJSON cr, guidanceForCell cr) of
    (Object o, gs@(_ : _)) -> Object (KM.insert "guidance" (toJSON gs) o)
    (v, _) -> v

------------------------------------------------------------------------
-- Rules
------------------------------------------------------------------------

{- | A missing import. Two real GHC shapes:

* correct module name, undeclared dependency: "Could not load module ‘M’. It is
  a member of the hidden package ‘pkg-1.2.3’." GHC names the package (with a
  version, and often repeated many times). We strip the version and foreground a
  single @-- cabal:@ line, which is the fix GHC buries in that wall of text.
* wrong/unknown module name: "Could not find module ‘M’". Resolve M through the
  curated table, falling back to a message that still teaches the mechanism.
-}
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

-- | The package named in GHC's "hidden package ‘pkg-1.2.3’", version stripped.
hiddenPackage :: Text -> Maybe Text
hiddenPackage err = packageFromHidden <$> afterPhrase "hidden package " err

{- | The LANGUAGE extension GHC says is missing, from its "Perhaps you intended to
use X" hint (X quoted or bare). GHC's token is authoritative, but it is validated
against 'knownExtensions' so a misparse can never inject a bogus pragma; Nothing
when no known extension is suggested.
-}
neededExtension :: Text -> Maybe Text
neededExtension err = firstJust (concatMap fromLine (T.lines err))
  where
    fromLine l = [afterInfix m l >>= extOf | m <- ["intended to use ", "-X"]]
    extOf rest =
        let tok = T.takeWhile isAlphaNum (T.dropWhile (not . isUpper) rest)
         in if tok `elem` knownExtensions then Just tok else Nothing

{- | The GHC extensions auto-fix will enable when GHC suggests them — the ones a
small model forgets most. A curated allow-list, not the full set: an unknown
suggestion falls through to the model rather than risking a wrong pragma.
-}
knownExtensions :: [Text]
knownExtensions =
    [ "OverloadedStrings"
    , "TemplateHaskell"
    , "ScopedTypeVariables"
    , "TypeApplications"
    , "FlexibleContexts"
    , "FlexibleInstances"
    , "MultiParamTypeClasses"
    , "RankNTypes"
    , "GADTs"
    , "DataKinds"
    , "DeriveFunctor"
    , "DeriveGeneric"
    , "DeriveAnyClass"
    , "GeneralizedNewtypeDeriving"
    , "DerivingStrategies"
    , "StandaloneDeriving"
    , "TupleSections"
    , "LambdaCase"
    , "MultiWayIf"
    , "RecordWildCards"
    , "NamedFieldPuns"
    , "BangPatterns"
    , "ViewPatterns"
    , "ConstraintKinds"
    , "KindSignatures"
    , "PolyKinds"
    , "TypeFamilies"
    , "TypeOperators"
    , "InstanceSigs"
    , "ExistentialQuantification"
    , "FunctionalDependencies"
    , "QuasiQuotes"
    , "BlockArguments"
    , "ImportQualifiedPost"
    , "NumericUnderscores"
    , "OverloadedLabels"
    ]

{- | A wrong module name GHC offered a correction for: "Could not find module 'X'.
Perhaps you meant Y" → (X, Y), Y the first suggested module. Drives the import-name
auto-fix; Nothing when GHC made no suggestion (then we cannot safely rename).
-}
misnamedModule :: Text -> Maybe (Text, Text)
misnamedModule err = do
    wrong <- afterPhrase "Could not find module " err
    right <- moduleAfter "Perhaps you meant" err
    pure (wrong, right)

{- | The module named in "Could not find module ‘M’", whether or not GHC offered a
correction. 'misnamedModule' handles the with-hint rename; this exposes the bare
name so a module-index repair can act when GHC gives no "Perhaps you meant".
-}
couldNotFindModule :: Text -> Maybe Text
couldNotFindModule = afterPhrase "Could not find module "

{- | The first module-name token after a phrase: skip to the first uppercase, then
take the dotted identifier (stops at the space before GHC's "(needs flag …)").
-}
moduleAfter :: Text -> Text -> Maybe Text
moduleAfter phrase err = do
    rest <- afterInfix phrase err
    let tok = T.takeWhile isModChar (T.dropWhile (not . isUpper) rest)
    if T.null tok then Nothing else Just tok
  where
    isModChar c = isAlphaNum c || c == '.' || c == '_' || c == '\''

{- | The package in GHC's "(needs flag -package-id pkg-1.2.3 …)" note, version
stripped — the dependency an import-rename also needs declared.
-}
packageNeedsFlag :: Text -> Maybe Text
packageNeedsFlag err = do
    rest <- afterInfix "-package-id " err
    let tok = T.takeWhile pkgChar (T.stripStart rest)
    if T.null tok then Nothing else Just (packageFromHidden tok)
  where
    pkgChar c = isAlphaNum c || c == '-' || c == '.'

-- | "granite-0.7.3.0" -> "granite"; "foo-bar-1.0" -> "foo-bar".
packageFromHidden :: Text -> Text
packageFromHidden = T.intercalate "-" . takeWhile (not . isVersion) . T.splitOn "-"
  where
    isVersion p = not (T.null p) && T.all (\c -> isDigit c || c == '.') p

-- | The quoted token immediately following a phrase, if present.
afterPhrase :: Text -> Text -> Maybe Text
afterPhrase phrase t = case T.breakOn phrase t of
    (_, rest)
        | not (T.null rest) -> quotedToken (T.drop (T.length phrase) rest)
    _ -> Nothing

{- | GHC already computed a correction ("Perhaps you meant ‘x’"); surface it
rather than leaving it buried in the raw text.
-}
didYouMean :: Text -> [Guidance]
didYouMean err = case lineContaining "Perhaps you meant" err of
    Just l -> [Guidance "did-you-mean" (T.strip l)]
    Nothing -> []

-- | The classic top-level @let@ parse error (kept from the scratchpad).
letParse :: Text -> [Guidance]
letParse err
    | mentionsLetParseError err =
        [Guidance "top-level-let" topLevelLetMessage]
    | otherwise = []
  where
    mentionsLetParseError t =
        "parse error" `T.isInfixOf` t
            && ("let" `T.isInfixOf` t || ("=" `T.isInfixOf` t && "on input" `T.isInfixOf` t))

{- | The single deduped message for a rejected top-level @let@, shared by the
post-GHC 'letParse' rule and the pre-GHC structural validator in
"Sabela.Parse" so both paths say exactly the same thing.
-}
topLevelLetMessage :: Text
topLevelLetMessage =
    "GHCi rejected a top-level `let`. Write `x = 1` directly (no `let`). `let ... in\
    \ ...` expressions and `let` inside do/where blocks are fine."

{- | When GHC reports a not-in-scope name with a printed type
(@Variable not in scope: foo :: A -> B@, GHC-88464), build the concrete typed
hole @_ :: A -> B@ that 'Sabela.SessionTypes.sbQueryHoleFits' can query for
real in-scope names. 'Nothing' when there is no printed type to anchor on.
-}
holeFitGoal :: Text -> Maybe Text
holeFitGoal err = firstJust (map fromLine (T.lines err))
  where
    fromLine l = do
        rest <- afterInfix "not in scope:" l
        case T.breakOn "::" rest of
            (_, ty) | not (T.null ty) -> goalOf (T.strip (T.drop 2 ty))
            _ -> Nothing
    goalOf ty = if T.null ty then Nothing else Just ("_ :: " <> ty)

-- | The text after the first occurrence of @needle@, if present.
afterInfix :: Text -> Text -> Maybe Text
afterInfix needle t = case T.breakOn needle t of
    (_, rest) | not (T.null rest) -> Just (T.drop (T.length needle) rest)
    _ -> Nothing

{- | 'afterInfix' matching @needle@ case-insensitively but returning the tail from
the ORIGINAL text, so the name after it keeps its case. GHC writes both @Variable
not in scope:@ and @Not in scope:@; the lowercase match catches both.
-}
afterInfixCI :: Text -> Text -> Maybe Text
afterInfixCI needle t = case T.breakOn needle (T.toLower t) of
    (pre, rest)
        | not (T.null rest) -> Just (T.drop (T.length pre + T.length needle) t)
    _ -> Nothing

------------------------------------------------------------------------
-- E2: the seam between "regrounded" and "keep repairing"
------------------------------------------------------------------------

{- | Where a failing in-grammar cell routes. 'KeepRepairing' is an ordinary type
bug inside a construct the grammar offered, so the loop repairs and never
abandons the grammar. 'Rediscover' carries the un-offered name that implicated
the grammar (a not-in-scope/no-instance on a name the grammar did not name), so
the loop regrounds via discover/hole-fits instead of repairing in place.
-}
data GrammarRoute = KeepRepairing | Rediscover Text
    deriving (Eq, Show)

{- | Decide whether a failure abandons the grammar. Abandonment is gated on a
grammar-implicated error only: a not-in-scope or no-instance error naming a
token the grammar did NOT offer. Every other failure — a type mismatch, an
ambiguous type, or a name the grammar DID offer — keeps repairing, because the
grammar named the right thing and the bug is inside it.
-}
routeFailure :: [Text] -> Text -> GrammarRoute
routeFailure offered err =
    case firstJust (map implicatedName (T.lines err)) of
        Just name | name `notElem` offered -> Rediscover name
        _ -> KeepRepairing

{- | The identifier a not-in-scope or no-instance error implicates, if any. A
not-in-scope line implicates the missing name; a no-instance line implicates
the @arising from a use of ‘X’@ name. A bare type mismatch implicates nothing.
-}
implicatedName :: Text -> Maybe Text
implicatedName l = firstJust [notInScopeName l, arisingFromName l]

-- | The name in @Variable not in scope: foo[ :: ty]@ / @Not in scope: ‘foo’@.
notInScopeName :: Text -> Maybe Text
notInScopeName l = do
    rest <- afterInfixCI "not in scope:" l
    pure (firstToken (T.strip rest))
  where
    firstToken t = case quotedToken t of
        Just q -> q
        Nothing -> T.takeWhile (\c -> c /= ' ' && c /= ':') (T.strip t)

-- | The name in a @No instance for ... arising from a use of ‘X’@ line.
arisingFromName :: Text -> Maybe Text
arisingFromName l
    | "No instance for" `T.isInfixOf` l =
        afterInfix "arising from a use of" l >>= quotedToken
    | otherwise = Nothing

{- | The name and module-qualified candidates an @Ambiguous occurrence 'take' …
either 'Prelude.take' or 'DataFrame.take'@ error names, read off its @either@\/@or@
lines. 'Nothing' for an ambiguous TYPE — a different error, fixed by an annotation.
-}
ambiguousOccurrence :: Text -> Maybe (Text, [Text])
ambiguousOccurrence err
    | not ("Ambiguous occurrence" `T.isInfixOf` err) = Nothing
    | otherwise = do
        header <- afterInfix "Ambiguous occurrence" err
        name <- quotedToken (T.takeWhile (/= '\n') header)
        let cands =
                [ q
                | l <- T.lines err
                , isCandidateLine l
                , Just q <- [quotedToken l]
                , "." `T.isInfixOf` q
                ]
        if null cands then Nothing else Just (name, cands)
  where
    isCandidateLine l =
        let s = T.stripStart l
         in "either " `T.isPrefixOf` s || "or " `T.isPrefixOf` s

ambiguousType :: Text -> [Guidance]
ambiguousType err
    | "Ambiguous type" `T.isInfixOf` err =
        [ Guidance
            "ambiguous-type"
            "Pin the type with an annotation, e.g. `(x :: Int)` or `(x :: Double)`."
        ]
    | otherwise = []

-- | A type mismatch; surface GHC's expected/actual line directly.
typeMismatch :: Text -> [Guidance]
typeMismatch err = case lineContaining "Couldn't match" err of
    Just l -> [Guidance "type-mismatch" (T.strip l)]
    Nothing -> []

------------------------------------------------------------------------
-- Text helpers
------------------------------------------------------------------------

-- | The first token in smart ‘…’, backtick `…', or straight '…' quotes.
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
