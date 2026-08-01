{-# LANGUAGE OverloadedStrings #-}

module Sabela.Diagnose (
    Guidance (..),
    diagnoseWith,
    guidanceForCell,
    guidancePairs,
    cellResultWithGuidance,
    cellResultWithExtraGuidance,
    withGuidance,
    hasTopLevelLet,
    topLevelLetMessage,
    ambiguousTypeMessage,
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
import Data.List (nub)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.CellResult (CellOutcome (..), CellResult, crOutcome)
import Sabela.AI.Health (namesHarnessBinder)
import Sabela.AI.Unshowable (unshowableGuidanceMessage)
import Sabela.Diagnose.Packages (resolvePackageToken)
import Sabela.Diagnose.Parse (
    GrammarRoute (..),
    ambiguousOccurrence,
    ambiguousOccurrences,
    couldNotFindModule,
    couldNotFindModules,
    declaredPackages,
    hiddenPackage,
    hiddenPackages,
    holeFitGoal,
    lineContaining,
    misnamedModule,
    neededExtension,
    notInScopeName,
    packageNeedsFlag,
    quotedToken,
    routeFailure,
 )
import Sabela.Model (CellError (..))
import Sabela.Parse.Preprocess (hasTopLevelLet)

data Guidance = Guidance
    { gCategory :: Text
    , gMessage :: Text
    }
    deriving (Eq, Show)

instance ToJSON Guidance where
    toJSON g = object ["category" .= gCategory g, "message" .= gMessage g]

{- | What a category asserts about the source before its advice can be true.
Every rule states one, so a category cannot be added without deciding what
makes it honest, and `NoClaim` has to be written down.
-}
data SourceClaim = NoClaim | WritesTopLevelLet

claimHolds :: SourceClaim -> Text -> Bool
claimHolds NoClaim _ = True
claimHolds WritesTopLevelLet src = hasTopLevelLet src

{- | Everything a rule may read: the diagnostic, the source it was measured
on, and the package the store says exposes a module GHC could not find.
-}
data Reading = Reading
    { readSource :: Text
    , readExposedBy :: Maybe Text
    , readError :: Text
    }

data Rule = Rule
    { ruleCategory :: Text
    , ruleClaim :: SourceClaim
    , ruleMessages :: Reading -> [Text]
    }

rules :: [Rule]
rules =
    [ Rule "missing-dependency" NoClaim missingModule
    , Rule "did-you-mean" NoClaim (didYouMean . readError)
    , Rule "top-level-let" WritesTopLevelLet (letParse . readError)
    , Rule "ambiguous-type" NoClaim (ambiguousType . readError)
    , Rule "type-mismatch" NoClaim (typeMismatch . readError)
    , Rule "unshowable-result" NoClaim (unshowableResult . readError)
    ]

{- | The one classifier: no caller can ask for advice without handing over the
source it is advice about. Advice is dropped when its category's claim fails
of that source, and when it would repeat a binder the harness itself invented.
-}
diagnoseWith :: Maybe Text -> Text -> Text -> [Guidance]
diagnoseWith exposedBy src err =
    nub
        [ Guidance (ruleCategory r) m
        | r <- rules
        , claimHolds (ruleClaim r) src
        , m <- ruleMessages r (Reading src exposedBy err)
        , not (namesHarnessBinder m)
        ]

unshowableResult :: Text -> [Text]
unshowableResult = maybeToList . unshowableGuidanceMessage

guidanceForCell :: Text -> CellResult -> [Guidance]
guidanceForCell src =
    nub . concatMap (diagnoseWith Nothing src) . outcomeErrors . crOutcome

outcomeErrors :: CellOutcome -> [Text]
outcomeErrors (Raised m) = [m]
outcomeErrors (Rejected ds) = map ceMessage ds
outcomeErrors _ = []

guidancePairs :: [Guidance] -> [Pair]
guidancePairs [] = []
guidancePairs gs = ["guidance" .= gs]

cellResultWithGuidance :: Text -> CellResult -> Value
cellResultWithGuidance src = cellResultWithExtraGuidance src []

cellResultWithExtraGuidance :: Text -> [Guidance] -> CellResult -> Value
cellResultWithExtraGuidance src extra cr =
    withGuidance (guidanceForCell src cr ++ extra) cr

-- | A cell result carrying guidance already classified against its source.
withGuidance :: [Guidance] -> CellResult -> Value
withGuidance gs cr = case (toJSON cr, gs) of
    (Object o, _ : _) -> Object (KM.insert "guidance" (toJSON gs) o)
    (v, _) -> v

missingModule :: Reading -> [Text]
missingModule r
    | Just pkg <- hiddenPackage err = [declareMessage r pkg]
    | any (`T.isInfixOf` err) findMarkers
    , Just m <- quotedToken err =
        case resolvePackageToken m of
            Just pkg -> [declareMessage r pkg]
            Nothing -> case readExposedBy r of
                Just pkg -> [declareMessage r pkg]
                Nothing -> [unresolvedMessage r m]
    | otherwise = []
  where
    err = readError r
    findMarkers =
        [ "Could not find module"
        , "Failed to load interface for"
        , "is a package, not a module"
        ]

{- | "Declare this package" is advice only when the source does not already
declare it. The source is whatever the caller supplied — a cell, a snippet or a
whole notebook — so the report half says only that it is declared, not where.
-}
declareMessage :: Reading -> Text -> Text
declareMessage r pkg
    | pkg `elem` declaredPackages (readSource r) = alreadyDeclared
    | otherwise = imperative
  where
    subject = maybe "That module" ("Module " <>) (couldNotFindModule (readError r))
    installNote =
        "  (there is no `cabal install`; Sabela installs declared deps on the next\
        \ run, which restarts the kernel once)."
    imperative =
        "Add this as the FIRST line of the cell: -- cabal: build-depends: "
            <> pkg
            <> installNote
    alreadyDeclared =
        subject
            <> " is still not found although "
            <> pkg
            <> " is already declared. Check the module name, or search for the \
               \module to find the package that really exposes it."

{- | Nothing named a package for the module. What the source declares is a fact
about the source; whether those packages expose the module is not something the
harness looked up, so it is not asserted either way.
-}
unresolvedMessage :: Reading -> Text -> Text
unresolvedMessage r m = case declaredPackages (readSource r) of
    declared@(_ : _) ->
        "Module "
            <> m
            <> " was not found, and the harness has no package to name for it. \
               \Already declared: "
            <> T.intercalate ", " declared
            <> ". Check the module name, or search for the module to find the \
               \package that exposes it and add that one to the cell's \
               \-- cabal: build-depends: line."
    [] ->
        "Module "
            <> m
            <> " was not found. If it belongs to a package, declare it as\
               \ the FIRST line of the cell: -- cabal: build-depends: <package>  (not\
               \ `cabal install`)."

didYouMean :: Text -> [Text]
didYouMean err = case lineContaining "Perhaps you meant" err of
    Just l -> [T.strip l]
    Nothing -> []

{- | Only GHC naming @let@ as the offending token licenses this. The old test
also fired on any parse error mentioning @=@, and on GHC's own suggested fix,
whose wording contains @let@ whatever the cell wrote.
-}
letParse :: Text -> [Text]
letParse err
    | mentionsLetParseError err = [topLevelLetMessage]
    | otherwise = []
  where
    mentionsLetParseError t =
        "parse error" `T.isInfixOf` T.toLower t
            && any (`T.isInfixOf` t) offendingLet
    offendingLet =
        ["parse error on input \8216let\8217", "parse error on input `let'"]

topLevelLetMessage :: Text
topLevelLetMessage =
    "GHCi rejected a top-level `let`. Write `x = 1` directly (no `let`). `let ... in\
    \ ...` expressions and `let` inside do/where blocks are fine."

ambiguousType :: Text -> [Text]
ambiguousType err
    | "Ambiguous type" `T.isInfixOf` err = [ambiguousTypeMessage]
    | otherwise = []

{- | Fixed advice: it names nothing from the source or the diagnostic, so it
can contradict neither. Interpolating either would need a claim to back it.
-}
ambiguousTypeMessage :: Text
ambiguousTypeMessage =
    "Pin the type with an annotation, e.g. `(x :: Int)` or `(x :: Double)`."

typeMismatch :: Text -> [Text]
typeMismatch err = case lineContaining "Couldn't match" err of
    Just l -> [T.strip l]
    Nothing -> []
