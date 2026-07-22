{-# LANGUAGE OverloadedStrings #-}

{- | ONE repair dispatcher: the GHC diagnostic CLASS — never a library name —
selects the repair tiers, and one notebook-scope acceptance law (keep iff the
target heals and no sibling regresses) governs every tier. Shared by the
agent's red-cell cascade and the property suite.
-}
module Sabela.AI.RepairDispatch (
    DiagClass (..),
    RepairTier (..),
    classifyDiag,
    diagClassText,
    tiersFor,
    tierRequiresRestart,
    tierText,
    hiddenPackageFromDiag,
    missingModuleFromDiag,
    neededExtensionFromDiag,
    ambiguousFromDiag,
    notInScopeFromDiag,
    quotedTokens,
    acceptRepair,
    notRegressed,
    RepairReport (..),
    renderRepairReport,
    reportCharBudget,
) where

import Data.Char (isAlphaNum, isDigit, isUpper)
import Data.Maybe (isJust, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Health (
    Health,
    healthMsgsFor,
    improvesHealthFor,
    isClean,
 )
import Sabela.AI.HoleRepair (afterInfixCI, arityFromError, goalFromError)

-- | The diagnostic classes the dispatcher keys on (phrase-shaped, never a name).
data DiagClass
    = ClassHiddenPackage
    | ClassModuleNotFound
    | ClassMissingExtension
    | ClassAmbiguous
    | ClassNotInScope
    | ClassArity
    | ClassRefinement
    | ClassOther
    deriving (Bounded, Enum, Eq, Ord, Show)

diagClassText :: DiagClass -> Text
diagClassText c = case c of
    ClassHiddenPackage -> "hidden-package"
    ClassModuleNotFound -> "module-not-found"
    ClassMissingExtension -> "missing-extension"
    ClassAmbiguous -> "ambiguous-occurrence"
    ClassNotInScope -> "not-in-scope"
    ClassArity -> "arity"
    ClassRefinement -> "refinement"
    ClassOther -> "other"

-- | The repair tiers of the one cascade, in dispatch vocabulary.
data RepairTier
    = TierDepAdd
    | TierExtensionAdd
    | TierModuleRename
    | TierAddImport
    | TierNameResolve
    | TierQualify
    | TierTypeDirected
    | TierHoleFit
    | TierArity
    deriving (Bounded, Enum, Eq, Ord, Show)

tierText :: RepairTier -> Text
tierText t = case t of
    TierDepAdd -> "dep-add"
    TierExtensionAdd -> "extension-add"
    TierModuleRename -> "module-rename"
    TierAddImport -> "add-import"
    TierNameResolve -> "name-resolve"
    TierQualify -> "qualify"
    TierTypeDirected -> "type-directed"
    TierHoleFit -> "hole-fit"
    TierArity -> "arity"

{- | Classify one diagnostic by its GHC phrase shape alone; substituting any
library, module or package name in the text can never change the class.
-}
classifyDiag :: Text -> DiagClass
classifyDiag t
    | has "hidden package" = ClassHiddenPackage
    | has "could not find module" || has "could not load module" =
        ClassModuleNotFound
    | has "perhaps you intended to use" = ClassMissingExtension
    | has "ambiguous occurrence" = ClassAmbiguous
    | has "not in scope" = ClassNotInScope
    | isJust (arityFromError t) = ClassArity
    | has "found hole" = ClassRefinement
    | otherwise = ClassOther
  where
    low = T.toLower t
    has p = p `T.isInfixOf` low

-- | The ordered tiers a class dispatches to — a function of the class ALONE.
tiersFor :: DiagClass -> [RepairTier]
tiersFor c = case c of
    ClassHiddenPackage -> [TierDepAdd]
    ClassModuleNotFound -> [TierDepAdd, TierModuleRename]
    ClassMissingExtension -> [TierExtensionAdd]
    ClassAmbiguous -> [TierQualify]
    ClassNotInScope ->
        [TierNameResolve, TierAddImport, TierHoleFit, TierTypeDirected]
    ClassArity -> [TierArity]
    ClassRefinement -> [TierHoleFit, TierTypeDirected]
    ClassOther -> []

{- | A dep-declaring rewrite restarts the kernel, so verify-and-revert cannot
validate it; the report must disclose it as unvalidated (R7.3).
-}
tierRequiresRestart :: RepairTier -> Bool
tierRequiresRestart = (== TierDepAdd)

-- | Tokens GHC quoted with either @‘…’@ or @`…'@ style, in message order.
quotedTokens :: Text -> [Text]
quotedTokens t =
    concat
        [ toks open close
        | (open, close) <- [("\8216", "\8217"), ("`", "'")]
        ]
  where
    toks open close =
        [ tok
        | chunk <- drop 1 (T.splitOn open t)
        , let (tok, rest) = T.breakOn close chunk
        , not (T.null rest)
        , not (T.null tok)
        ]

-- | The package a hidden-package diagnostic names, version stripped.
hiddenPackageFromDiag :: Text -> Maybe Text
hiddenPackageFromDiag err = do
    rest <- afterInfix "hidden package" err
    tok <- listToMaybe (quotedTokens rest ++ T.words rest)
    let p = stripPkgVersion tok
    if T.null p then Nothing else Just p

-- | The module a could-not-find/load-module diagnostic names.
missingModuleFromDiag :: Text -> Maybe Text
missingModuleFromDiag err = do
    rest <-
        afterInfix "ould not find module" err
            `orElse` afterInfix "ould not load module" err
    listToMaybe (quotedTokens rest ++ take 1 (T.words rest))

-- | The extension GHC's "Perhaps you intended to use X" hint names.
neededExtensionFromDiag :: Text -> Maybe Text
neededExtensionFromDiag err = do
    rest <- afterInfix "erhaps you intended to use" err
    w <- listToMaybe (quotedTokens rest ++ T.words rest)
    let ext = T.filter isAlphaNum w
    if extLike ext then Just ext else Nothing
  where
    extLike e = maybe False (isUpper . fst) (T.uncons e)

-- | The (name, qualified candidates) of an ambiguous-occurrence diagnostic.
ambiguousFromDiag :: Text -> Maybe (Text, [Text])
ambiguousFromDiag err = do
    rest <- afterInfix "mbiguous occurrence" err
    name <- listToMaybe (quotedTokens rest)
    let quals =
            [ q
            | q <- quotedTokens rest
            , ("." <> name) `T.isSuffixOf` q
            ]
    if null quals then Nothing else Just (name, quals)

{- | The wrong name of a not-in-scope diagnostic (with or without a goal
type), matched casefolded so every GHC phrase variant extracts — including
the capitalised "Not in scope: type constructor or class" form.
-}
notInScopeFromDiag :: Text -> Maybe Text
notInScopeFromDiag err =
    (fst <$> goalFromError err) `orElse` bare
  where
    bare = do
        rest <- afterInfixCI "not in scope:" err
        w <- listToMaybe (T.words (skipDescriptors rest))
        let n = T.dropAround (`elem` ("\8216\8217`'()" :: String)) w
        if T.null n then Nothing else Just n

-- | Drop GHC's entity-descriptor prose between the phrase and the name.
skipDescriptors :: Text -> Text
skipDescriptors t0 = go (T.stripStart t0)
  where
    go t = case listToMaybe [d | d <- descriptors, matches d t] of
        Just d -> go (T.stripStart (T.drop (T.length d) t))
        Nothing -> t
    matches d t = T.toLower d `T.isPrefixOf` T.toLower t
    descriptors =
        ["type constructor or class", "data constructor", "record field"]

{- | The one acceptance law (R7.5): keep a repair iff the repaired cell ends
clean (or strictly improved) AND no sibling regressed — compared through
'healthMsgsFor', so knock-on echoes of the target's own names are excluded.
-}
acceptRepair ::
    Set Text -> [(Text, Health)] -> [(Text, Health)] -> Text -> Bool
acceptRepair defined before after target =
    targetOk && all siblingOk siblings
  where
    targetOk = case (lookup target before, lookup target after) of
        (Just o, Just n) -> isClean n || improvesHealthFor defined o n
        _ -> False
    siblings = [cid | (cid, _) <- after, cid /= target]
    siblingOk cid = case (lookup cid before, lookup cid after) of
        (Just o, Just n) -> notRegressed defined o n
        (Nothing, Just n) -> isClean n
        _ -> True

-- | No new diagnostic messages (knock-on excluded); equal or improved is fine.
notRegressed :: Set Text -> Health -> Health -> Bool
notRegressed defined old new =
    healthMsgsFor defined new `Set.isSubsetOf` healthMsgsFor defined old

{- | What crossed the proposer's surface: attempts run, why the cascade
stopped, the ONE surviving candidate — never vet transcripts or rejected
samples (R3.9; the type cannot even hold them).
-}
data RepairReport = RepairReport
    { rrClass :: DiagClass
    , rrAttempts :: Int
    , rrBudget :: Int
    , rrStop :: Text
    , rrKept :: Maybe (RepairTier, Text)
    , rrUnvalidated :: [Text]
    }
    deriving (Eq, Show)

-- | Hard cap on the rendered report (context-efficiency goal).
reportCharBudget :: Int
reportCharBudget = 400

-- | One bounded line: attempts, stop reason, kept candidate, R7.3 disclosure.
renderRepairReport :: RepairReport -> Text
renderRepairReport r =
    clamp reportCharBudget
        . T.intercalate "; "
        $ filter (not . T.null) [headline, keptLine, unvalLine]
  where
    headline =
        "repair ("
            <> diagClassText (rrClass r)
            <> "): "
            <> tShow (rrAttempts r)
            <> " of "
            <> tShow (rrBudget r)
            <> " candidates tried — "
            <> rrStop r
    keptLine = case rrKept r of
        Just (t, summ) -> "kept via " <> tierText t <> ": " <> clamp 160 summ
        Nothing -> ""
    unvalLine = case rrUnvalidated r of
        [] -> ""
        us ->
            "unvalidated (repair requires a kernel restart): "
                <> T.intercalate ", " (map (clamp 60) (take 2 us))

clamp :: Int -> Text -> Text
clamp n t = if T.length t > n then T.take n t <> "…" else t

-- | Drop a trailing @-1.2.3@ style version from a package token.
stripPkgVersion :: Text -> Text
stripPkgVersion u
    | null kept = u
    | otherwise = T.intercalate "-" kept
  where
    parts = T.splitOn "-" u
    kept = reverse (dropWhile isVer (reverse parts))
    isVer p = not (T.null p) && T.all (\c -> isDigit c || c == '.') p

afterInfix :: Text -> Text -> Maybe Text
afterInfix needle t = case T.breakOn needle t of
    (_, rest) | not (T.null rest) -> Just (T.drop (T.length needle) rest)
    _ -> Nothing

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing y = y

tShow :: Int -> Text
tShow = T.pack . show
