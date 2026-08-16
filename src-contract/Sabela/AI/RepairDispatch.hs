{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.RepairDispatch (
    depName,
    hiddenPackageFromDiagV,
    pinnedDep,
    splitPkgVersion,
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
    skipScopeDescriptors,
 )
import Sabela.AI.Hints (extensionHints)
import Sabela.AI.HoleRepair (afterInfixCI, arityFromError, goalFromError)
import Sabela.AI.Unshowable (unshowableShowType)

data DiagClass
    = ClassHiddenPackage
    | ClassModuleNotFound
    | ClassMissingExtension
    | ClassAmbiguous
    | ClassNotInScope
    | ClassArity
    | ClassRefinement
    | ClassUnshowable
    | ClassOther
    deriving (Bounded, Enum, Eq, Ord, Show)

diagClassText :: DiagClass -> Text
diagClassText c = case c of
    ClassHiddenPackage -> "installed-not-loaded"
    ClassModuleNotFound -> "module-not-found"
    ClassMissingExtension -> "missing-extension"
    ClassAmbiguous -> "ambiguous-occurrence"
    ClassNotInScope -> "not-in-scope"
    ClassArity -> "arity"
    ClassRefinement -> "refinement"
    ClassUnshowable -> "unshowable-display"
    ClassOther -> "other"

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
    | TierRenderWrap
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
    TierRenderWrap -> "render-wrap"

classifyDiag :: Text -> DiagClass
classifyDiag t
    | has "hidden package" = ClassHiddenPackage
    | has "could not find module" || has "could not load module" =
        ClassModuleNotFound
    | has "perhaps you intended to use" = ClassMissingExtension
    | has "ambiguous occurrence" = ClassAmbiguous
    | has "not in scope" = ClassNotInScope
    | isJust (unshowableShowType t) = ClassUnshowable
    | isJust (arityFromError t) = ClassArity
    | has "found hole" = ClassRefinement
    | otherwise = ClassOther
  where
    low = T.toLower t
    has p = p `T.isInfixOf` low

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
    ClassUnshowable -> [TierRenderWrap]
    ClassOther -> []

tierRequiresRestart :: RepairTier -> Bool
tierRequiresRestart = (== TierDepAdd)

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

hiddenPackageFromDiag :: Text -> Maybe Text
hiddenPackageFromDiag = fmap fst . hiddenPackageFromDiagV

-- | The hidden package with the version GHC's unit token named, when it did.
hiddenPackageFromDiagV :: Text -> Maybe (Text, Maybe Text)
hiddenPackageFromDiagV err = do
    rest <- afterInfix "hidden package" err
    tok <- listToMaybe (quotedTokens rest ++ T.words rest)
    let (p, v) = splitPkgVersion tok
    if T.null p then Nothing else Just (p, v)

missingModuleFromDiag :: Text -> Maybe Text
missingModuleFromDiag err = do
    rest <-
        afterInfix "ould not find module" err
            `orElse` afterInfix "ould not load module" err
    listToMaybe (quotedTokens rest ++ take 1 (T.words rest))

neededExtensionFromDiag :: Text -> Maybe Text
neededExtensionFromDiag = listToMaybe . extensionHints

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

notInScopeFromDiag :: Text -> Maybe Text
notInScopeFromDiag err =
    (fst <$> goalFromError err) `orElse` bare
  where
    bare = do
        rest <- afterInfixCI "not in scope:" err
        w <- listToMaybe (T.words (skipScopeDescriptors rest))
        let n = T.dropAround (`elem` ("\8216\8217`'()" :: String)) w
        if T.null n then Nothing else Just n

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

notRegressed :: Set Text -> Health -> Health -> Bool
notRegressed defined old new =
    healthMsgsFor defined new `Set.isSubsetOf` healthMsgsFor defined old

data RepairReport = RepairReport
    { rrClass :: DiagClass
    , rrAttempts :: Int
    , rrBudget :: Int
    , rrStop :: Text
    , rrKept :: Maybe (RepairTier, Text)
    , rrUnvalidated :: [Text]
    }
    deriving (Eq, Show)

reportCharBudget :: Int
reportCharBudget = 400

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

stripPkgVersion :: Text -> Text
stripPkgVersion u
    | null kept = u
    | otherwise = T.intercalate "-" kept
  where
    parts = T.splitOn "-" u
    kept = reverse (dropWhile isVer (reverse parts))
    isVer p = not (T.null p) && T.all (\c -> isDigit c || c == '.') p

-- | @text-2.0.2@ -> @("text", Just "2.0.2")@; a version-less token keeps Nothing.
splitPkgVersion :: Text -> (Text, Maybe Text)
splitPkgVersion tok
    | name == tok = (tok, Nothing)
    | otherwise = (name, Just (T.drop (T.length name + 1) tok))
  where
    name = stripPkgVersion tok

{- | A dependency entry pinned to the version its evidence names, so the
solver either keeps that version or fails loudly instead of downgrading.
-}
pinnedDep :: Text -> Maybe Text -> Text
pinnedDep name = maybe name (\v -> name <> " ==" <> v)

-- | The package name of a dependency entry, its constraint dropped.
depName :: Text -> Text
depName = T.strip . T.takeWhile (`notElem` (" =<>^" :: String)) . T.strip

afterInfix :: Text -> Text -> Maybe Text
afterInfix needle t = case T.breakOn needle t of
    (_, rest) | not (T.null rest) -> Just (T.drop (T.length needle) rest)
    _ -> Nothing

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing y = y

tShow :: Int -> Text
tShow = T.pack . show
