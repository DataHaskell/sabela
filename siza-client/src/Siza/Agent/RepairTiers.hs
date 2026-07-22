{-# LANGUAGE OverloadedStrings #-}

{- | Pure candidate generation for the agent's repair cascade: each tier maps a
diagnostic + cell source to rewrite candidates, recording the names it proposes
so heal and search cross-check against one catalogue (R7.6). Vetting (apply,
re-run, keep-iff-notebook-improves) lives in "Siza.Agent.Repair".
-}
module Siza.Agent.RepairTiers (
    Candidate (..),
    TierInput (..),
    candidatesFor,
    tierCandidates,
    addDepLine,
    addPragmaLine,
    addImportLine,
    addQualifiedImportLine,
    splitQualified,
) where

import Data.Char (isUpper)
import Data.List (nub, permutations, sortOn)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoleFits (HoleFit (..), parseHoleFits)
import Sabela.AI.HoleRepair (
    arityFromError,
    goalFromError,
    holeFitNames,
    holeTypeFromDiagnostic,
    orderBySimilarity,
    substituteName,
    suggestedNames,
 )
import Sabela.AI.RepairDispatch (
    RepairTier (..),
    ambiguousFromDiag,
    hiddenPackageFromDiag,
    missingModuleFromDiag,
    neededExtensionFromDiag,
    notInScopeFromDiag,
 )
import Sabela.AI.SelfHeal (plausibleRename)

{- | What a tier may draw on: the diagnostic, the cell source, a hole-fit
blob (when fetched), the discover-backed locator for a name's modules
(module, Just package when a dep must be declared), and the nearest-module
locator for a wrong module spelling (same catalogue, R7.6).
-}
data TierInput = TierInput
    { tiDiag :: Text
    , tiSource :: Text
    , tiHoleFits :: Text
    , tiLocate :: Text -> [(Text, Maybe Text)]
    , tiModules :: Text -> [Text]
    }

-- | One rewrite candidate plus the names/modules/packages it introduces.
data Candidate = Candidate
    { cdTier :: RepairTier
    , cdSource :: Text
    , cdProposes :: [Text]
    }
    deriving (Eq, Show)

-- | All tiers' candidates in dispatch order, deduped on the rewritten source.
candidatesFor :: [RepairTier] -> TierInput -> [Candidate]
candidatesFor tiers input =
    dedupOn cdSource (concatMap (`tierCandidates` input) tiers)

-- | One tier's candidates; a tier this client cannot generate yields none.
tierCandidates :: RepairTier -> TierInput -> [Candidate]
tierCandidates tier input = case tier of
    TierDepAdd ->
        [ Candidate TierDepAdd src' [p]
        | Just p <- [hiddenPackageFromDiag diag]
        , let src' = addDepLine p src
        , src' /= src
        ]
    TierExtensionAdd ->
        [ Candidate TierExtensionAdd src' []
        | Just ext <- [neededExtensionFromDiag diag]
        , let src' = addPragmaLine ext src
        , src' /= src
        ]
    TierNameResolve ->
        [ Candidate TierNameResolve src' [cand]
        | Just wrong <- [notInScopeFromDiag diag]
        , cand <- orderBySimilarity wrong (nub (suggestedNames diag))
        , plausibleRename wrong cand
        , let src' = substituteName wrong cand src
        , src' /= src
        ]
    TierAddImport ->
        [ Candidate TierAddImport src' [m]
        | Just wrong <- [notInScopeFromDiag diag]
        , let (mAlias, bare) = splitQualified wrong
        , (m, mPkg) <- tiLocate input bare
        , let imp = case mAlias of
                Just a -> addQualifiedImportLine m a src
                Nothing -> addImportLine m bare src
        , let src' = withDep mPkg imp
        , src' /= src
        ]
    TierQualify ->
        [ Candidate TierQualify src' [qual]
        | Just (name, quals) <- [ambiguousFromDiag diag]
        , qual <- quals
        , let src' = substituteName name qual src
        , src' /= src
        ]
    TierHoleFit ->
        [ Candidate TierHoleFit src' [n]
        | Just (wrong, _) <- [holeTargetTyped diag]
        , n <- holeFitNames (tiHoleFits input <> "\n" <> diag)
        , let src' = substituteName wrong n src
        , src' /= src
        ]
    TierTypeDirected ->
        [ Candidate TierTypeDirected src' [n]
        | Just (wrong, Just goalTy) <- [holeTargetTyped diag]
        , n <- goalTypedFits goalTy (tiHoleFits input)
        , let src' = substituteName wrong n src
        , src' /= src
        ]
    TierArity ->
        [ Candidate TierArity src' []
        | _ <- maybe [] (const [()]) (arityFromError diag)
        , src' <- permuteApplications src
        , src' /= src
        ]
    TierModuleRename ->
        [ Candidate TierModuleRename src' [m]
        | Just wrong <- [missingModuleFromDiag diag]
        , m <- tiModules input wrong
        , m /= wrong
        , let src' = substituteName wrong m src
        , src' /= src
        ]
  where
    diag = tiDiag input
    src = tiSource input
    withDep Nothing s = s
    withDep (Just p) s = addDepLine p s

{- | The (rewrite-target, maybe goal-type) a refinement/not-in-scope diagnostic
implies: the not-in-scope name with its inferred type when GHC named one, else a
Found-hole's @_@ token and hole type (live_test cell 4). The hole-fit tier takes
either; the type-directed tier takes the typed case (so an empty-fit hole heals).
-}
holeTargetTyped :: Text -> Maybe (Text, Maybe Text)
holeTargetTyped diag = case goalFromError diag of
    Just (w, ty) -> Just (w, Just ty)
    Nothing -> case notInScopeFromDiag diag of
        Just w -> Just (w, Nothing)
        Nothing -> (\ty -> ("_", Just ty)) <$> holeTypeFromDiagnostic diag

{- | Split an alias-qualified name (@T.unpack@) into (alias, bare name); the
catalogue is queried with the BARE name only, never the qualified spelling.
-}
splitQualified :: Text -> (Maybe Text, Text)
splitQualified w = case T.breakOnEnd "." w of
    (qual, bare)
        | not (T.null qual)
        , not (T.null bare)
        , upperHead (T.dropEnd 1 qual) ->
            (Just (T.dropEnd 1 qual), bare)
    _ -> (Nothing, w)
  where
    upperHead t = maybe False (isUpper . fst) (T.uncons t)

{- | Hole fits that PRODUCE the goal type (result type equals it), nullary
bindings first so a "value of type T" repair prefers a ready-made value
over a producer still needing arguments (search-api.md section 7.1).
-}
goalTypedFits :: Text -> Text -> [Text]
goalTypedFits goalTy blob =
    map hfWrite . sortOn (argCount . hfType) $
        [f | f <- parseHoleFits blob, not (hfRefined f), producesGoal (hfType f)]
  where
    producesGoal ty = normType ty == g || ("-> " <> g) `T.isSuffixOf` normType ty
    g = normType goalTy

-- | Whitespace-normalise a type for structural comparison.
normType :: Text -> Text
normType = T.unwords . T.words

-- | Count top-level function arrows in a signature (its argument arity).
argCount :: Text -> Int
argCount ty = length (T.splitOn "->" (normType ty)) - 1

{- | Argument-order permutations of each top-level application on a line: the
head stays put, its argument tokens are reordered. Blind to scope and types
('acceptRepair' is the verifier), bounded so an arg list cannot explode.
-}
permuteApplications :: Text -> [Text]
permuteApplications src =
    nub [T.intercalate "\n" ls' | ls' <- oneLineVariants (T.lines src)]
  where
    oneLineVariants [] = []
    oneLineVariants (l : rest) =
        [v : rest | v <- lineVariants l, v /= l]
            ++ [l : vs | vs <- oneLineVariants rest]

-- | The argument-permuted spellings of one line's applied RHS.
lineVariants :: Text -> [Text]
lineVariants l = case T.breakOn "=" l of
    (lhs, rhs)
        | not (T.null rhs) ->
            [ lhs <> "= " <> body
            | body <- spineVariants (T.strip (T.drop 1 rhs))
            ]
    _ -> []

-- | Reorderings of an application spine @head a b …@ (head fixed), bounded.
spineVariants :: Text -> [Text]
spineVariants body = case T.words body of
    (h : args)
        | length args >= 2 && length args <= 4 ->
            [ T.unwords (h : p)
            | p <- take maxPermutations (permutations args)
            , p /= args
            ]
    _ -> []

-- | Cap on argument permutations tried per spine (context/latency guard).
maxPermutations :: Int
maxPermutations = 6

{- | Merge @pkg@ into the cell's @-- cabal: build-depends:@ line (mirrors
the server fixer's semantics): append to an existing line, else prepend one.
-}
addDepLine :: Text -> Text -> Text
addDepLine pkg src = case break ("build-depends:" `T.isInfixOf`) ls of
    (before, depLine : after)
        | pkg `elem` declared depLine -> src
        | otherwise -> T.unlines (before ++ [depLine <> ", " <> pkg] ++ after)
    _ -> T.unlines (("-- cabal: build-depends: " <> pkg) : ls)
  where
    ls = T.lines src
    declared line =
        map T.strip (T.splitOn "," (snd (T.breakOn "build-depends:" line)))

-- | Enable a LANGUAGE extension: a fresh pragma after any leading cabal line.
addPragmaLine :: Text -> Text -> Text
addPragmaLine ext src
    | ("{-# LANGUAGE" `T.isPrefixOf` T.stripStart src)
        && (ext `T.isInfixOf` src) =
        src
    | otherwise = case T.lines src of
        (l : rest)
            | "-- cabal:" `T.isPrefixOf` T.stripStart l ->
                T.unlines (l : pragma : rest)
        ls -> T.unlines (pragma : ls)
  where
    pragma = "{-# LANGUAGE " <> ext <> " #-}"

{- | Add @import M (n)@ after the cell's last import (else after the leading
cabal\/pragma header); a no-op when the module is already imported.
-}
addImportLine :: Text -> Text -> Text -> Text
addImportLine m n src
    | any ((("import " <> m) `T.isPrefixOf`) . T.stripStart) (T.lines src) =
        src
    | otherwise = insertImportLine ("import " <> m <> " (" <> n <> ")") src

{- | Add @import qualified M as A@ — the unimported-alias sub-class of the
not-in-scope diagnostic (@No module named A is imported@).
-}
addQualifiedImportLine :: Text -> Text -> Text -> Text
addQualifiedImportLine m alias src
    | any ((line `T.isPrefixOf`) . T.stripStart) (T.lines src) = src
    | otherwise = insertImportLine line src
  where
    line = "import qualified " <> m <> " as " <> alias

-- | Splice an import line after the last import (else after the header).
insertImportLine :: Text -> Text -> Text
insertImportLine line src = T.unlines (before ++ [line] ++ after)
  where
    ls = T.lines src
    lastImport =
        maximum (0 : [i | (i, l) <- zip [1 ..] ls, isImport l])
    isImport l = "import " `T.isPrefixOf` T.stripStart l
    headerLen = length (takeWhile isHeader ls)
    isHeader l =
        "-- cabal:" `T.isPrefixOf` T.stripStart l
            || "{-#" `T.isPrefixOf` T.stripStart l
    cut = max lastImport headerLen
    (before, after) = splitAt cut ls

dedupOn :: (Eq b) => (a -> b) -> [a] -> [a]
dedupOn f = go []
  where
    go _ [] = []
    go seen (x : xs)
        | f x `elem` seen = go seen xs
        | otherwise = x : go (f x : seen) xs
