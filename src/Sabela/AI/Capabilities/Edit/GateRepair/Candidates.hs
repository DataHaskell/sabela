{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.GateRepair.Candidates (
    gateCandidates,
    roundRobin,
    costOrdered,
    repairCandidates,
    aliasImportCandidates,
    importWidenCandidates,
    missingModuleCandidates,
    exactMatchOnly,
    proofCap,
) where

import Data.List (nub, partition)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.GateRepair.Binders (boundNames)
import Sabela.AI.Capabilities.Edit.Repair.Mitigate (substituteNameInCode)
import Sabela.AI.Capabilities.ModuleCard (resolveInstalledModule)
import Sabela.AI.Capabilities.ModuleSearch (resolveNameToModules)
import Sabela.AI.Capability (Capability (..))
import Sabela.AI.DepRepair (addBuildDepend, newDependencies)
import Sabela.AI.ExtRepair (addExtension)
import Sabela.AI.Health (scopeSubject)
import Sabela.AI.Hints (Hint (..), RenameCandidate (..), parseHints, sameNameClass)
import Sabela.AI.ImportRepair (
    addQualifiedImport,
    dropImportList,
    importedAliasMisses,
    unboundAliasUses,
    widenImportList,
 )
import Sabela.AI.PackageIndex (PackageEntry (..))
import Sabela.Diagnose (
    ambiguousOccurrences,
    couldNotFindModules,
    hiddenPackages,
 )
import Sabela.State (App)

{- | Every repair the diagnostic licenses, taken one class at a time so a
prolific class cannot spend the probe budget the others need, then cheapest
tier first so a build never runs before a typecheck.
-}
gateCandidates :: App -> Text -> Text -> IO [(Text, [Text])]
gateCandidates app diagnostic src = do
    aliases <- aliasCandidates app diagnostic src
    missing <- missingModuleCandidates diagnostic src
    pure . nub . costOrdered src $
        roundRobin
            [ importWidenCandidates diagnostic src
            , missing
            , aliases
            , repairCandidates diagnostic src
            ]

roundRobin :: [[a]] -> [a]
roundRobin xss = case [(x, rest) | (x : rest) <- xss] of
    [] -> []
    ys -> map fst ys <> roundRobin (map snd ys)

{- | Sinks the candidates that declare a new dependency below the rest: proving
one costs a package build (measured at 52.8s) against 6.7s for a typecheck.
Stable, so the round-robin order survives inside each tier.
-}
costOrdered :: Text -> [(Text, [Text])] -> [(Text, [Text])]
costOrdered src candidates = cheap <> costly
  where
    (costly, cheap) = partition needsBuild candidates
    needsBuild (candidate, _) = not (null (newDependencies src candidate))

aliasCandidates :: App -> Text -> Text -> IO [(Text, [Text])]
aliasCandidates app diagnostic src =
    concat
        <$> mapM forPair (importedAliasMisses diagnostic <> unboundAliasUses diagnostic)
  where
    forPair (alias, name) = do
        caps <- resolveNameToModules app name
        pure (aliasImportCandidates alias (take 2 (map capModule caps)) src)

missingModuleCandidates :: Text -> Text -> IO [(Text, [Text])]
missingModuleCandidates diagnostic src = do
    pkgs <- nub . concat <$> mapM verifiedPackage (couldNotFindModules diagnostic)
    let repaired = foldl' (flip addBuildDepend) src pkgs
        fixes = ["declared build-depends: " <> p | p <- pkgs]
    pure [(repaired, fixes) | repaired /= src, not (null fixes)]
  where
    verifiedPackage modName =
        maybe [] ((: []) . peName) . exactMatchOnly modName
            <$> resolveInstalledModule modName

-- | Rejects a near-spelling fallback: only the asked-for name, verbatim.
exactMatchOnly :: Text -> Maybe (Text, PackageEntry) -> Maybe PackageEntry
exactMatchOnly modName resolved = do
    (near, pkg) <- resolved
    if near == modName then Just pkg else Nothing

{- | Rejection-sampled import repairs: for every module the cell imports
selectively, widen the list with each missing name, then try the module
wholesale; the re-gate probe discards any guess the module cannot honour.
-}
importWidenCandidates :: Text -> Text -> [(Text, [Text])]
importWidenCandidates diagnostic src
    | null names = []
    | otherwise =
        concat
            [ [ (widened, ["added " <> commaNames <> " to import " <> m])
              | let widened = widenImportList m names src
              , widened /= src
              ]
                ++ [ (wholesale, ["imported " <> m <> " without an import list"])
                   | let wholesale = dropImportList m src
                   , wholesale /= src
                   ]
            | m <- selectiveImportModules src
            ]
  where
    commaNames = T.intercalate ", " names
    defined = boundNames src
    names =
        nub
            [ n
            | chunk <- T.splitOn "\n\n" diagnostic
            , Just n <- [scopeSubject chunk]
            , not ("." `T.isInfixOf` n)
            , not (n `Set.member` defined)
            ]

selectiveImportModules :: Text -> [Text]
selectiveImportModules src =
    nub
        [ T.takeWhile (/= ' ') rest
        | l <- map T.stripStart (T.lines src)
        , Just rest <- [T.stripPrefix "import " l]
        , not ("qualified " `T.isPrefixOf` rest)
        , " (" `T.isInfixOf` rest
        , not (" hiding " `T.isInfixOf` rest)
        ]

aliasImportCandidates :: Text -> [Text] -> Text -> [(Text, [Text])]
aliasImportCandidates alias modules src =
    [ (src', ["imported " <> m <> " as " <> alias])
    | m <- modules
    , let src' = addQualifiedImport m alias src
    , src' /= src
    ]

data Scope = Scope {scopeBody :: Bool, scopeHeader :: Bool}

scopes :: [Scope]
scopes = [Scope True True, Scope False True, Scope True False]

repairCandidates :: Text -> Text -> [(Text, [Text])]
repairCandidates diagnostic src =
    take proofCap (nub (mapMaybe candidate variations))
  where
    variations =
        [(p, s, k) | p <- [True, False], s <- scopes, k <- [0 .. proofCap - 1]]
    defined = boundNames src
    hints = parseHints diagnostic
    allRenames = [(w, cs) | HintRename w cs <- hints, not (T.null w), not (null cs)]
    extensions = nub [e | HintExtension e <- hints]
    ambiguities =
        [ (nm, [RenameCandidate c "" "" | c <- cands])
        | (nm, cands) <- ambiguousOccurrences diagnostic
        ]
    hiddenPkgs = hiddenPackages diagnostic

    renamesFor prune
        | prune = filter (\(w, _) -> not (w `Set.member` defined)) allRenames
        | otherwise = allRenames

    {- | Rewriting a name the cell binds rewrites its binder too, so the
    replacement must be able to stand there: neither @Median = ...@ nor
    @DF.median = ...@ parses. A name the cell only uses is unconstrained.
    -}
    bindable w r
        | w `Set.member` defined = sameNameClass w r && not ("." `T.isInfixOf` r)
        | otherwise = True

    wellFormedRenames rs =
        [ (w, kept)
        | (w, cs) <- rs
        , let kept = filter (bindable w . rcName) cs
        , not (null kept)
        ]

    candidate (prune, scope, k)
        | null applied || repaired == src = Nothing
        | otherwise = Just (repaired, applied)
      where
        renames = wellFormedRenames (renamesFor prune <> ambiguities)
        varied = [i | (i, (_, cs)) <- zip [(0 :: Int) ..] renames, length cs > 1]
        pick j cs = case varied of
            (v : _) | j == v -> choose k cs
            _ -> choose 0 cs
        picks
            | scopeBody scope =
                [(w, pick j cs) | (j, (w, cs)) <- zip [(0 :: Int) ..] renames]
            | otherwise = []
        renamed =
            foldl' (\acc (w, c) -> substituteNameInCode w (rcName c) acc) src picks
        exts = if scopeHeader scope then extensions else []
        deps = if scopeHeader scope then hiddenPkgs else []
        repaired =
            foldl' (flip addBuildDepend) (foldl' (flip addExtension) renamed exts) deps
        applied =
            [ w <> " -> " <> rcName c <> provNote c
            | (w, c) <- picks
            , substituteNameInCode w (rcName c) src /= src
            ]
                <> map ("enabled " <>) exts
                <> map ("declared build-depends: " <>) deps

    choose k cs = cs !! min k (length cs - 1)
    provNote c
        | T.null (rcProvenance c) = ""
        | otherwise = " (" <> rcProvenance c <> ")"

proofCap :: Int
proofCap = 3
