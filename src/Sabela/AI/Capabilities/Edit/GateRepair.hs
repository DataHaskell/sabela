{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.GateRepair (
    gatedCandidate,
    repairCandidates,
    aliasImportCandidates,
    importWidenCandidates,
    missingModuleCandidates,
    exactMatchOnly,
    proofCap,
) where

import Data.List (foldl', nub)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.CompileGate (
    compileGateSpec,
    gateHoleNudge,
    prevDefinedNames,
    rejectionJson,
 )
import Sabela.AI.Capabilities.Edit.HoleNudge (attachPairs)
import Sabela.AI.Capabilities.Edit.Repair.Mitigate (substituteNameInCode)
import Sabela.AI.Capabilities.ModuleCard (resolveInstalledModule)
import Sabela.AI.Capabilities.ModuleSearch (resolveNameToModules)
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.Capability (Capability (..))
import Sabela.AI.DepRepair (addBuildDepend)
import Sabela.AI.ExtRepair (addExtension)
import Sabela.AI.Health (scopeSubject)
import Sabela.AI.Hints (Hint (..), RenameCandidate (..), parseHints)
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
import Sabela.Model (CellType (..))
import Sabela.Parse (cellNames)
import Sabela.Session.Materialize (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    runDisposableTry,
 )
import Sabela.SessionTypes (CellLang (..))
import Sabela.State (App)

import Data.Aeson (Value)

gatedCandidate ::
    App ->
    Maybe Int ->
    CellLang ->
    CellType ->
    Text ->
    IO (Either Value (Text, [Text]))
gatedCandidate app mReplaces lang ty src
    | ty /= CodeCell || lang /= Haskell = pure (Right (src, []))
    | otherwise = do
        result <- runDisposableTry app (compileGateSpec mReplaces src)
        case disposableVerdict result of
            DisposableOk -> pure (Right (src, []))
            verdict -> do
                enabled <- featureEnabled "SABELA_GATE_REPAIR"
                let repairable = enabled && verdict == DisposableCompileError
                    diagnostic = diagnosticOf result
                    tries
                        | repairable = repairCandidates diagnostic src
                        | otherwise = []
                    widened
                        | repairable = importWidenCandidates diagnostic src
                        | otherwise = []
                aliases <-
                    if repairable
                        then aliasCandidates app diagnostic src
                        else pure []
                missing <-
                    if repairable
                        then missingModuleCandidates diagnostic src
                        else pure []
                prevDefined <- prevDefinedNames app mReplaces
                let mkRejection = do
                        nudge <- gateHoleNudge app mReplaces verdict diagnostic src
                        pure . attachPairs nudge $
                            rejectionJson mReplaces src prevDefined verdict result
                attempt
                    mkRejection
                    (take proofCap (nub (widened <> missing <> aliases <> tries)))
  where
    attempt mkRejection [] = Left <$> mkRejection
    attempt mkRejection ((candidate, fixes) : rest) = do
        result <- runDisposableTry app (compileGateSpec mReplaces candidate)
        case disposableVerdict result of
            DisposableOk -> pure (Right (candidate, [disclosure fixes]))
            _ -> attempt mkRejection rest
    disclosure fixes =
        "Applied GHC's suggested fix before committing: "
            <> T.intercalate "; " fixes
            <> "."

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
    defined = fst (cellNames src)
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

diagnosticOf :: DisposableResult -> Text
diagnosticOf result =
    maybe (disposableStderr result) failureMessage (disposableFailure result)

data Scope = Scope {scopeBody :: Bool, scopeHeader :: Bool}

scopes :: [Scope]
scopes = [Scope True True, Scope False True, Scope True False]

repairCandidates :: Text -> Text -> [(Text, [Text])]
repairCandidates diagnostic src =
    take proofCap (nub (mapMaybe candidate variations))
  where
    variations =
        [(p, s, k) | p <- [True, False], s <- scopes, k <- [0 .. proofCap - 1]]
    defined = fst (cellNames src)
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

    candidate (prune, scope, k)
        | null applied || repaired == src = Nothing
        | otherwise = Just (repaired, applied)
      where
        renames = renamesFor prune <> ambiguities
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
