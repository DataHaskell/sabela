{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.GateRepair (
    gatedCandidate,
    repairCandidates,
    aliasImportCandidates,
    proofCap,
) where

import Data.List (foldl', nub)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.CompileGate (compileGateSpec, rejectionJson)
import Sabela.AI.Capabilities.Edit.Repair.Mitigate (substituteNameInCode)
import Sabela.AI.Capabilities.ModuleSearch (resolveNameToModules)
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.Capability (Capability (..))
import Sabela.AI.DepRepair (addBuildDepend)
import Sabela.AI.ExtRepair (addExtension)
import Sabela.AI.Hints (Hint (..), RenameCandidate (..), parseHints)
import Sabela.AI.ImportRepair (
    addQualifiedImport,
    importedAliasMisses,
    unboundAliasUses,
 )
import Sabela.Diagnose (ambiguousOccurrences, hiddenPackages)
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
                aliases <-
                    if repairable
                        then aliasCandidates app diagnostic src
                        else pure []
                attempt
                    (rejectionJson mReplaces src verdict result)
                    (take proofCap (nub (aliases <> tries)))
  where
    attempt rejection [] = pure (Left rejection)
    attempt rejection ((candidate, fixes) : rest) = do
        result <- runDisposableTry app (compileGateSpec mReplaces candidate)
        case disposableVerdict result of
            DisposableOk -> pure (Right (candidate, [disclosure fixes]))
            _ -> attempt rejection rest
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
