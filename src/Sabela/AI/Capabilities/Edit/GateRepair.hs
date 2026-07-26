{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.GateRepair (
    gatedCandidate,
    repairCandidates,
    proofCap,
) where

import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.CompileGate (compileGateSpec, rejectionJson)
import Sabela.AI.Capabilities.Edit.Repair.Mitigate (substituteNameInCode)
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.DepRepair (addBuildDepend)
import Sabela.AI.ExtRepair (addExtension)
import Sabela.AI.Hints (Hint (..), RenameCandidate (..), parseHints)
import Sabela.Diagnose (ambiguousOccurrence, hiddenPackage)
import Sabela.Model (CellType (..))
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
                let tries
                        | enabled && verdict == DisposableCompileError =
                            repairCandidates (diagnosticOf result) src
                        | otherwise = []
                attempt (rejectionJson mReplaces src verdict result) tries
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
    variations = [(s, k) | s <- scopes, k <- [0 .. proofCap - 1]]
    hints = parseHints diagnostic
    renames = [(w, cs) | HintRename w cs <- hints, not (T.null w), not (null cs)]
    extensions = nub [e | HintExtension e <- hints]
    ambiguity = ambiguousOccurrence diagnostic
    hiddenPkg = hiddenPackage diagnostic
    varied = [i | (i, (_, cs)) <- zip [(0 :: Int) ..] renames, length cs > 1]

    candidate (scope, k)
        | null applied || repaired == src = Nothing
        | otherwise = Just (repaired, applied)
      where
        picks
            | scopeBody scope =
                [(w, pick j k cs) | (j, (w, cs)) <- zip [(0 :: Int) ..] renames]
            | otherwise = []
        renamed =
            foldl' (\acc (w, c) -> substituteNameInCode w (rcName c) acc) src picks
        toQualify = if scopeBody scope then ambiguity else Nothing
        qualified = case toQualify of
            Just (nm, cands@(_ : _)) ->
                substituteNameInCode nm (choose k cands) renamed
            _ -> renamed
        exts = if scopeHeader scope then extensions else []
        dep = if scopeHeader scope then hiddenPkg else Nothing
        repaired =
            maybe id addBuildDepend dep (foldl' (flip addExtension) qualified exts)
        applied =
            [ w <> " -> " <> rcName c <> provNote c
            | (w, c) <- picks
            , substituteNameInCode w (rcName c) src /= src
            ]
                <> ["qualified " <> nm <> " as " <> choose k cands |
                      qualified /= renamed, Just (nm, cands@(_ : _)) <- [toQualify]]
                <> map ("enabled " <>) exts
                <> map ("declared build-depends: " <>) (maybe [] pure dep)

    choose k cs = cs !! min k (length cs - 1)
    pick j k cs = case varied of
        (v : _) | j == v -> choose k cs
        _ -> choose 0 cs
    provNote c
        | T.null (rcProvenance c) = ""
        | otherwise = " (" <> rcProvenance c <> ")"

proofCap :: Int
proofCap = 3
