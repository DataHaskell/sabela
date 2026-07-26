{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.GateRepair (
    gatedCandidate,
    repairCandidates,
    proofCap,
) where

import Data.List (nub)
import Data.Maybe (mapMaybe, isNothing)
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

repairCandidates :: Text -> Text -> [(Text, [Text])]
repairCandidates diagnostic src =
    take proofCap (nub (mapMaybe composite [0 .. proofCap - 1]))
  where
    hints = parseHints diagnostic
    renames = [(w, cs) | HintRename w cs <- hints, not (T.null w), not (null cs)]
    extensions = nub [e | HintExtension e <- hints]
    ambiguity = ambiguousOccurrence diagnostic
    hiddenPkg = hiddenPackage diagnostic
    varied = [i | (i, (_, cs)) <- zip [0 ..] renames, length cs > 1]
    composite k
        | null renames && null extensions && isNothing ambiguity && isNothing hiddenPkg =
            Nothing
        | otherwise =
            let picks =
                    [ (w, pick j k cs)
                    | (j, (w, cs)) <- zip [(0 :: Int) ..] renames
                    ]
                substituted =
                    foldl
                        (\acc (w, c) -> substituteNameInCode w (rcName c) acc)
                        src
                        picks
                qualified = case ambiguity of
                    Just (nm, cands@(_ : _)) ->
                        substituteNameInCode nm (cands !! min k (length cands - 1)) substituted
                    _ -> substituted
                withExts = foldl (flip addExtension) qualified extensions
                withDep = maybe withExts (`addBuildDepend` withExts) hiddenPkg
                fixes =
                    [ w <> " -> " <> rcName c <> provNote c
                    | (w, c) <- picks
                    , substituteNameInCode w (rcName c) src /= src
                    ]
                        <> ["qualified "
                              <> nm <> " as " <> (cands !! min k (length cands - 1)) |
                              qualified /= substituted, Just (nm, cands@(_ : _)) <- [ambiguity]]
                        <> ["enabled " <> e | e <- extensions]
                        <> ["declared build-depends: " <> p | Just p <- [hiddenPkg]]
             in if withDep == src || null fixes
                    then Nothing
                    else Just (withDep, fixes)
    pick j k cs = case varied of
        (v : _) | j == v -> cs !! min k (length cs - 1)
        _ -> head cs
    provNote c
        | T.null (rcProvenance c) = ""
        | otherwise = " (" <> rcProvenance c <> ")"

proofCap :: Int
proofCap = 3
