{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.AI.Capabilities.Scratchpad (
    execScratchpadGuarded,
    execScratchpad,
    evictScratchpad,
    ensureScratchpad,
    renderHaskellForGhci,
    augmentGhciError,
    silentDiagnostic,
    isolationDiagnostic,
    scratchpadVerdict,
) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (atomicModifyIORef')
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import qualified System.IO as IO
import System.IO.Temp (createTempDirectory)

import Sabela.AI.Capabilities.Util (compactMaybeText, fieldText, parseCellLang)
import Sabela.AI.NormalizeGate (gatedRewrite)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)
import Sabela.Api (errorJson)

import Sabela.Deps (collectMetadata, collectMetadataFromContent, mergedMeta)
import Sabela.Diagnose (
    diagnose,
    guidancePairs,
    hiddenPackage,
    packageNeedsFlag,
 )
import Sabela.Output (displayPrelude)
import Sabela.PythonSession (newPythonSession, pythonBackend)
import Sabela.Session (
    SessionConfig (..),
    mkSessionConfig,
    readErrorBuffer,
    runBlock,
 )
import Sabela.Session.Process (
    closeSession,
    ghciBackend,
    newSession,
 )
import Sabela.Session.Project (ReplSupport (..), setupReplProject)
import Sabela.SessionTypes (CellLang (..), SessionBackend (..))
import Sabela.State (App (..), readNotebook)
import Sabela.State.Environment (Environment (..))
import Sabela.ThrowawayExecute (
    ContainmentStatus (..),
    ExecuteQualification (..),
    ExecuteReason (..),
    ExecuteResult (..),
    ExecuteVerdict (..),
    SandboxStatus (..),
    admitExecute,
    executeFlagEnabled,
 )
import qualified ScriptHs.Parser as Scripths
import qualified ScriptHs.Render as Scripths

execScratchpadGuarded :: App -> AIStore -> Value -> IO ToolOutcome
execScratchpadGuarded app store input = do
    outcome <- execScratchpad app store input
    let isErr = toolOutcomeIsError outcome
        result = toolOutcomeValue outcome
    mTurn <- getCurrentTurn store
    case mTurn of
        Nothing -> pure outcome
        Just turn -> do
            fails <-
                if isErr
                    then
                        atomicModifyIORef'
                            (turnScratchpadFails turn)
                            (\n -> (n + 1, n + 1))
                    else do
                        _ <- atomicModifyIORef' (turnScratchpadFails turn) (const (0, ()))
                        pure 0
            if fails >= 3
                then
                    pure $
                        if isErr
                            then errOutcome (annotateChurn fails result)
                            else okOutcome (annotateChurn fails result)
                else pure outcome
  where
    annotateChurn n (Object o) =
        Object $
            KM.insert
                (Key.fromText "_sabelaHint")
                ( String $
                    "You have had "
                        <> T.pack (show n)
                        <> " consecutive failing scratchpad calls this turn. Common causes:"
                        <> " (a) top-level `let` (forbidden by scripths — write `x = 1` without `let`);"
                        <> " (b) ambiguous type defaults (pin with `:: Int` or `:: Double`);"
                        <> " (c) missing import. Before retrying, either ghci_query :type"
                        <> " the function to confirm its signature, or step back and explain to"
                        <> " the user what you're blocked on."
                )
                o
    annotateChurn n other =
        object
            [ "scratchpadResult" .= other
            , "_sabelaHint"
                .= ( "Churning: "
                        <> T.pack (show n)
                        <> " consecutive scratchpad errors. Change approach or ask the user." ::
                        Text
                   )
            ]

execScratchpad :: App -> AIStore -> Value -> IO ToolOutcome
execScratchpad app store input = do
    let rawCode = fieldText "code" input
        rawLang = fieldText "language" input
        mLang = if T.null rawLang then Just Haskell else parseCellLang rawLang
    case (T.null rawCode, mLang) of
        (True, _) ->
            pure (errOutcome (withVerdict VerdictCouldNotRun (errorJson "code required")))
        (_, Nothing) ->
            pure
                ( errOutcome
                    ( withVerdict VerdictCouldNotRun $
                        errorJson
                            ( "Unknown language: "
                                <> rawLang
                                <> ". Expected Haskell or Python."
                            )
                    )
                )
        (False, Just Haskell) -> do
            requested <- executeFlagEnabled <$> lookupEnv "SABELA_TYPECHECK_FORK_SCRATCH"
            if requested
                then unavailableThrowawayExecute
                else runLegacyScratchpad app store Haskell rawCode
        (False, Just lang) -> runLegacyScratchpad app store lang rawCode

runLegacyScratchpad :: App -> AIStore -> CellLang -> Text -> IO ToolOutcome
runLegacyScratchpad app store lang rawCode = do
    nb <- readNotebook (appNotebook app)
    let deps0 =
            S.toList . S.fromList $
                Scripths.metaDeps (collectMetadata nb)
                    ++ Scripths.metaDeps (collectMetadataFromContent rawCode)
    runScratchpadHealed app store lang rawCode deps0 scratchpadDepHealCap

unavailableThrowawayExecute :: IO ToolOutcome
unavailableThrowawayExecute = do
    started <- getMonotonicTimeNSec
    result <- admitExecute qualification (pure ())
    finished <- getMonotonicTimeNSec
    let latencyUs = (finished - started) `div` 1000
    IO.hPutStrLn IO.stderr $
        "sabela_throwaway_execute mode=execute verdict=unavailable latency_us="
            <> show latencyUs
            <> " sandbox=unqualified no_orphan_check=failed"
    pure . errOutcome $
        object
            [ "mode" .= ("execute" :: Text)
            , "verdict" .= executeVerdictText (executeVerdict result)
            , "reason" .= fmap executeReasonText (executeReason result)
            , "sandbox" .= ("macos-sandbox-exec-unqualified" :: Text)
            , "noOrphanCheck" .= ("failed-setsid-descendant-scenario" :: Text)
            , "latencyUs" .= latencyUs
            ]
  where
    qualification =
        ExecuteQualification
            { qualificationNotebookGhc = "unknown"
            , qualificationHelperGhc = "unknown"
            , qualificationSandbox = SandboxMacOS
            , qualificationContainment = ContainmentUnproven
            }
    executeVerdictText ExecuteOk = "ok" :: Text
    executeVerdictText ExecuteCompileError = "compile_error"
    executeVerdictText ExecuteTimedOut = "timed_out"
    executeVerdictText ExecuteUnavailable = "unavailable"
    executeReasonText HelperVersionMismatch = "helper_version_mismatch" :: Text
    executeReasonText ContainmentNotProven = "containment_not_proven"

scratchpadDepHealCap :: Int
scratchpadDepHealCap = 3

runScratchpadHealed ::
    App -> AIStore -> CellLang -> Text -> [Text] -> Int -> IO ToolOutcome
runScratchpadHealed app store lang rawCode deps budget = do
    let code = case lang of
            Haskell -> renderHaskellForGhci (fst (gatedRewrite rawCode))
            Python -> rawCode
    res <-
        try
            ( do
                backend <- ensureScratchpad app store lang deps
                sbRunBlock backend code
            ) ::
            IO (Either SomeException (Text, Text))
    case res of
        Left e -> do
            evictScratchpad store
            pure
                ( errOutcome
                    (withVerdict VerdictCouldNotRun (errorJson (T.pack (show e))))
                )
        Right (stdout, stderr)
            | lang == Haskell
            , budget > 0
            , Just pkg <- hiddenPackage stderr <|> packageNeedsFlag stderr
            , pkg `notElem` deps ->
                runScratchpadHealed
                    app
                    store
                    lang
                    rawCode
                    (S.toList (S.fromList (pkg : deps)))
                    (budget - 1)
            | otherwise -> do
                let augErr = augmentGhciError stderr
                stdoutV <- compactMaybeText store (Just stdout)
                stderrV <- compactMaybeText store (Just augErr)
                let guidance = guidancePairs (diagnose stderr)
                    diagPair =
                        [ "diagnostic" .= d
                        | Just d <- [silentDiagnostic stdout stderr guidance]
                        ]
                    payload =
                        object
                            ( [ "verdict" .= verdictTag (scratchpadVerdict stdout stderr)
                              , "stdout" .= stdoutV
                              , "stderr" .= stderrV
                              ]
                                <> guidance
                                <> diagPair
                            )
                pure $
                    if T.null stderr
                        then okOutcome payload
                        else errOutcome payload

scratchpadVerdict :: Text -> Text -> VerdictClass
scratchpadVerdict stdout stderr
    | not (T.null (T.strip stderr)) = VerdictDiagnostic
    | not (T.null (T.strip stdout)) = VerdictOk
    | otherwise = VerdictCouldNotRun

withVerdict :: VerdictClass -> Value -> Value
withVerdict c (Object o) =
    Object (KM.insert (Key.fromText "verdict") (String (verdictTag c)) o)
withVerdict c v = object ["verdict" .= verdictTag c, "result" .= v]

silentDiagnostic :: Text -> Text -> [a] -> Maybe Text
silentDiagnostic stdout stderr guidance
    | T.null (T.strip stdout) && T.null (T.strip stderr) && null guidance =
        Just isolationDiagnostic
    | otherwise = Nothing

isolationDiagnostic :: Text
isolationDiagnostic =
    "No output and no error. The scratchpad is ISOLATED from the notebook \
    \session: it cannot see notebook bindings, and packages need their own \
    \`-- cabal:` line inside the snippet. A pure binding prints nothing — \
    \`print` it here, or probe live notebook state with check_type / \
    \list_bindings, or just insert a cell instead."

evictScratchpad :: AIStore -> IO ()
evictScratchpad store = do
    mSp <- getScratchpad store
    case mSp of
        Just sp -> do
            _ <-
                try (sbClose (spBackend sp)) ::
                    IO (Either SomeException ())
            setScratchpad store Nothing
        Nothing -> pure ()

renderHaskellForGhci :: Text -> Text
renderHaskellForGhci src =
    let sf = Scripths.scriptLines (Scripths.parseScript src)
     in Scripths.toGhciScript sf

augmentGhciError :: Text -> Text
augmentGhciError err
    | T.null err = err
    | mentionsLetParseError err =
        err
            <> "\n[sabela hint] GHCi rejected a top-level `let` binding. Scripths strips the"
            <> " `let` automatically in notebook cells; in scratchpad, write `x = 1`"
            <> " directly (no `let`). `let ... in ...` expressions and `let` inside do/where"
            <> " blocks remain fine."
    | otherwise = err
  where
    mentionsLetParseError t =
        ("parse error" `T.isInfixOf` t)
            && ("let" `T.isInfixOf` t || "=" `T.isInfixOf` t && "on input" `T.isInfixOf` t)

ensureScratchpad :: App -> AIStore -> CellLang -> [Text] -> IO SessionBackend
ensureScratchpad app store lang deps = do
    mSp <- getScratchpad store
    case mSp of
        Just sp | spLang sp == lang, spDeps sp == deps -> pure (spBackend sp)
        mOld -> do
            case mOld of
                Just sp -> sbClose (spBackend sp)
                Nothing -> pure ()
            spDir <-
                createTempDirectory (envTmpDir (appEnv app)) "sabela-scratch"
            backend <- case lang of
                Haskell -> do
                    nb <- readNotebook (appNotebook app)
                    let projDir = envTmpDir (appEnv app) </> "scratch-project"
                        meta =
                            mergedMeta
                                (envGlobalDeps (appEnv app) <> S.fromList deps)
                                (collectMetadata nb)
                    setupReplProject
                        WithNotebookSupport
                        (envLocalPackages (appEnv app))
                        projDir
                        meta
                    cfg <- (\c -> c{scJsonDiagnostics = False}) <$> mkSessionConfig projDir spDir
                    sess <- newSession cfg
                    initRes <-
                        try (runBlock sess displayPrelude) ::
                            IO (Either SomeException (Text, Text))
                    case initRes of
                        Left e -> do
                            errBuf <- readErrorBuffer sess
                            _ <-
                                try (closeSession sess) ::
                                    IO (Either SomeException ())
                            ioError $
                                userError $
                                    "Scratchpad startup failed: "
                                        ++ show e
                                        ++ ( if T.null errBuf
                                                then " (no stderr captured)"
                                                else
                                                    "\n--- captured stderr ---\n"
                                                        ++ T.unpack errBuf
                                           )
                        Right _ -> pure (ghciBackend sess)
                Python -> do
                    let venvDir = envTmpDir (appEnv app) </> "python-venv"
                    sess <- newPythonSession (Just venvDir) spDir
                    pure (pythonBackend sess)
            let sp = ScratchpadSession backend spDir lang deps
            setScratchpad store (Just sp)
            pure backend
