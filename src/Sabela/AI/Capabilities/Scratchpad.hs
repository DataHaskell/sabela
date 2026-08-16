{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.AI.Capabilities.Scratchpad (
    execScratchpadGuarded,
    execScratchpad,
    evictScratchpad,
    ensureScratchpad,
    renderHaskellForGhci,
    scratchpadPayload,
    silentDiagnostic,
    isolationDiagnostic,
    scratchpadVerdict,
) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), object, (.=))
import Data.IORef (atomicModifyIORef')
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import qualified System.IO as IO
import System.IO.Temp (createTempDirectory)

import Sabela.AI.Capabilities.Scratchpad.Key (
    ScratchpadPlan (..),
    scratchpadPlan,
 )
import Sabela.AI.Capabilities.Scratchpad.Payload (
    annotateChurn,
    isolationDiagnostic,
    scratchpadPayload,
    scratchpadVerdict,
    silentDiagnostic,
    withVerdict,
 )
import Sabela.AI.Capabilities.Util (compactMaybeText, fieldText, parseCellLang)
import Sabela.AI.NormalizeGate (gatedRewrite)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.AI.Verdict (VerdictClass (..))
import Sabela.Api (errorJson)
import Sabela.Session.GhcProbe (resolvedGhcVersion)

import Sabela.Deps (collectMetadata, collectMetadataFromContent)
import Sabela.Diagnose (hiddenPackage, packageNeedsFlag)
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
                payload <-
                    scratchpadPayload
                        (compactMaybeText store . Just)
                        rawCode
                        stdout
                        stderr
                pure $
                    if T.null stderr
                        then okOutcome payload
                        else errOutcome payload

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

ensureScratchpad :: App -> AIStore -> CellLang -> [Text] -> IO SessionBackend
ensureScratchpad app store lang deps = do
    nb <- readNotebook (appNotebook app)
    mSp <- getScratchpad store
    ghcVersion <- resolvedGhcVersion
    case scratchpadPlan
        ghcVersion
        (envWorkDir (appEnv app))
        (envLocalPackages (appEnv app))
        lang
        (envGlobalDeps (appEnv app))
        nb
        deps
        mSp of
        ReuseScratchpad backend -> pure backend
        FreshScratchpad meta localPkgs key -> do
            mapM_ (sbClose . spBackend) mSp
            spDir <-
                createTempDirectory (envTmpDir (appEnv app)) "sabela-scratch"
            backend <- case lang of
                Haskell -> do
                    let projDir = envTmpDir (appEnv app) </> "scratch-project"
                    setupReplProject
                        WithNotebookSupport
                        localPkgs
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
            let sp = ScratchpadSession backend spDir lang key
            setScratchpad store (Just sp)
            pure backend
