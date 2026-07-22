{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Scratchpad tool: an isolated GHCi (or Python) session the AI can
run snippets in without touching the notebook. 'execScratchpadGuarded'
wraps the raw 'execScratchpad' with a per-turn churn breaker that nudges
the model after several consecutive failures.
-}
module Sabela.AI.Capabilities.Scratchpad (
    execScratchpadGuarded,
    execScratchpad,
    evictScratchpad,
    ensureScratchpad,

    -- * Pieces (exposed for testing)
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
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (createTempDirectory)

import Sabela.AI.Capabilities.Util (compactMaybeText, fieldText, parseCellLang)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)
import Sabela.Api (errorJson)
import Sabela.Parse.Normalize (rewriteTopLevelLet)

-- 'ToolOutcome' (and its smart constructors) is re-exported from
-- 'Sabela.AI.Types' via the open import above; no explicit list needed.
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

{- | Wrap 'execScratchpad' with a circuit breaker. If the model has had
several consecutive failing scratchpad attempts in the same turn, append a
nudge to the tool response telling it to change approach instead of
silently letting the loop churn through a rate-limit window.

The breaker is per-turn and stored on the 'AIStore' via the current turn's
'turnScratchpadFails' counter. It doesn't actually block the tool — it just
annotates the response, which is enough for the model to notice.
-}
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
    -- Seed the scratchpad with the notebook's declared deps plus the
    -- snippet's own, so an already-declared package is available.
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
    hPutStrLn stderr $
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

-- | How many missing deps the scratchpad will infer-and-install per call.
scratchpadDepHealCap :: Int
scratchpadDepHealCap = 3

{- | Run a scratchpad snippet, and if it fails on a package GHC names as hidden
or needing a flag, add that dep and rebuild — self-installing missing deps the
way a cell does, so the model isn't stuck retrying the same import.
-}
runScratchpadHealed ::
    App -> AIStore -> CellLang -> Text -> [Text] -> Int -> IO ToolOutcome
runScratchpadHealed app store lang rawCode deps budget = do
    -- Run the snippet through the same scripths preprocessing cells get (strips
    -- top-level `let`, wraps multi-line defs, etc.) so the two agree.
    let code = case lang of
            Haskell -> renderHaskellForGhci (rewriteTopLevelLet rawCode)
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

{- | Section 5.3: the schema-required verdict of a scratchpad run, keyed on the
observable outcome class alone — stderr = diagnostic, output = ok, silence =
could-not-run. Silence can never render as a pass.
-}
scratchpadVerdict :: Text -> Text -> VerdictClass
scratchpadVerdict stdout stderr
    | not (T.null (T.strip stderr)) = VerdictDiagnostic
    | not (T.null (T.strip stdout)) = VerdictOk
    | otherwise = VerdictCouldNotRun

-- | Prepend the section-5.3 verdict field to a verifier payload.
withVerdict :: VerdictClass -> Value -> Value
withVerdict c (Object o) =
    Object (KM.insert (Key.fromText "verdict") (String (verdictTag c)) o)
withVerdict c v = object ["verdict" .= verdictTag c, "result" .= v]

{- | R6.7: the scratchpad never answers with silence. When a snippet yields no
output, no error and no guidance, a typed diagnostic states the scratchpad's
isolation and the in-session alternative; any real output suppresses it.
-}
silentDiagnostic :: Text -> Text -> [a] -> Maybe Text
silentDiagnostic stdout stderr guidance
    | T.null (T.strip stdout) && T.null (T.strip stderr) && null guidance =
        Just isolationDiagnostic
    | otherwise = Nothing

-- | What the scratchpad cannot do, and what to use instead.
isolationDiagnostic :: Text
isolationDiagnostic =
    "No output and no error. The scratchpad is ISOLATED from the notebook \
    \session: it cannot see notebook bindings, and packages need their own \
    \`-- cabal:` line inside the snippet. A pure binding prints nothing — \
    \`print` it here, or probe live notebook state with check_type / \
    \list_bindings, or just insert a cell instead."

{- | Tear down the cached scratchpad backend (if any) and clear the slot.
Used after a failure so the next scratchpad call starts a fresh session.
-}
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

{- | Apply scripths's GHCi rendering to an ad-hoc Haskell snippet so scratchpad
is parity with notebook cells.
-}
renderHaskellForGhci :: Text -> Text
renderHaskellForGhci src =
    let sf = Scripths.scriptLines (Scripths.parseScript src)
     in Scripths.toGhciScript sf

{- | Tack a short hint onto GHCi stderr when it contains the classic top-level
`let` parse error, so models don't hallucinate that "session state is
corrupted". Zero-cost on benign stderr.
-}
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
            -- Kill an existing scratchpad when the language OR the declared deps
            -- changed, so a self-healed dep triggers a rebuild.
            case mOld of
                Just sp -> sbClose (spBackend sp)
                Nothing -> pure ()
            spDir <-
                createTempDirectory (envTmpDir (appEnv app)) "sabela-scratch"
            backend <- case lang of
                Haskell -> do
                    -- The scratchpad gets its OWN scaffolded project (separate
                    -- dist-newstyle), so it works on a cold notebook (before any
                    -- cell has materialised repl-project) and never contends with
                    -- the live notebook session over the shared build dir.
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
                    -- The scratchpad reports raw GHCi stderr to the model and
                    -- runs textual diagnose/guidance over it, so keep it on the
                    -- textual diagnostic path (no -fdiagnostics-as-json).
                    cfg <- (\c -> c{scJsonDiagnostics = False}) <$> mkSessionConfig projDir spDir
                    sess <- newSession cfg
                    -- If GHCi/cabal dies during prelude injection, surface
                    -- the captured cabal/ghci stderr instead of the opaque
                    -- "GHCi process exited with ExitFailure 1" so callers can
                    -- see the real cause.
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
