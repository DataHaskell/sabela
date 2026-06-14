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
) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (atomicModifyIORef')
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Temp (createTempDirectory)

import Sabela.AI.Capabilities.Util (compactMaybeText, fieldText, parseCellLang)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Api (errorJson)

-- 'ToolOutcome' (and its smart constructors) is re-exported from
-- 'Sabela.AI.Types' via the open import above; no explicit list needed.
import Sabela.Deps (collectMetadataFromContent, mergedMeta)
import Sabela.Notebook.Support (materializeSupport, supportPackageDir)
import Sabela.Output (displayPrelude)
import Sabela.PythonSession (newPythonSession, pythonBackend)
import Sabela.Session (
    SessionConfig (..),
    readErrorBuffer,
    runBlock,
 )
import Sabela.Session.Process (
    closeSession,
    ghciBackend,
    newSession,
 )
import Sabela.Session.Project (setupReplProject)
import Sabela.SessionTypes (CellLang (..), SessionBackend (..))
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
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
    case (T.null rawCode, parseCellLang rawLang) of
        (True, _) -> pure (errOutcome (errorJson "code required"))
        (_, Nothing) ->
            pure
                ( errOutcome
                    ( errorJson
                        ( "Unknown language: "
                            <> rawLang
                            <> ". Expected Haskell or Python."
                        )
                    )
                )
        (False, Just lang) -> do
            -- For Haskell, run the snippet through the same scripths
            -- preprocessing that notebook cells get — this strips top-level
            -- `let`, auto-wraps multi-line defs in `:{ :}`, rewrites
            -- top-level TH splices, etc. Keeps scratchpad semantics parity
            -- with cell semantics so the model doesn't learn two rule-sets.
            let code = case lang of
                    Haskell -> renderHaskellForGhci rawCode
                    Python -> rawCode
            res <-
                try
                    ( do
                        backend <- ensureScratchpad app store lang
                        sbRunBlock backend code
                    ) ::
                    IO (Either SomeException (Text, Text))
            case res of
                Left e -> do
                    -- Drop the cached scratchpad so the next call rebuilds a
                    -- fresh GHCi instead of repeatedly hitting a dead
                    -- backend.
                    evictScratchpad store
                    pure (errOutcome (errorJson (T.pack (show e))))
                Right (stdout, stderr) -> do
                    let augErr = augmentGhciError stderr
                    stdoutV <- compactMaybeText store (Just stdout)
                    stderrV <- compactMaybeText store (Just augErr)
                    let payload =
                            object
                                [ "stdout" .= stdoutV
                                , "stderr" .= stderrV
                                ]
                    pure $
                        if T.null stderr
                            then okOutcome payload
                            else errOutcome payload

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

ensureScratchpad :: App -> AIStore -> CellLang -> IO SessionBackend
ensureScratchpad app store lang = do
    mSp <- getScratchpad store
    case mSp of
        Just sp | spLang sp == lang -> pure (spBackend sp)
        mOld -> do
            -- Kill existing if language changed
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
                    let workDir = envWorkDir (appEnv app)
                        projDir = envTmpDir (appEnv app) ++ "/scratch-project"
                        meta = mergedMeta (envGlobalDeps (appEnv app)) (collectMetadataFromContent "")
                    _ <- materializeSupport workDir
                    setupReplProject [supportPackageDir workDir] projDir meta
                    sess <- newSession (SessionConfig projDir spDir)
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
                    let venvDir = envTmpDir (appEnv app) ++ "/python-venv"
                    sess <- newPythonSession (Just venvDir) spDir
                    pure (pythonBackend sess)
            let sp = ScratchpadSession backend spDir lang
            setScratchpad store (Just sp)
            pure backend
