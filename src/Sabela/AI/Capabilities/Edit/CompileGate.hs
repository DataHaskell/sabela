{-# LANGUAGE OverloadedStrings #-}

{- | The prime-invariant gate (G1): before an AI mutation (@insert_cell@,
@replace_cell_source@, or an accepted @propose_edit@) commits, the candidate
is compiled — typecheck-only, no evaluation — against the current notebook
prefix in a disposable reconstruction ("Sabela.Session.Materialize", A2's
cache-backed route). Green proceeds to the existing checked commit
unchanged; not green rejects the write outright: no cell, no generation
bump, no session mutation. A dep-adding cell (@-- cabal:@ anywhere in its
source) is gate-compiled in that same disposable, cache-backed environment
BEFORE any live install/restart is ever paid — the specific gap this closes.
-}
module Sabela.AI.Capabilities.Edit.CompileGate (
    compileGateCheck,
    compileGateSpec,
    rejectionJson,
) where

import Data.Aeson (Value, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.CompileGate.Render (renderNonExecuting)
import Sabela.AI.HoleFits (holeFitsJson)
import Sabela.AI.HoleProbe (holeProbeJson)
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)
import Sabela.AI.WriteAck (refusalAck)
import Sabela.Api (errorJsonWith)
import Sabela.Model (CellType (..))
import Sabela.Session.Materialize (
    CandidateSpec (..),
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    materializeStageText,
    runDisposableTry,
 )
import Sabela.SessionTypes (CellLang (..))
import Sabela.State (App)

{- | Gate-compile a Haskell code-cell candidate before it may commit.
@mReplaces@ names the cell a replace supersedes, so the disposable prefix
drops its stale copy; 'Nothing' for an insert (append). Non-Haskell or
non-code cells pass straight through — only Haskell code can fail to
compile, and the human/prose paths this never touches stay untouched.
-}
compileGateCheck ::
    App -> Maybe Int -> CellLang -> CellType -> Text -> IO (Either Value ())
compileGateCheck app mReplaces lang ty src
    | ty /= CodeCell || lang /= Haskell = pure (Right ())
    | otherwise = do
        result <- runDisposableTry app (compileGateSpec mReplaces src)
        pure $ case disposableVerdict result of
            DisposableOk -> Right ()
            verdict -> Left (rejectionJson mReplaces src verdict result)

{- | The declarations-only trial: every piece of the candidate — including
an action or monadic bind that would otherwise run — rebound so GHC
type-checks it without ever forcing it. See 'renderNonExecuting'. Shared
with G3's hole probe, which asks the compiler the same question with the
same guarantee that nothing is evaluated.
-}
compileGateSpec :: Maybe Int -> Text -> CandidateSpec
compileGateSpec mReplaces src =
    CandidateSpec
        { candidateMetadataSource = src
        , candidateSetup = renderNonExecuting src
        , candidateExpression = Nothing
        , candidateReplacesCellId = mReplaces
        , -- A gate compile is always a deliberate commit, never a trial: the
          -- model is asking to LAND this cell. Budgeting it as a speculative
          -- trial is what deadlocked live_test21 — the gate refused a
          -- dependency cell and then advised committing that same cell.
          candidateDeliberate = True
        }

{- | The rejection envelope: a 'refusalAck' naming the closed verdict, the
materialize stage, the full compiler diagnostic (a plain typecheck already
reports hole fits, parsed out alongside it when present — see
"Sabela.AI.HoleFits"), and the rejected source verbatim. A compile failure
is 'VerdictDiagnostic' (a real, negative answer); a gate-infrastructure
failure (env build timeout, scratch kernel wedge) is 'VerdictInfra' — fail
closed, telling the caller to retry or state the blocker, never
"could not verify, committing anyway".
-}
rejectionJson ::
    Maybe Int -> Text -> DisposableVerdict -> DisposableResult -> Value
rejectionJson mReplaces src verdict result =
    refusalAck "compile-gate" mReplaces $
        errorJsonWith
            message
            ( [ "verdict" .= verdictTag verdictClass
              , "stage" .= maybe "unknown" (materializeStageText . failureStage) failure
              , "diagnostic" .= diagnostic
              , "source" .= src
              ]
                <> holeFitPairs
                <> holeProbePairs
            )
  where
    failure = disposableFailure result
    diagnostic =
        let raw = maybe (disposableStderr result) failureMessage failure
         in if T.null (T.strip raw) then infraFallback else raw
    infraFallback =
        "The compile gate could not verify this write; nothing was committed."
    verdictClass
        | verdict `elem` [DisposableTimedOut, DisposableUnavailable] = VerdictInfra
        | otherwise = VerdictDiagnostic
    message = case verdictClass of
        VerdictInfra ->
            "Could not verify this write (compile gate infrastructure failed): "
                <> diagnostic
                <> " Nothing was committed; retry, or state the blocker."
        _ -> "This candidate does not compile, so nothing was committed: " <> diagnostic
    holeFitPairs =
        case holeFitsJson holeFitCap diagnostic of
            [] -> []
            fits -> ["holeFits" .= fits]
    -- G3 task 5: a rejected hole-bearing write still gets its answer — the
    -- fits harvested by this very gate check, stated as conclusions.
    holeProbePairs = maybe [] (\v -> ["holeProbe" .= v]) (holeProbeJson diagnostic)

{- | How many fits a rejection ships. Past the first handful they are noise
the model pays context for; the cap is shared with the probe route so both
answers are the same size.
-}
holeFitCap :: Int
holeFitCap = 8
