{-# LANGUAGE OverloadedStrings #-}

{- | Evaluating an expression in the live session without letting it change
that session: admit it only if its type is not IO, run it under a shared
deadline, then prove the bindings and @it@ came out as they went in.
-}
module Sabela.Session.Query.PureEval (
    evalPureLive,
) where

import Control.Exception (SomeException, finally, mask, try)
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Session (
    Session (..),
    checkProcessAlive,
    readSessionGen,
    withQueryLocks,
 )
import Sabela.Session.Proc (destroySession)
import Sabela.Session.Query.Bindings (itFingerprint, scrubBindingShapes)
import Sabela.Session.Query.Command (queryTimeoutUs)
import Sabela.Session.Query.PureEval.Deadline (
    EvalDeadline,
    commandTimedOut,
    deadlineCommand,
    deadlineMessage,
    fingerprintCommand,
    newEvalDeadline,
    timeoutRecovery,
 )
import Sabela.Session.Query.PureEval.Protocol (
    admissionCommand,
    diagnostic,
    evalCommand,
    framed,
 )
import Sabela.Output (
    pureAdmittedMarker,
    pureErrorMarker,
    pureIOMarker,
    pureValueMarker,
 )
import Sabela.SessionTypes (
    PureEvalRecovery (..),
    PureEvalRequest (..),
    PureEvalResult (..),
    PureEvalVerdict (..),
 )

evalPureLive :: Session -> PureEvalRequest -> IO PureEvalResult
evalPureLive sess req
    | T.null expr = pure (rejected "expression required")
    | T.any (`elem` ['\n', '\r']) expr =
        pure (rejected "pure live evaluation accepts one protocol line")
    | otherwise =
        withQueryLocks sess $
            mask $ \restore -> do
                savedErr <- readIORef (sessErrBuf sess)
                attempted <-
                    try (restore (lockedEval sess req))
                        `finally` writeIORef (sessErrBuf sess) savedErr
                case attempted of
                    Left e -> do
                        destroySession (sessProcSess sess)
                        pure $
                            (baseResult PureEvalUnavailable (pureEvalExpectedGeneration req))
                                { pureEvalError = T.pack (show (e :: SomeException))
                                , pureEvalRecovery = PureEvalKernelDestroyed
                                }
                    Right result -> pure result
  where
    expr = T.strip (pureEvalExpression req)
    rejected msg =
        (baseResult PureEvalRejected (pureEvalExpectedGeneration req))
            { pureEvalError = msg
            }

lockedEval :: Session -> PureEvalRequest -> IO PureEvalResult
lockedEval sess req = do
    deadline <- newEvalDeadline (pureEvalTimeoutUs req)
    checkProcessAlive sess
    gen <- readSessionGen sess
    if gen /= pureEvalExpectedGeneration req
        then
            pure $
                (baseResult PureEvalStale gen)
                    { pureEvalError = "live generation changed before evaluation"
                    }
        else startCandidate deadline sess gen
  where
    expression = T.strip (pureEvalExpression req)

    startCandidate deadline live gen = do
        (beforeRaw, beforeErr) <-
            deadlineCommand deadline queryTimeoutUs live ":show bindings"
        if commandTimedOut beforeErr
            then pure (timedOutResult gen beforeErr)
            else
                if not (T.null (T.strip beforeErr))
                    then
                        pure $
                            (baseResult PureEvalInvariantFailed gen)
                                { pureEvalError =
                                    "could not fingerprint live bindings\n" <> beforeErr
                                }
                    else classifyCandidate deadline live gen beforeRaw

    classifyCandidate deadline live gen beforeRaw = do
        (admitOut, admitErr) <-
            deadlineCommand deadline queryTimeoutUs live (admissionCommand expression)
        if commandTimedOut admitErr
            then
                finishFingerprint
                    deadline
                    live
                    gen
                    beforeRaw
                    ""
                    ""
                    admitErr
                    PureEvalTimedOut
                    (timeoutRecovery admitErr)
            else case (framed pureIOMarker admitOut, framed pureAdmittedMarker admitOut) of
                (Just inferred, _) ->
                    finishFingerprint
                        deadline
                        live
                        gen
                        beforeRaw
                        inferred
                        ""
                        ( "scratch candidate is IO; it was not executed"
                            <> diagnostic "" admitErr
                        )
                        PureEvalRejected
                        PureEvalNoRecovery
                (_, Just inferred) ->
                    runCandidate deadline live gen beforeRaw inferred
                _ ->
                    finishFingerprint
                        deadline
                        live
                        gen
                        beforeRaw
                        ""
                        ""
                        (diagnostic admitOut admitErr)
                        PureEvalRejected
                        PureEvalNoRecovery

    runCandidate deadline live gen beforeRaw inferred = do
        (runOut, runErr) <-
            deadlineCommand
                deadline
                (max 1 (pureEvalTimeoutUs req))
                live
                (evalCommand expression)
        let (verdict, recovery, value, err)
                | commandTimedOut runErr =
                    (PureEvalTimedOut, timeoutRecovery runErr, "", runErr)
                | Just e <- framed pureErrorMarker runOut =
                    (PureEvalRuntimeError, PureEvalNoRecovery, "", e)
                | Just valueOut <- framed pureValueMarker runOut =
                    (PureEvalSucceeded, PureEvalNoRecovery, valueOut, runErr)
                | otherwise =
                    (PureEvalUnshowable, PureEvalNoRecovery, "", "")
        finishFingerprint
            deadline
            live
            gen
            beforeRaw
            inferred
            value
            err
            verdict
            recovery

finishFingerprint ::
    EvalDeadline ->
    Session ->
    Int ->
    Text ->
    Text ->
    Text ->
    Text ->
    PureEvalVerdict ->
    PureEvalRecovery ->
    IO PureEvalResult
finishFingerprint deadline sess gen beforeRaw inferred value err verdict recovery = do
    (deadlineExpired, afterRaw, afterErr) <- fingerprintCommand deadline sess
    baseline <- readIORef (sessBaselineBindings sess)
    let bindingsSame =
            scrubBindingShapes baseline beforeRaw == scrubBindingShapes baseline afterRaw
        itSame = itFingerprint beforeRaw == itFingerprint afterRaw
        invariantOk = bindingsSame && itSame && T.null (T.strip afterErr)
        finalVerdict
            | not invariantOk = PureEvalInvariantFailed
            | deadlineExpired = PureEvalTimedOut
            | otherwise = verdict
        finalRecovery
            | deadlineExpired && verdict /= PureEvalTimedOut = PureEvalNoRecovery
            | otherwise = recovery
        deadlineErr = if deadlineExpired then deadlineMessage deadline else ""
        invariantErr
            | invariantOk = diagnostic err deadlineErr
            | otherwise =
                T.unlines
                    ( filter
                        (not . T.null)
                        [err, deadlineErr, afterErr, "live binding fingerprint changed"]
                    )
    pure $
        (baseResult finalVerdict gen)
            { pureEvalInferredType = inferred
            , pureEvalOutput = value
            , pureEvalError = invariantErr
            , pureEvalBindingsUnchanged = bindingsSame
            , pureEvalItUnchanged = itSame
            , pureEvalRecovery = finalRecovery
            }

baseResult :: PureEvalVerdict -> Int -> PureEvalResult
baseResult verdict gen =
    PureEvalResult verdict gen "" "" "" True True PureEvalNoRecovery

timedOutResult :: Int -> Text -> PureEvalResult
timedOutResult gen err =
    (baseResult PureEvalTimedOut gen)
        { pureEvalError = err
        , pureEvalRecovery = timeoutRecovery err
        }
