{-# LANGUAGE OverloadedStrings #-}

{- | Cheap GHCi introspection (:type, :info, …) over the live session.
Queries are bounded; an abandoned query's stale output self-heals via
the numbered-marker discard in the next drain.
-}
module Sabela.Session.Query (
    queryComplete,
    queryType,
    queryInfo,
    queryKind,
    queryBrowse,
    queryDoc,
    queryHoleFits,
    queryBindings,
    TypecheckInput (..),
    TypecheckResult (..),
    classifyTypecheckInput,
    typecheckValueWith,
    typecheckLetDeclarations,
    evalPureLive,
    captureBindingsBaseline,
    scrubBindings,
    groupEntries,
) where

import Control.Concurrent (withMVar)
import Control.Exception (SomeException, finally, mask, try)
import Data.Char (isAlphaNum, isSpace, toLower)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Sabela.Output (
    pureAdmittedMarker,
    pureErrorMarker,
    pureIOMarker,
    pureValueCap,
    pureValueMarker,
 )
import Sabela.Session (
    Session (..),
    checkProcessAlive,
    getMarker,
    markerText,
    placeMarker,
    readErrorBuffer,
    readSessionGen,
    resetErrorBuffer,
    runBlockStreamingUnlockedWithTimeout,
    sendRaw,
    sessLines,
 )
import Sabela.Session.Drain (drainResultText, drainUntilMarker)
import Sabela.Session.Proc (destroySession)
import Sabela.SessionTypes (
    PureEvalRecovery (..),
    PureEvalRequest (..),
    PureEvalResult (..),
    PureEvalVerdict (..),
 )
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

queryTimeoutUs :: Int
queryTimeoutUs = 10 * 1000000

fingerprintRecoveryGraceUs :: Int
fingerprintRecoveryGraceUs = 1000000

data EvalDeadline = EvalDeadline
    { evalDeadlineNs :: Word64
    , evalDeadlineBudgetUs :: Int
    }

{- | GHCi @:complete repl@ defaults to ~250 matches returned ALPHABETICALLY, so
for the bare @import @ prefix the discovery-relevant namespaces (@DataFrame.*@,
@Granite.*@) sort past the cap and never appear — leaving @find_function@ unable
to discover a re-exported verb like @fit@. With @SABELA_INSTANCE_SURFACING@ set we
ask for the full list (high count); unset keeps the default cap (the baseline the
A/B's OFF arm measures against). No module names are hardcoded.
-}
queryComplete :: Session -> Text -> IO [Text]
queryComplete sess prefix = do
    surfacing <- isJust <$> lookupEnv "SABELA_INSTANCE_SURFACING"
    let countArg = if surfacing then "1000000 " else ""
    res <- runQueryCommand sess (QueryComplete (countArg <> "\"" <> prefix <> "\""))
    pure (concatMap parseCompletionLine (T.lines res))

parseCompletionLine :: Text -> [Text]
parseCompletionLine line =
    let stripped = T.strip line
     in case T.stripPrefix "\"" stripped of
            Just rest -> case T.stripSuffix "\"" rest of
                Just inner -> [inner]
                Nothing -> []
            Nothing -> []

queryType :: Session -> Text -> IO Text
queryType sess name = runQueryCommand sess (QueryType name)

queryInfo :: Session -> Text -> IO Text
queryInfo sess name = runQueryCommand sess (QueryInfo name)

queryKind :: Session -> Text -> IO Text
queryKind sess name = runQueryCommand sess (QueryKind name)

queryBrowse :: Session -> Text -> IO Text
queryBrowse sess mname = runQueryCommand sess (QueryBrowse mname)

queryDoc :: Session -> Text -> IO Text
queryDoc sess name = runQueryCommand sess (QueryDoc name)

{- | Ask GHC which in-scope names fit a concrete goal type. The goal must be a
typed hole expression, e.g. @_ :: [Int] -> Int@; GHC reports the fits in its
error blob, which 'Sabela.AI.Capabilities.Query.parseHoleFits' parses.
-}
queryHoleFits :: Session -> Text -> IO Text
queryHoleFits sess goal = runQueryCommand sess (QueryHoleFits goal)

{- | List the session's interactive bindings, scrubbed of the prelude-injected
ones, so only what the notebook defined remains (@:show bindings@).
-}
queryBindings :: Session -> IO Text
queryBindings sess = do
    raw <- runQueryCommand sess QueryBindings
    baseline <- readIORef (sessBaselineBindings sess)
    pure (scrubBindings baseline raw)

{- | Result from the deliberately limited Path-2 feasibility prototype.
This is not the general primitive: GHCi's @:type@ accepts only expression-
local value bindings, not imports or type/class/instance declarations.
-}
data TypecheckResult = TypecheckResult
    { tcSucceeded :: Bool
    , tcDiagnostics :: Text
    }
    deriving (Eq, Show)

-- | The honest routing boundary of GHCi's no-add @:type@ mechanism.
data TypecheckInput = ValueExpression | ValueBindings | OutsideValueSubset
    deriving (Eq, Show)

classifyTypecheckInput :: Text -> TypecheckInput
classifyTypecheckInput source
    | any outside (meaningfulLines source) = OutsideValueSubset
    | any isBinding (meaningfulLines source) = ValueBindings
    | otherwise = ValueExpression
  where
    outside line = any (`T.isPrefixOf` lower line) excluded
    excluded =
        [ "data "
        , "newtype "
        , "type "
        , "class "
        , "instance "
        , "import "
        , "foreign "
        , "default "
        , "infix"
        , "{-#"
        , "-- cabal:"
        ]
    lower = T.map toLower . T.stripStart
    isBinding line =
        let lhs = T.takeWhile (/= '=') line
         in "=" `T.isInfixOf` line
                && not (T.null (T.strip lhs))
                && T.all validLhs lhs
    validLhs c = isAlphaNum c || isSpace c || c `elem` ("_'(),[]" :: String)

meaningfulLines :: Text -> [Text]
meaningfulLines = filter (not . T.null) . map T.strip . T.lines

{- | Type-check a value-binding group against the live interactive scope
without committing it. Disabled unless @SABELA_TYPECHECK_PRIMITIVE@ is set.
Newlines are rendered as explicit-let semicolons because GHCi commands occupy
one protocol line. The caller must not use this subset as a whole-cell gate.
-}
typecheckLetDeclarations :: Session -> Text -> IO TypecheckResult
typecheckLetDeclarations sess =
    typecheckValueWith
        (runQueryCommand sess . QueryType)
        (runQueryCommand sess QueryBindings)

{- | Run the proven Path-2 subset through any live-session backend. It snapshots
bindings itself: every query both enforces and reports the no-pollution
invariant instead of relying on callers to remember the check.
-}
typecheckValueWith :: (Text -> IO Text) -> IO Text -> Text -> IO TypecheckResult
typecheckValueWith askType askBindings source = do
    started <- getMonotonicTimeNSec
    enabled <- primitiveEnabled
    case (enabled, classifyTypecheckInput source) of
        (False, _) -> finish started "disabled" True "type-check primitive disabled" True
        (_, OutsideValueSubset) ->
            finish started "not-in-value-subset" False "not in the Path-2 value subset" True
        _ -> do
            before <- askBindings
            output <- askType (wrapped source)
            after <- askBindings
            let failed = any (`T.isInfixOf` output) failureSignals
                ok = not failed && expectedSuffix source `T.isInfixOf` output
            finish started (if ok then "ok" else "diagnostic") ok output (before == after)
  where
    failureSignals = ["error:", "Found hole:", "parse error"]
    finish started verdict ok diagnostics unchanged = do
        finished <- getMonotonicTimeNSec
        hPutStrLn stderr $
            "sabela_typecheck mode=path2-value verdict="
                <> verdict
                <> " no_pollution="
                <> map toLower (show unchanged)
                <> " latency_us="
                <> show ((finished - started) `div` 1000)
        pure
            ( TypecheckResult
                (ok && unchanged)
                ( if unchanged
                    then diagnostics
                    else diagnostics <> "\nPath-2 polluted live bindings"
                )
            )
    wrapped s = case classifyTypecheckInput s of
        ValueBindings ->
            ("(let { " <>) . (<> " } in ())") . T.intercalate "; " $ meaningfulLines s
        _ -> s
    expectedSuffix s = case classifyTypecheckInput s of
        ValueBindings -> ":: ()"
        _ -> "::"

primitiveEnabled :: IO Bool
primitiveEnabled = do
    value <- lookupEnv "SABELA_TYPECHECK_PRIMITIVE"
    pure $ maybe True ((`notElem` ["0", "off", "false", "no"]) . map toLower) value

{- | Atomically prove and evaluate one non-@IO@ expression against the live
scope. Both session locks stay held from the generation check through the
before/after fingerprints, so nothing can slip between admission and execution.
-}
evalPureLive :: Session -> PureEvalRequest -> IO PureEvalResult
evalPureLive sess req
    | T.null expr = pure (rejected "expression required")
    | T.any (`elem` ['\n', '\r']) expr =
        pure (rejected "pure live evaluation accepts one protocol line")
    | otherwise =
        withMVar (sessLock sess) $ \_ ->
            withMVar (sessQueryLock sess) $ \_ ->
                mask $ \restore -> do
                    savedErr <- readIORef (sessErrBuf sess)
                    attempted <-
                        try (restore (lockedEval sess req))
                            `finally` writeIORef (sessErrBuf sess) savedErr
                    case attempted of
                        Left e -> do
                            -- A catch-all result must not leave a computation
                            -- or half-framed protocol alive behind a truthful
                            -- 'PureEvalKernelDestroyed' recovery tag.
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
                    (PureEvalRejected, PureEvalNoRecovery, "", diagnostic runOut runErr)
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

lockedCommand :: Int -> Session -> Text -> IO (Text, Text)
lockedCommand budgetUs sess command =
    runBlockStreamingUnlockedWithTimeout budgetUs sess command (\_ -> pure ())

newEvalDeadline :: Int -> IO EvalDeadline
newEvalDeadline requestedUs = do
    now <- getMonotonicTimeNSec
    let budgetUs = max 1 requestedUs
    pure
        EvalDeadline
            { evalDeadlineNs = now + fromIntegral budgetUs * 1000
            , evalDeadlineBudgetUs = budgetUs
            }

remainingDeadlineUs :: EvalDeadline -> IO (Maybe Int)
remainingDeadlineUs deadline = do
    now <- getMonotonicTimeNSec
    pure $
        if now >= evalDeadlineNs deadline
            then Nothing
            else
                Just
                    ( fromIntegral
                        ((evalDeadlineNs deadline - now + 999) `div` 1000)
                    )

deadlineCommand :: EvalDeadline -> Int -> Session -> Text -> IO (Text, Text)
deadlineCommand deadline capUs sess command = do
    remaining <- remainingDeadlineUs deadline
    case remaining of
        Nothing -> pure ("", deadlineMessage deadline)
        Just budgetUs -> lockedCommand (max 1 (min capUs budgetUs)) sess command

fingerprintCommand :: EvalDeadline -> Session -> IO (Bool, Text, Text)
fingerprintCommand deadline sess = do
    remaining <- remainingDeadlineUs deadline
    case remaining of
        Just budgetUs -> do
            (out, err) <-
                lockedCommand (max 1 (min queryTimeoutUs budgetUs)) sess ":show bindings"
            pure (False, out, err)
        Nothing -> do
            -- The semantic deadline owns all candidate work.  This one bounded
            -- grace command only proves post-timeout state before returning.
            (out, err) <- lockedCommand fingerprintRecoveryGraceUs sess ":show bindings"
            pure (True, out, err)

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

deadlineMessage :: EvalDeadline -> Text
deadlineMessage deadline =
    "*** Execution timed out after "
        <> T.pack (show (evalDeadlineBudgetUs deadline))
        <> " microseconds; pure request deadline exhausted ***"

timeoutRecovery :: Text -> PureEvalRecovery
timeoutRecovery err
    | "pure request deadline exhausted" `T.isInfixOf` err = PureEvalNoRecovery
    | otherwise = PureEvalInterrupted

commandTimedOut :: Text -> Bool
commandTimedOut = T.isInfixOf "Execution timed out after"

diagnostic :: Text -> Text -> Text
diagnostic out err = T.strip (T.unlines (filter (not . T.null . T.strip) [err, out]))

admissionCommand :: Text -> Text
admissionCommand expr =
    ":cmd ((\\_sabelaCandidate -> if (Prelude.==) "
        <> "(Data.Typeable.typeRepTyCon (Data.Typeable.typeOf _sabelaCandidate)) "
        <> "(Data.Typeable.typeRepTyCon (Data.Typeable.typeRep "
        <> "(Data.Proxy.Proxy :: Data.Proxy.Proxy (Prelude.IO ())))) "
        <> "then (Prelude.>>) (Prelude.putStrLn "
        <> quoted pureIOMarker
        <> ") ((Prelude.>>) (Prelude.putStrLn (Prelude.show (Data.Typeable.typeOf _sabelaCandidate))) (Prelude.pure \"\")) "
        <> "else (Prelude.>>) (Prelude.putStrLn "
        <> quoted pureAdmittedMarker
        <> ") ((Prelude.>>) (Prelude.putStrLn (Prelude.show (Data.Typeable.typeOf _sabelaCandidate))) (Prelude.pure \"\"))) ("
        <> expr
        <> "))"

evalCommand :: Text -> Text
evalCommand expr =
    ":cmd ((\\_sabelaCandidate -> (Prelude.>>=) "
        <> "((Control.Exception.try (let rendered = Prelude.take "
        <> packed (pureValueCap + 1)
        <> " (Prelude.show _sabelaCandidate) in "
        <> "(Prelude.>>) (Control.Exception.evaluate (Prelude.length rendered)) "
        <> "((Prelude.>>) (Prelude.putStrLn "
        <> quoted pureValueMarker
        <> ") (if (Prelude.>) (Prelude.length rendered) "
        <> packed pureValueCap
        <> " then Prelude.putStrLn ((Prelude.++) (Prelude.take "
        <> packed pureValueCap
        <> " rendered) \"...(truncated)\") else Prelude.putStrLn rendered)))) "
        <> ":: Prelude.IO (Prelude.Either Control.Exception.SomeException ())) "
        <> "(\\result -> case result of { Prelude.Left e -> "
        <> "(Prelude.>>) (Prelude.putStrLn "
        <> quoted pureErrorMarker
        <> ") ((Prelude.>>) (Prelude.putStrLn (Prelude.take "
        <> packed pureValueCap
        <> " (Prelude.show e))) (Prelude.pure \"\")); "
        <> "Prelude.Right () -> Prelude.pure \"\" })) ("
        <> expr
        <> "))"

packed :: (Show a) => a -> Text
packed = T.pack . show

quoted :: Text -> Text
quoted = T.pack . show . T.unpack

framed :: Text -> Text -> Maybe Text
framed marker output = do
    rest <- T.stripPrefix marker (T.stripStart output)
    pure (T.strip rest)

itFingerprint :: Text -> Text
itFingerprint =
    T.intercalate "\n"
        . filter ("it ::" `T.isPrefixOf`)
        . map T.strip
        . groupEntries

{- | Snapshot the prelude-injected bindings as the baseline scrubbed from later
'queryBindings' results. Call once, right after prelude injection.
-}
captureBindingsBaseline :: Session -> IO ()
captureBindingsBaseline sess = do
    raw <- runQueryCommand sess QueryBindings
    writeIORef (sessBaselineBindings sess) (groupEntries raw)

{- | Drop the prelude-injected bindings (those in the baseline), GHCi's @it@, and
the internal @_sabela*@ refs from @:show bindings@ output, leaving the notebook's
own bindings.
-}
scrubBindings :: [Text] -> Text -> Text
scrubBindings baseline = renderScrubbedBindings baseline id

{- | Same scrub as 'scrubBindings', but drops each entry's shown value: forcing
a lazy binding during a pure candidate changes @_@ to a result without
changing its identity, and the fingerprint invariant compares shapes only.
-}
scrubBindingShapes :: [Text] -> Text -> Text
scrubBindingShapes baseline = renderScrubbedBindings baseline bindingSignature

renderScrubbedBindings :: [Text] -> (Text -> Text) -> Text -> Text
renderScrubbedBindings baseline norm current =
    T.intercalate "\n" (map norm (filter keep (groupEntries current)))
  where
    keep e = e `notElem` baseline && not (isHidden (T.strip e))
    -- @instance@ lines carry GHCi's @GhciN.@ counter, which bumps as cells run,
    -- so they drift from the baseline; drop them (they are not value bindings).
    isHidden s =
        any
            (`T.isPrefixOf` s)
            ["it ::", "it =", "_sab", "instance "]

-- | Strip a @:show bindings@ entry's shown value, keeping just @name :: type@.
bindingSignature :: Text -> Text
bindingSignature entry =
    case T.breakOn " = " entry of
        (sig, rest) | not (T.null rest) -> T.stripEnd sig
        _ -> entry

{- | Split @:show bindings@ output into entries: an entry starts on a
non-indented line and absorbs the indented continuation lines that follow it.
-}
groupEntries :: Text -> [Text]
groupEntries = map (T.intercalate "\n") . collect . T.lines
  where
    collect [] = []
    collect (l : ls)
        | starts l = let (cont, rest) = span continues ls in (l : cont) : collect rest
        | otherwise = collect ls
    starts x = not (T.null x) && not (isSpace (T.head x))
    continues x = not (T.null x) && isSpace (T.head x)

{- | Serialise on 'sessQueryLock', not the cell run-lock: a query then
never blocks at the lock level behind a draining cell (the busy gate stays
admission control). The query lock still prevents query/query interleave,
which would corrupt GHCi's shared stdin/stdout stream (case 20).
-}
runQueryCommand :: Session -> QueryCommand -> IO Text
runQueryCommand sess cmd = withMVar (sessQueryLock sess) $ \_ -> do
    resetErrorBuffer sess
    mk <- getMarker sess
    sendRaw sess $ T.unpack $ toText cmd
    placeMarker sess mk
    mRes <-
        timeout queryTimeoutUs $
            drainUntilMarker (sessLines sess) (markerText mk) (\_ -> pure ())
    case mRes of
        Nothing -> pure "*** query timed out ***"
        Just dr -> do
            let out = T.strip (drainResultText dr)
            errLines <- readErrorBuffer sess
            pure $ if T.null out then errLines else out

data QueryCommand
    = QueryType Text
    | QueryInfo Text
    | QueryKind Text
    | QueryBrowse Text
    | QueryDoc Text
    | QueryHoleFits Text
    | QueryComplete Text
    | QueryBindings

toText :: QueryCommand -> Text
toText (QueryType t) = ":type " <> t
toText (QueryInfo t) = ":info " <> t
toText (QueryKind t) = ":kind " <> t
toText (QueryBrowse t) = ":browse " <> t
toText (QueryDoc t) = ":doc " <> t
toText (QueryHoleFits t) =
    ":set -fno-max-valid-hole-fits -frefinement-level-hole-fits=2\
    \ -fsort-by-subsumption-hole-fits\n"
        <> t
toText (QueryComplete t) = ":complete repl " <> t
toText QueryBindings = ":show bindings"
