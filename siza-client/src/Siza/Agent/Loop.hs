{- | The agent episode loop: a local model drives the siza tools until it stops
calling them or hits the turn cap, re-entering while any owned cell is red within
a repair-round budget @D@ ('ebMaxRepairs') and a wall-clock deadline.

Under 'GrammarOn' the discover stage ('Siza.Agent.Discover') synthesises a grammar
from live @:browse@: proactively over the notebook's imports at start, after an
install, and as the step-4 seam re-browses a cell's modules on a not-in-scope
error. The scaffold ('Siza.Agent.Scaffold') pre-commits the named data file's
load as a disclosed, outcome-gated setup write; substitute-and-verify
('Siza.Agent.Repair') repairs any red owned cell from GHC
hole-fits, keeping the first fit that compiles.

The loop takes the request as plain 'Text' and injects verification through the
'Driver'; graders and the benchmark 'Task' stay in the eval harness that wraps it.
-}
module Siza.Agent.Loop (
    AgentRun (..),
    StopDecision (..),
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (..),
    SampleVerify (..),
    SampleResult (..),
    defaultBudget,
    runEpisodeWith,
    runEpisodeWith',
    runEpisodeTraced,
    runEpisodeSeeded,
    sampleVerifyOne,
    ownedCellOutcome,
    stopDecision,
    discoverModules,
    systemPrompt,
    sampleK,
    writeSource,
    qualifiedBaseNames,
) where

import Control.Monad (unless, void, when)
import Data.Aeson (Value (..), object, (.=))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Grammar (discoverGrammarBlock)
import Sabela.AI.PromptCore (sharedPromptCoreWith)
import Sabela.AI.Types (ToolOutcome (..))

import Sabela.AI.Salvage (salvageCell)
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..))
import Siza.Agent.Check (CheckResult (..))
import Siza.Agent.Deliverable (missingDeliverables)
import Siza.Agent.Discover (
    GrammarMode (..),
    declaresDepsCall,
    discoverModules,
    proactiveDiscover,
    runDiscoverOutcomes,
    seamDiscover,
 )
import Siza.Agent.Discover.History (SearchLedger)
import Siza.Agent.Discover.HistoryGuard (
    closeSearchLedgerRanked,
    guardDiscover,
    heldCallReady,
    newSearchLedger,
    seedSearchLedger,
    setSearchPressure,
 )
import Siza.Agent.EmitLedger (
    EmitLedger,
    dedupInjected,
    emitTurn,
    newEmitLedger,
 )
import Siza.Agent.Exemplars (retrieveForPrompt, saveVerified)
import Siza.Agent.Futility (guardDispatch, newFutilityGuard)
import Siza.Agent.Loop.Escalate (escalateNudge)
import Siza.Agent.Loop.Route (blockingCell, discloseRoute)
import Siza.Agent.Loop.Support (
    callActs,
    groundingMsgs,
    maxChatRetries,
    maxStuckVerifies,
    qualifiedBaseNames,
    replaceCall,
    sampleK,
    streakHints,
    stuckFinal,
    updateNudge,
    writeSource,
 )
import Siza.Agent.Loop.WrapUp (
    budgetView,
    escalationK,
    missRungFloor,
    wrapUpFinal,
    wrapUpOnce,
 )
import Siza.Agent.Messages (
    doneSignalMsg,
    reenterMsg,
    streakMsg,
    toolMsg,
    unconfirmedMsgWith,
    verifyMsgWith,
 )
import Siza.Agent.Owned (
    OwnedCell (..),
    StopDecision (..),
    bestFailing,
    latestDraft,
    noProgressStep,
    ownedCellOutcome,
    recordOwned,
    redSignature,
    stopDecision,
 )
import Siza.Agent.RenderContract (repairDisplayContract)
import Siza.Agent.Repair (repairRedCells)
import Siza.Agent.Repair.Blocking (repairBlockingCell)
import Siza.Agent.Sample (SampleResult (..), SampleVerify (..), sampleVerifyOne)
import Siza.Agent.Scaffold (runScaffoldStage)
import Siza.Agent.ToolRoute (normalizeToolCall, recoverTurn)
import Siza.Agent.Tools (offeredArgKeys, renderOutcome)
import Siza.Agent.Transcript (renderMessage)

data AgentRun = AgentRun
    { arTurns :: Int
    , arToolCalls :: Int
    , arFinal :: Text
    , arStopped :: Text
    , arTranscript :: [Value]
    -- ^ The episode's full message list, for 'Siza.Agent.Transcript.renderTranscript'.
    }
    deriving (Show)

{- | TODO: mchavinda - the system prompt might be moot cause
| small models forget anyway.
-}
systemPrompt :: Text
systemPrompt =
    "Pair on a live Sabela reactive Haskell notebook through tools; editing or \
    \running a cell re-runs everything downstream. insert_cell adds a cell (full \
    \source, with the requested type signature); replace_cell_source fixes one. \
    \Once the deliverable's cell runs clean, give a one-line summary and stop; do \
    \not ask questions.\n\n"
        <> sharedPromptCoreWith discoverGrammarBlock

{- | The episode's side effects, injectable so the loop is testable without a
live model or server. Production wires Ollama @chat@ and the siza @dispatch@.
-}
data Driver = Driver
    { drvChat :: [Value] -> IO (Either Text Turn)
    , drvDispatch :: ToolCall -> IO (Either Text ToolOutcome)
    , drvNow :: IO Double
    -- ^ Wall-clock reading in seconds, injectable so the deadline is testable.
    , drvVerify :: IO (CheckResult, Maybe Text)
    {- ^ The three-valued covering-test verdict (R5-T5): 'CheckFailed' carries
    the counterexample (a denial always has positive evidence); 'CheckUncheckable'
    carries what-to-run guidance, never a failure claim. "done" = test greens.
    -}
    }

{- | What an episode is allowed to spend before it gives up. @D@ caps how many
times the health gate may re-enter the loop; the deadline caps total time.
-}
data EpisodeBudget = EpisodeBudget
    { ebMaxRepairs :: Int
    , ebDeadlineSecs :: Double
    }
    deriving (Show)

-- | The weak-model default: a few repair rounds inside a generous deadline.
defaultBudget :: EpisodeBudget
defaultBudget = EpisodeBudget{ebMaxRepairs = 4, ebDeadlineSecs = 600}

runEpisodeWith :: EpisodeBudget -> Driver -> Text -> Int -> IO AgentRun
runEpisodeWith = runEpisodeWith' GrammarOn

{- | Run an episode under an explicit grammar mode. 'GrammarOn' synthesises the
live-browse grammar (proactively at start, after installs, and on the re-discover
seam); 'GrammarOff' is the raw-dump baseline the negative control measures.
-}
runEpisodeWith' ::
    GrammarMode -> EpisodeBudget -> Driver -> Text -> Int -> IO AgentRun
runEpisodeWith' = runEpisodeTraced (const (pure ()))

{- | The episode loop with a trace sink: 'flush' streams each newly appended
message via 'renderMessage', so 'runEpisodeWith'' and the live-audit debug path
share one body. The sink sees the prompt, inputs, thinking, calls, and outcomes.
-}
runEpisodeTraced ::
    (Text -> IO ()) ->
    GrammarMode ->
    EpisodeBudget ->
    Driver ->
    Text ->
    Int ->
    IO AgentRun
runEpisodeTraced = runEpisodeSeeded []

{- | Like 'runEpisodeTraced' but seeded with a prior transcript: an empty seed
starts fresh (prompt, scaffold, discovery); a non-empty one appends the new
user turn only. The dispatch seam runs under the 'Siza.Agent.Futility' guard.
-}
runEpisodeSeeded ::
    [Value] ->
    (Text -> IO ()) ->
    GrammarMode ->
    EpisodeBudget ->
    Driver ->
    Text ->
    Int ->
    IO AgentRun
runEpisodeSeeded seed emit mode budget driver0 prompt maxTurns = do
    futility <- newFutilityGuard
    ledger <- newSearchLedger
    emits <- newEmitLedger
    -- R1.7: the arg envelope is unwrapped BEFORE the guards, so futility and
    -- the history ledger see the same normalised keys the dispatcher resolves.
    -- R9-T5: garbled names with a unique schema match are recovered at the
    -- chat seam and stamped into turnRaw before anything reads the turn.
    let driver =
            driver0
                { drvChat =
                    fmap (fmap (recoverTurn offeredArgKeys)) . drvChat driver0
                , drvDispatch =
                    guardDiscover ledger (guardDispatch futility (drvDispatch driver0))
                        . normalizeToolCall
                }
    episodeCore ledger emits seed emit mode budget driver prompt maxTurns

-- | The episode body 'runEpisodeSeeded' runs under the guarded driver.
episodeCore ::
    IORef SearchLedger ->
    IORef EmitLedger ->
    [Value] ->
    (Text -> IO ()) ->
    GrammarMode ->
    EpisodeBudget ->
    Driver ->
    Text ->
    Int ->
    IO AgentRun
episodeCore ledger emits seed emit mode budget driver prompt maxTurns = do
    printed <- newIORef (0 :: Int)
    consec <- newIORef (0 :: Int)
    nudgeRung <- newIORef (0 :: Int)
    delivered <- newIORef False
    signalled <- newIORef False
    signalDone <- newIORef False
    chatRetries <- newIORef (0 :: Int)
    stuck <- newIORef (0 :: Int)
    reenterStuck <- newIORef (0 :: Int)
    seenRedSigs <- newIORef Set.empty
    streaks <- newIORef Map.empty
    wrapped <- newIORef False
    lastDitch <- newIORef False
    start <- drvNow driver
    (owned0, msgs0) <-
        if null seed
            then do
                exemplars <- retrieveEx
                pre <- runScaffoldStage (drvDispatch driver) prompt
                -- Turn-0 assertion seeding (search-api.md section 11), after
                -- the scaffold so its imports count as environment facts.
                seedSearchLedger (drvDispatch driver) ledger
                proactive <- proactiveDiscover mode (drvDispatch driver)
                injected0 <- dedupInjected emits 0 (exemplars ++ pre ++ proactive)
                pure (Map.empty, initial ++ injected0)
            else pure (Map.empty, seed ++ [userMsg])
    let flush msgs = do
            n <- readIORef printed
            mapM_
                (\(i, m) -> emit (renderMessage i m <> "\n"))
                (zip [n + 1 ..] (drop n msgs))
            writeIORef printed (length msgs)
        -- The final routes through 'wrapUpFinal': an empty final is
        -- unrepresentable under every stop reason (R8.3, R6-T3).
        finish owned turn nCalls final stopped msgs
            | stopped `elem` repairableGiveUpReasons = do
                -- One last-ditch class-keyed cascade over the owned reds before a
                -- give-up final (§9.5): heal the compiler-named cell rather than
                -- report it red. Runs at most once per episode.
                already <- readIORef lastDitch
                (owned', fixes) <-
                    if already
                        then pure (owned, [])
                        else do
                            writeIORef lastDitch True
                            fixes <-
                                repairRedCells (drvDispatch driver) $
                                    [ (c, ocDiagnostic oc)
                                    | (c, oc) <- Map.toList owned
                                    , not (ocHealthy oc)
                                    ]
                            pure (foldr recordOwned owned fixes, fixes)
                let repairMsgs = concatMap auditedRepairMessages fixes
                    msgs' = msgs ++ repairMsgs
                flush msgs'
                    >> pure
                        ( AgentRun
                            turn
                            (nCalls + length fixes)
                            (wrapUpFinal stopped owned' (bestFailing owned'))
                            stopped
                            msgs'
                        )
            | otherwise =
                flush msgs
                    >> pure
                        ( AgentRun
                            turn
                            nCalls
                            (wrapUpFinal stopped owned final)
                            stopped
                            msgs
                        )
        -- R5.6 pressure + the once-per-episode last-turn wrap-up (R5.7).
        preTurn elapsed turn repairs owned = do
            setSearchPressure ledger (missRungFloor maxTurns (maxTurns - turn))
            wrap <-
                wrapUpOnce wrapped (rankedFacts owned) $
                    budgetView
                        maxTurns
                        turn
                        (ebMaxRepairs budget)
                        repairs
                        elapsed
                        (ebDeadlineSecs budget)
            dedupInjected emits turn wrap
        go start' turn nCalls repairs owned msgs = do
            flush msgs
            if turn >= maxTurns
                then finish owned turn nCalls (bestFailing owned) "max_turns" msgs
                else
                    if repairs >= ebMaxRepairs budget
                        then finish owned turn nCalls (bestFailing owned) "repair_budget" msgs
                        else do
                            now <- drvNow driver
                            if now - start' >= ebDeadlineSecs budget
                                then finish owned turn nCalls (bestFailing owned) "deadline" msgs
                                else do
                                    wrap <- preTurn (now - start') turn repairs owned
                                    step start' turn nCalls repairs owned (msgs ++ wrap)
        step start' turn nCalls repairs owned msgs = do
            res <- drvChat driver msgs
            case res of
                Left e -> do
                    -- N10: a chat error (e.g. malformed tool-args JSON) is a
                    -- bounded retry of the same turn, not a silent terminal
                    -- failure the next turn inherits as if progress was made.
                    r <- readIORef chatRetries
                    if r < maxChatRetries
                        then do
                            writeIORef chatRetries (r + 1)
                            step start' turn nCalls repairs owned msgs
                        else do
                            writeIORef chatRetries 0
                            finish owned turn nCalls ("chat error after retries: " <> e) "error" msgs
                Right t -> do
                    writeIORef chatRetries 0
                    if null (turnCalls t)
                        then case stopDecision (Map.map ocHealthy owned) of
                            Stop
                                | Map.null owned
                                , Just src <- salvageCell (turnContent t) -> do
                                    let call = ToolCall "insert_cell" (object ["source" .= src])
                                    outcome <- drvDispatch driver call
                                    let owned' = recordOwned (call, outcome) owned
                                        salvaged = ToolCall "salvage" (tcArgs call)
                                    out <-
                                        emitTurn
                                            emits
                                            turn
                                            (turnRaw t)
                                            [toolMsg salvaged (renderOutcome outcome)]
                                    writeIORef stuck 0
                                    go start' (turn + 1) (nCalls + 1) repairs owned' (msgs ++ out)
                            Stop -> do
                                (result, mEv) <- drvVerify driver
                                case result of
                                    CheckPassed -> do
                                        saveEx owned
                                        finish owned (turn + 1) nCalls (turnContent t) "done" (msgs ++ [turnRaw t])
                                    _ -> do
                                        -- No-progress guard: model declared done, the check did
                                        -- not confirm, nothing changed. A few in a row means a bad
                                        -- or uncheckable check, so stop rather than spin.
                                        s <- readIORef stuck
                                        if s + 1 >= maxStuckVerifies
                                            then finish owned (turn + 1) nCalls stuckFinal "stuck" (msgs ++ [turnRaw t])
                                            else do
                                                writeIORef stuck (s + 1)
                                                let vmsg = case result of
                                                        CheckFailed -> diagVerifyMsg mEv owned
                                                        _ -> unconfirmedDiagMsg mEv owned
                                                out <- emitTurn emits turn (turnRaw t) [vmsg]
                                                go
                                                    start'
                                                    (turn + 1)
                                                    nCalls
                                                    (repairs + 1)
                                                    owned
                                                    (msgs ++ out)
                            Reenter reds -> do
                                owned' <- repairReds owned reds
                                redisc <- reDiscover delivered owned' reds
                                let stillPairs =
                                        [ (c, ocDiagnostic oc)
                                        | (c, oc) <- Map.toList owned'
                                        , not (ocHealthy oc)
                                        ]
                                    still = map fst stillPairs
                                    -- No check ran on this path, so a failure
                                    -- claim would be unevidenced (R5-T5).
                                    msg =
                                        if null still
                                            then unconfirmedDiagMsg Nothing owned'
                                            else reenterMsg stillPairs
                                    sig = redSignature still owned'
                                out <- emitTurn emits turn (turnRaw t) (msg : redisc)
                                let msgs' = msgs ++ out
                                writeIORef stuck 0
                                rs <- readIORef reenterStuck
                                seen <- readIORef seenRedSigs
                                -- Reenter no-progress guard: a still-red signature
                                -- seen before (stuck OR A/B/A/B oscillation) counts;
                                -- maxStuckVerifies repeats stops the spin honestly.
                                let (seen', repeated) = noProgressStep seen sig
                                if not (null still) && repeated
                                    then
                                        if rs + 1 >= maxStuckVerifies
                                            then
                                                finish
                                                    owned'
                                                    (turn + 1)
                                                    nCalls
                                                    (bestFailing owned')
                                                    "stuck_reenter"
                                                    msgs'
                                            else do
                                                writeIORef reenterStuck (rs + 1)
                                                writeIORef seenRedSigs seen'
                                                go start' (turn + 1) nCalls (repairs + 1) owned' msgs'
                                    else do
                                        writeIORef reenterStuck 0
                                        writeIORef
                                            seenRedSigs
                                            (if null still then seen else seen')
                                        go start' (turn + 1) nCalls (repairs + 1) owned' msgs'
                        else do
                            results <- mapM (dispatchCall msgs) (turnCalls t)
                            let dispatched = [c | (c, Right _) <- results]
                            -- R6.10: discovery attaches only to a successful
                            -- install call, and the channel closes for good
                            -- once a clean deliverable write has landed.
                            done0 <- readIORef delivered
                            discovered <-
                                if done0
                                    then pure []
                                    else
                                        runDiscoverOutcomes
                                            mode
                                            (drvDispatch driver)
                                            [(c, o) | (c, Right (Right o)) <- results]
                            when (any deliverableLanded results) $
                                writeIORef delivered True
                            -- R5-T5 done-signal: tools kept coming a full turn
                            -- after a clean deliverable write — probe ONCE; only
                            -- a passing recomputation emits the stop line.
                            signalMsgs <- doneSignalProbe signalled done0
                            unless (null signalMsgs) $
                                writeIORef signalDone True
                            -- R5.7: once the stop line is out, no channel may
                            -- emit search/read advice — the act nudge closes.
                            quiet <- readIORef signalDone
                            -- R5.6: nudge once a call-ready fact is held and
                            -- reads keep coming (never fact-free/below floor).
                            ready <- heldCallReady ledger
                            nudge <-
                                if quiet
                                    then pure []
                                    else
                                        updateNudge
                                            (mkNudge nudgeRung turn owned)
                                            consec
                                            (escalationK maxTurns (maxTurns - turn))
                                            ready
                                            (maxTurns - turn)
                                            dispatched
                            let owned' =
                                    foldr recordOwned owned [(c, o) | (c, Right o) <- results]
                                toolMsgs =
                                    [ toolMsg c (either id renderOutcome o)
                                    | (c, o) <- results
                                    ]
                            -- Mid-loop contrast: a red diagnostic persisting
                            -- streakThreshold turns gets the wrong-vs-real
                            -- message NOW, not only at the stop rail.
                            hints <- streakHints streaks owned'
                            out <-
                                emitTurn emits turn (turnRaw t) $
                                    toolMsgs
                                        ++ discovered
                                        ++ map streakMsg hints
                                        ++ signalMsgs
                                        ++ nudge
                            writeIORef stuck 0
                            go
                                start'
                                (turn + 1)
                                (nCalls + length dispatched)
                                repairs
                                owned'
                                (msgs ++ out)
    go start 0 0 0 owned0 msgs0
  where
    -- Only no-progress stops may spend one last repair. Hard turn, repair and
    -- wall-clock budget boundaries perform no work after exhaustion.
    repairableGiveUpReasons :: [Text]
    repairableGiveUpReasons = ["stuck", "stuck_reenter"]

    -- A harness-authored repair is represented as an assistant call followed by
    -- its result, so the persisted transcript explains every state-changing
    -- write and satisfies the same call/result cardinality law as model calls.
    auditedRepairMessages (tc, out) =
        [ object
            [ "role" .= ("assistant" :: Text)
            , "content" .= ("Automatic final repair attempt." :: Text)
            , "tool_calls" .= [object ["function" .= callFunction]]
            ]
        , toolMsg tc (renderOutcome out)
        ]
      where
        callFunction = object ["name" .= tcName tc, "arguments" .= tcArgs tc]
    initial =
        [ object ["role" .= ("system" :: Text), "content" .= systemPrompt]
        , userMsg
        ]
    userMsg = object ["role" .= ("user" :: Text), "content" .= prompt]
    -- The diagnostic verify re-prompt: name what is missing (no cell at all,
    -- an undefined requested deliverable, or the failing counterexample).
    diagVerifyMsg mCe owned =
        verifyMsgWith
            (Map.size owned)
            (missingDeliverables prompt (map ocSource (Map.elems owned)))
            mCe
    -- The uncheckable twin of diagVerifyMsg: same structural diagnosis, but
    -- phrased as not-yet-confirmed — never as a failure claim (R5-T5).
    unconfirmedDiagMsg mEv owned =
        unconfirmedMsgWith
            (Map.size owned)
            (missingDeliverables prompt (map ocSource (Map.elems owned)))
            mEv
    -- At most one contract probe per episode, and only after the model kept
    -- going past a landed deliverable; only CheckPassed emits anything.
    doneSignalProbe signalled done0
        | not done0 = pure []
        | otherwise = do
            already <- readIORef signalled
            if already
                then pure []
                else do
                    writeIORef signalled True
                    (r, _) <- drvVerify driver
                    pure [doneSignalMsg | r == CheckPassed]
    -- Learning loop ('Siza.Agent.Exemplars'), gated by SIZA_EXEMPLAR_STORE.
    retrieveEx = retrieveForPrompt prompt
    saveEx owned =
        saveVerified
            prompt
            [ocSource oc | oc <- Map.elems owned, ocHealthy oc]
    -- Source comes from the tool-call args already repaired by parseTurn's
    -- recovery layer. A cell write routes through rejection sampling when
    -- SIZA_SAMPLE_K > 1; every other call dispatches once.
    dispatchCall msgs call = do
        k <- sampleK
        if k > 1 && callActs call && isJust (writeSource call)
            then rejectionDispatch msgs k call
            else plainDispatch call
    plainDispatch call = do
        outcome <- drvDispatch driver call
        case blockingCell outcome of
            Just n -> do
                -- Heal the blocking cell with its own hole-fits FIRST, before
                -- the red is handed back to the model to thrash on (§5.4/§9.5).
                healed <- repairBlockingCell (drvDispatch driver) n
                case healed of
                    Just (c, o) -> surfaceDisplay c o
                    Nothing
                        | tcName call == "insert_cell"
                        , Just src <- writeSource call -> do
                            let rc = replaceCall n src
                            o2 <- drvDispatch driver rc
                            surfaceDisplay rc (fmap (discloseRoute n) o2)
                    _ -> pure (call, Right outcome)
            _ -> surfaceDisplay call outcome

    -- A clean markup-producing write with no visible output gets one
    -- type-directed, compile-and-output-vetted display repair. Failed proposals
    -- are restored inside the generator and the original result is preserved.
    surfaceDisplay call outcome = do
        repaired <- repairDisplayContract prompt (drvDispatch driver) call outcome
        pure $ case repaired of
            Just (c, o) -> (c, Right o)
            Nothing -> (call, Right outcome)

    -- Rejection-sample a cell write: on a red proposal, re-ask up to K-1 times and
    -- keep the first that compiles, else restore the original. Verifies in the real
    -- notebook (deps, data files) via 'Siza.Agent.Sample.sampleVerifyOne'.
    rejectionDispatch msgs k call = do
        o0 <- drvDispatch driver call
        case ownedCellOutcome call o0 of
            Just (cid, False) -> do
                -- Ground the re-ask: answer the model's guessed API with the real
                -- signatures from the live index, so re-samples are grounded, not
                -- blind re-guesses of the same types. Computed once, reused per sample.
                ground <- groundingMsgs (drvDispatch driver) (fromMaybe "" (writeSource call))
                let msgs' = msgs ++ ground
                winRef <- newIORef Nothing
                let sv =
                        SampleVerify
                            { svSample = const (fromMaybe "" <$> reAskSource msgs')
                            , svRollout = rolloutReplace winRef cid
                            , svInsert = const (pure ())
                            }
                _ <- sampleVerifyOne (k - 1) sv
                mWin <- readIORef winRef
                case mWin of
                    Just win -> pure win
                    Nothing -> restoreOriginal cid call o0
            -- Already compiled, or not a cell-write outcome: nothing to sample.
            _ -> pure (call, Right o0)
    -- Replace the cell with a candidate and keep it iff the run compiled; capture
    -- the winning (call, outcome) so the caller records the real committed write.
    rolloutReplace winRef cid src
        | T.null (T.strip src) = pure False
        | otherwise = do
            let rc = replaceCall cid src
            o <- drvDispatch driver rc
            let ok = maybe False snd (ownedCellOutcome rc o)
            when ok (writeIORef winRef (Just (rc, Right o)))
            pure ok
    restoreOriginal cid call o0 = do
        _ <-
            maybe
                (pure ())
                (void . drvDispatch driver . replaceCall cid)
                (writeSource call)
        pure (call, Right o0)
    -- One more diverse proposal for the same step; temperature (env) makes each
    -- re-ask differ. The first cell-writing call's source, if any.
    reAskSource msgs = do
        r <- drvChat driver msgs
        pure $ case r of
            Right t -> listToMaybe [s | c <- turnCalls t, callActs c, Just s <- [writeSource c]]
            Left _ -> Nothing
    repairReds owned reds = do
        fixes <-
            repairRedCells
                (drvDispatch driver)
                [(c, ocDiagnostic oc) | c <- reds, Just oc <- [Map.lookup c owned]]
        pure (foldr recordOwned owned fixes)
    -- A clean write with no dep declaration: the deliverable landed (R6.10).
    deliverableLanded (c, Right o) =
        maybe False snd (ownedCellOutcome c o) && not (declaresDepsCall c)
    deliverableLanded _ = False
    -- R7-T4: nudge and wrap-up close the ledger through the ranked selection.
    rankedFacts owned =
        closeSearchLedgerRanked prompt (map ocSource (Map.elems owned)) ledger
    -- R8-T3 escalation in kind: rung 1 echoes facts, rung 2+ carries the
    -- typed-hole candidate ('Siza.Agent.Loop.Escalate').
    mkNudge rung turn owned = do
        facts <- rankedFacts owned
        escalateNudge rung (latestDraft owned) facts $
            "Remaining turn budget: "
                <> T.pack (show (maxTurns - turn))
                <> "."
    reDiscover dref owned' reds = do
        done <- readIORef dref
        if done
            then pure []
            else seamDiscover mode (drvDispatch driver) (redCells owned' reds)
    redCells owned' reds =
        [ (ocSource oc, ocDiagnostic oc)
        | c <- reds
        , Just oc <- [Map.lookup c owned']
        , not (ocHealthy oc)
        ]
