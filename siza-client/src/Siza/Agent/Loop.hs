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
    mcpInstructions,
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
import Sabela.AI.PromptCore (sabelaBuiltins)
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
    writeSource,
 )
import Siza.Agent.Loop.WrapUp (
    budgetView,
    missRungFloor,
    wrapUpFinal,
    wrapUpOnce,
 )
import Siza.Agent.Messages (
    doneSignalMsg,
    noCheckSignalMsg,
    reenterAlarmMsg,
    streakMsg,
    toolMsg,
    unconfirmedMsgWith,
    verifyMsgWith,
 )
import Siza.Agent.Owned (
    OwnedCell (..),
    StopDecision (..),
    bestFailing,
    hasArtifact,
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
import Siza.Agent.Tools (offeredArgKeys, renderOutcome, toolSurfacePrompt)
import Siza.Agent.Transcript (renderMessage)

data AgentRun = AgentRun
    { arTurns :: Int
    , arToolCalls :: Int
    , arFinal :: Text
    , arStopped :: Text
    , arTranscript :: [Value]
    }
    deriving (Show)

systemPrompt :: Text
systemPrompt = introBlock <> toolSurfacePrompt <> examplesBlock <> sabelaBuiltins

mcpInstructions :: Text
mcpInstructions = introBlock <> examplesBlock <> sabelaBuiltins

introBlock :: Text
introBlock =
    T.unlines
        [ "Pair on a live Sabela reactive Haskell notebook through tools."
        , "Editing or running a cell re-runs every cell downstream of it."
        , "insert_cell and replace_cell_source only commit code that compiles;"
            <> " a rejection carries the compiler's diagnostic so you can fix it and retry."
        , ""
        ]

examplesBlock :: Text
examplesBlock =
    T.unlines
        [ "Examples:"
        , ""
        , "* \"what is already here?\" -> list_cells, then read_cell on the one you care about"
        , "* \"which cell defines the counter?\" -> discover {query: \"counter\"}"
        , "* \"is there a priority queue?\" -> discover {query: \"priority queue\"}"
        , "* \"what is in Data.Map?\" -> discover {module: \"Data.Map\"}"
        , "* \"how do I merge two maps?\""
            <> " -> discover {query: \"Map k v -> Map k v -> Map k v\"}"
        , "* \"how do I thread state?\" -> discover {query: \"StateT\"}"
        , "* \"what arguments does mapAccumL take?\" -> check_type {expr: \"mapAccumL\"}"
        , "* \"will this compile?\" -> try {code: \"...\"}, then insert_cell once it runs"
        , "* \"the kernel says busy\" -> await_idle"
        , ""
        ]

stopTagFor :: CheckResult -> Text
stopTagFor CheckPassed = "done"
stopTagFor _ = "done_unverified"

data Driver = Driver
    { drvChat :: [Value] -> IO (Either Text Turn)
    , drvDispatch :: ToolCall -> IO (Either Text ToolOutcome)
    , drvNow :: IO Double
    , drvVerify :: IO (CheckResult, Maybe Text)
    }

data EpisodeBudget = EpisodeBudget
    { ebMaxRepairs :: Int
    , ebDeadlineSecs :: Double
    }
    deriving (Show)

defaultBudget :: EpisodeBudget
defaultBudget = EpisodeBudget{ebMaxRepairs = 4, ebDeadlineSecs = 600}

runEpisodeWith :: EpisodeBudget -> Driver -> Text -> Int -> IO AgentRun
runEpisodeWith = runEpisodeWith' GrammarOn

runEpisodeWith' ::
    GrammarMode -> EpisodeBudget -> Driver -> Text -> Int -> IO AgentRun
runEpisodeWith' = runEpisodeTraced (const (pure ()))

runEpisodeTraced ::
    (Text -> IO ()) ->
    GrammarMode ->
    EpisodeBudget ->
    Driver ->
    Text ->
    Int ->
    IO AgentRun
runEpisodeTraced = runEpisodeSeeded []

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
    let driver =
            driver0
                { drvChat =
                    fmap (fmap (recoverTurn offeredArgKeys)) . drvChat driver0
                , drvDispatch =
                    guardDiscover ledger (guardDispatch futility (drvDispatch driver0))
                        . normalizeToolCall
                }
    episodeCore ledger emits seed emit mode budget driver prompt maxTurns

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
    (owned0, msgs0) <-
        if null seed
            then do
                exemplars <- retrieveEx
                pre <- runScaffoldStage (drvDispatch driver) prompt
                seedSearchLedger (drvDispatch driver) ledger
                proactive <- proactiveDiscover mode (drvDispatch driver)
                injected0 <- dedupInjected emits 0 (exemplars ++ pre ++ proactive)
                pure (Map.empty, initial ++ injected0)
            else pure (Map.empty, seed ++ [userMsg])
    start <- drvNow driver
    let flush msgs = do
            n <- readIORef printed
            mapM_
                (\(i, m) -> emit (renderMessage i m <> "\n"))
                (zip [n + 1 ..] (drop n msgs))
            writeIORef printed (length msgs)
        finish owned turn nCalls final stopped msgs
            | stopped `elem` repairableGiveUpReasons = do
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
                                    r
                                        | r `elem` [CheckPassed, CheckNotApplicable]
                                        , hasArtifact owned -> do
                                            saveEx owned
                                            finish
                                                owned
                                                (turn + 1)
                                                nCalls
                                                (turnContent t)
                                                (stopTagFor r)
                                                (msgs ++ [turnRaw t])
                                    _ -> do
                                        s <- readIORef stuck
                                        if s + 1 >= maxStuckVerifies
                                            then finish owned (turn + 1) nCalls stuckFinal "stuck" (msgs ++ [turnRaw t])
                                            else do
                                                writeIORef stuck (s + 1)
                                                let vmsg = case result of
                                                        CheckUncheckable -> unconfirmedDiagMsg mEv owned
                                                        _ -> diagVerifyMsg mEv owned
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
                                        [ (c, ocDiagnostic oc, ocInvariantAlarm oc)
                                        | (c, oc) <- Map.toList owned'
                                        , not (ocHealthy oc)
                                        ]
                                    still = [c | (c, _, _) <- stillPairs]
                                    msg =
                                        if null still
                                            then unconfirmedDiagMsg Nothing owned'
                                            else reenterAlarmMsg stillPairs
                                    sig = redSignature still owned'
                                out <- emitTurn emits turn (turnRaw t) (msg : redisc)
                                let msgs' = msgs ++ out
                                writeIORef stuck 0
                                rs <- readIORef reenterStuck
                                seen <- readIORef seenRedSigs
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
                            signalMsgs <- doneSignalProbe signalled done0
                            unless (null signalMsgs) $
                                writeIORef signalDone True
                            let nudge = []
                            let owned' =
                                    foldr recordOwned owned [(c, o) | (c, Right o) <- results]
                                toolMsgs =
                                    [ toolMsg c (either id renderOutcome o)
                                    | (c, o) <- results
                                    ]
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
    repairableGiveUpReasons :: [Text]
    repairableGiveUpReasons = ["stuck", "stuck_reenter"]

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
    diagVerifyMsg mCe owned =
        verifyMsgWith
            (Map.size owned)
            (missingDeliverables prompt (map ocSource (Map.elems owned)))
            mCe
    unconfirmedDiagMsg mEv owned =
        unconfirmedMsgWith
            (Map.size owned)
            (missingDeliverables prompt (map ocSource (Map.elems owned)))
            mEv
    doneSignalProbe signalled done0
        | not done0 = pure []
        | otherwise = do
            already <- readIORef signalled
            if already
                then pure []
                else do
                    writeIORef signalled True
                    (r, _) <- drvVerify driver
                    pure $ case r of
                        CheckPassed -> [doneSignalMsg]
                        CheckNotApplicable -> [noCheckSignalMsg]
                        _ -> []
    retrieveEx = retrieveForPrompt prompt
    saveEx owned =
        saveVerified
            prompt
            [ocSource oc | oc <- Map.elems owned, ocHealthy oc]
    dispatchCall msgs call = do
        k <- sampleK
        if k > 1 && callActs call && isJust (writeSource call)
            then rejectionDispatch msgs k call
            else plainDispatch call
    plainDispatch call = do
        outcome <- drvDispatch driver call
        case blockingCell outcome of
            Just n -> do
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

    surfaceDisplay call outcome = do
        repaired <- repairDisplayContract prompt (drvDispatch driver) call outcome
        pure $ case repaired of
            Just (c, o) -> (c, Right o)
            Nothing -> (call, Right outcome)

    rejectionDispatch msgs k call = do
        o0 <- drvDispatch driver call
        case ownedCellOutcome call o0 of
            Just (cid, False) -> do
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
            _ -> pure (call, Right o0)
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
    deliverableLanded (c, Right o) =
        maybe False snd (ownedCellOutcome c o) && not (declaresDepsCall c)
    deliverableLanded _ = False
    rankedFacts owned =
        closeSearchLedgerRanked prompt (map ocSource (Map.elems owned)) ledger
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
