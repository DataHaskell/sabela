{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Synchronous cell-execution helpers split out of "Sabela.AI.Capabilities.Edit".

These are the pieces every mutating tool (@replace_cell_source@,
@insert_cell@) calls so the tool response carries the freshly-computed
execution summary, plus the @execute_cell@ tool itself. Kept as a
separate module because the listener-and-timeout pattern in 'executeCell'
is also a natural reuse point for a REST blocking-run endpoint.
-}
module Sabela.AI.Capabilities.Edit.Run (
    autoExecuteAfterMutation,
    execExecuteCell,
    executeCell,
    missingCellError,
    parseRepairBudget,
    repairTierOrder,
    abortCancelled,
    abortSuperseded,
    abortTimedOut,
) where

import Data.Aeson (Value, (.=))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (diffUTCTime, getCurrentTime)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Sabela.AI.Capabilities.Bindings (attachWriteEcho)
import Sabela.AI.Capabilities.Edit.Assemble (applicationCandidates)
import Sabela.AI.Capabilities.Edit.Exec (
    abortCancelled,
    abortSuperseded,
    abortTimedOut,
    executeCell,
 )
import Sabela.AI.Capabilities.Edit.HoleSearch (
    argInsertCandidates,
    holeFitCandidates,
    holeSearchCandidates,
 )
import Sabela.AI.Capabilities.Edit.Repair (
    ambiguousResolveCandidates,
    firstFix,
    hoogleCandidates,
    importResolveCandidates,
    moduleDepStep,
    moduleResolveCandidates,
    typeDiscoverCandidates,
 )
import Sabela.AI.Capabilities.Edit.TypeSelect (selectCleanByTypeCheck)
import Sabela.AI.Capabilities.Util (fieldInt)
import Sabela.AI.CellResult (mergeToolOk, toCellResult)
import Sabela.AI.ErrorIndex (errorInfoForCell, errorInfoPairs, withErrorInfo)
import Sabela.AI.Health (healthOfResult, improvesHealthFor, isClean)
import Sabela.AI.Repair (firstJustM)
import Sabela.AI.RepairTrace (RepairEvent (..), recordRepair)
import Sabela.AI.SelfHeal (attachSelfHeal, selfHealNote)
import Sabela.AI.Store
import Sabela.AI.Triage (triageResult)
import Sabela.AI.Types
import Sabela.Anthropic.Types (CancelToken)
import Sabela.Api (errorJson)
import Sabela.Diagnose (
    cellResultWithGuidance,
    guidanceForCell,
    guidancePairs,
 )
import Sabela.Handlers (ReactiveNotebook (..), updateCellSource)
import Sabela.Model
import Sabela.Parse (cellNames)
import Sabela.Parse.Declared (preservesDeclarations)
import Sabela.State

{- | Run a single cell via the reactive notebook and return the typed
'CellResult' JSON for embedding as the mutation-tool @execution@ summary.
The outcome sum (Succeeded/Raised/Rejected/Aborted) and the @ok@ boolean
ride on the same value the @execute_cell@ tool emits.

@_store@ is the (currently unused) carrier for a staged Output chokepoint;
@crOutputs@ inline raw here. The in-browser chat is bounded by
'Sabela.AI.Orchestrator.Compact'; the REST bridge ('aiToolH') is deliberately
un-stashed on this path.
-}
autoExecuteAfterMutation ::
    App -> AIStore -> ReactiveNotebook -> CancelToken -> Int -> IO Value
autoExecuteAfterMutation app store rn cancelTok cid = do
    pre <- cellSrc app cid
    res0 <- executeWithRepair app store rn cid cancelTok
    post <- cellSrc app cid
    let res = triageResult post res0
    let cr = toCellResult res (resultOutputs res)
    attachWriteEcho app (isClean (healthOfResult res)) post $
        attachSelfHeal
            (selfHealNote pre post)
            (withErrorInfo cr (cellResultWithGuidance cr))

{- | @execute_cell@. @_store@ is the staged Output-chokepoint carrier — see
'autoExecuteAfterMutation'; @crOutputs@ inline raw on this path.
-}
execExecuteCell ::
    App -> AIStore -> ReactiveNotebook -> CancelToken -> Value -> IO ToolOutcome
execExecuteCell app store rn cancelTok input =
    case fieldInt "cell_id" input of
        Nothing -> pure (errOutcome (errorJson "cell_id required"))
        Just cid -> do
            nb <- readNotebook (appNotebook app)
            case missingCellError (nbCells nb) cid of
                Just msg -> pure (errOutcome (errorJson msg))
                Nothing -> do
                    pre <- cellSrc app cid
                    result0 <- executeWithRepair app store rn cid cancelTok
                    post <- cellSrc app cid
                    let result = triageResult post result0
                    let cr = toCellResult result (resultOutputs result)
                        heal =
                            maybe [] (\n -> ["self_heal" .= n]) (selfHealNote pre post)
                    pure $
                        mergeToolOk
                            cr
                            ( ["cellId" .= cid]
                                <> guidancePairs (guidanceForCell cr)
                                <> errorInfoPairs (errorInfoForCell cr)
                                <> heal
                            )

{- | The @execute_cell@ pre-check: a target id absent from the notebook fails
fast with a clear, id-naming message. Without it the cell is never dispatched,
so no @EvCellResult@ ever broadcasts and 'executeCell' waits out its full
130s timeout before reporting a misleading abort.
-}
missingCellError :: [Cell] -> Int -> Maybe Text
missingCellError cells cid
    | any ((== cid) . cellId) cells = Nothing
    | otherwise = Just ("No cell with id " <> T.pack (show cid))

-- | Outputs an @executeCell@ result carried; @[]@ for an abort 'Left'.
resultOutputs :: Either Text ExecutionResult -> [OutputItem]
resultOutputs (Left _) = []
resultOutputs (Right er) = erOutputs er

-- | The cell's current source, for the before/after self-heal delta.
cellSrc :: App -> Int -> IO Text
cellSrc app cid =
    maybe "" cellSource . lookupCell cid <$> readNotebook (appNotebook app)

{- | Run a cell, and if it fails with an error an obvious fixer can repair — a
missing package (add the @-- cabal: build-depends:@ line) or a missing LANGUAGE
extension (add the pragma) — apply the fix and re-run. Bounded by 'repairCap';
each fixer no-ops once its fix is already present, so a fix that does not help
cannot loop. These are the deterministic repairs GHC's own error names, so the
model never sees the noisy error for them.
-}
executeWithRepair ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    Int ->
    CancelToken ->
    IO (Either Text ExecutionResult)
executeWithRepair app store rn cid cancelTok = do
    res0 <- executeCell app rn cid cancelTok
    staleRef <- newIORef False
    start <- getCurrentTime
    budget <- parseRepairBudget <$> lookupEnv "SABELA_REPAIR_BUDGET_SECS"
    go start budget staleRef repairCap res0
  where
    {- The driver: run the first tier (in 'repairTierOrder') that produces a
    kept result and loop; all declined → restore the notebook if a reverted
    candidate left it stale, and return the original result. -}
    go start budget staleRef n res
        | n <= 0 = restoreIfStale staleRef res
        | otherwise = do
            -- Wall-clock budget, checked per round: the cascade must not eat
            -- the episode's clock — a decline is cheaper than a late heal.
            now <- getCurrentTime
            if realToFrac (diffUTCTime now start) > budget
                then restoreIfStale staleRef res
                else do
                    fired <-
                        firstJustM (\nm -> tierBody nm staleRef res) repairTierOrder
                    case fired of
                        Just (_, newRes) -> go start budget staleRef (n - 1) newRes
                        Nothing -> restoreIfStale staleRef res
    tierBody nm = case nm of
        "firstFix" -> tierFirstFix
        "moduleDep" -> tierModuleDep
        "speculative" -> tierSpeculative
        "resolvers" -> tierResolvers
        "restart" -> tierRestart
        _ -> \_ _ -> pure Nothing
    tierFirstFix staleRef res = do
        nb <- readNotebook (appNotebook app)
        case lookupCell cid nb >>= (firstFix res . cellSource) of
            Just newSrc -> Just <$> applyFresh staleRef newSrc
            Nothing -> pure Nothing
    -- Phase 1 of B1: declare a near-miss module's package via the commit path
    -- (a dep change restarts the kernel, which verify-and-revert cannot
    -- survive); phase 2 renames in the resolvers tier.
    tierModuleDep staleRef res = withCellSrc $ \src -> do
        mDep <- moduleDepStep res src
        traverse (applyFresh staleRef) mDep
    {- Compile-only, NON-COMMITTING type-directed tier — tried BEFORE the
    session-committing name resolvers so a type-correct in-scope fix
    (takeWhile1 → takeWhileP) wins without the keyword resolvers ever running;
    they cross-import wrong packages (attoparsec) and pollute the live session
    on the vetting attempt. Picks by a non-executing :type check; the winner is
    still vetted against the cell's HEALTH (a substitution that :type-checks in
    isolation but trades the error for a type mismatch is reverted). -}
    tierSpeculative staleRef res = withCellSrc $ \src -> do
        -- Arity repair first: it fires only on a function-shaped expected type,
        -- which the hole tiers cannot see (they gate on "not in scope").
        arityCands <- applicationCandidates app res src
        argCands <- argInsertCandidates app res src
        holeSearchCands <- holeSearchCandidates app res src
        (endorsed, lexical) <- holeFitCandidates app store res src
        -- Type-ENDORSED candidates (refinement fits) go straight to the health
        -- vet; only lexical guesses pass the compile-only select — that check
        -- :types an expression outside the cell's scope, so re-checking an
        -- endorsed candidate there can only wrongly destroy it.
        lexWins <-
            selectCleanByTypeCheck
                app
                (arityCands ++ argCands ++ lexical ++ holeSearchCands)
        let wins = endorsed ++ lexWins
        traceSpeculative
            res
            cid
            (arityCands ++ argCands)
            holeSearchCands
            (endorsed ++ lexical)
            (listToMaybe wins)
        verifyAndRevert staleRef res src (take speculativeCap wins)
    tierResolvers staleRef res = withCellSrc $ \src -> do
        modCands <- moduleResolveCandidates app res src
        impCands <- importResolveCandidates app store res src
        ambigCands <- ambiguousResolveCandidates res src
        verifyAndRevert staleRef res src (modCands ++ impCands ++ ambigCands)
    {- Restart-causing tier: verify-and-revert each type-directed / hoogle pick
    (add-dep + scoped import). Committing an unverified pick let junk deps
    persist (e.g. Dhall.Pretty for @Operator@); a kept pick restarts the kernel
    identically, only a bad pick pays one extra restart to shed the dep. -}
    tierRestart staleRef res = withCellSrc $ \src -> do
        typeCands <- typeDiscoverCandidates app store res src
        hoogCands <- hoogleCandidates app store res src
        traceRestart res typeCands hoogCands
        verifyAndRevert staleRef res src (typeCands ++ hoogCands)
    withCellSrc k = do
        mCell <- lookupCell cid <$> readNotebook (appNotebook app)
        maybe (pure Nothing) (k . cellSource) mCell
    -- Commit path: write + run. The stored result is fresh by construction.
    applyFresh staleRef newSrc = do
        modifyNotebook (appNotebook app) (updateCellSource cid newSrc)
        broadcastNotebook app
        writeIORef staleRef False
        executeCell app rn cid cancelTok
    {- A reverted candidate leaves the notebook holding the FAILED candidate's
    result against the restored source — the model then reads an error it never
    caused. One re-run of the reverted source restores the truthful pair;
    costs a single execution, only on the all-declined path. -}
    restoreIfStale staleRef res = do
        stale <- readIORef staleRef
        if stale then executeCell app rn cid cancelTok else pure res
    {- Record what each source produced and which one won, so a null bench result
    can be read: no candidates is a trigger bug, candidates with no winner is a
    selector bug, and a winner that still fails the task kills the idea.

    Only a RED result is traced. The cascade also runs on a green cell (where every
    source declines), and those all-zero lines would drown the signal. -}
    traceSpeculative res' tracedCell arity holeSearch holeFit mWin
        | isClean (healthOfResult res') = pure ()
        | otherwise =
            recordRepair (envWorkDir (appEnv app)) $
                RepairEvent
                    { reCellId = tracedCell
                    , reCounts =
                        [ ("arity", length arity)
                        , ("holeSearch", length holeSearch)
                        , ("holeFit", length holeFit)
                        ]
                    , reWinner = mWin >>= sourceOf
                    }
      where
        sourceOf w
            | w `elem` arity = Just "arity"
            | w `elem` holeSearch = Just "holeSearch"
            | w `elem` holeFit = Just "holeFit"
            | otherwise = Nothing
    -- The restart tier's counter record, same red-only rule as traceSpeculative.
    traceRestart res' typeCands hoogCands
        | isClean (healthOfResult res') = pure ()
        | otherwise =
            recordRepair (envWorkDir (appEnv app)) $
                RepairEvent
                    { reCellId = cid
                    , reCounts =
                        [ ("typeDiscover", length typeCands)
                        , ("hoogle", length hoogCands)
                        ]
                    , reWinner = Nothing
                    }
    {- Try each candidate with APPLY → RE-RUN → KEEP-IFF-IMPROVES, else REVERT
    to priorSrc. 'Just' the kept run for the first candidate whose diagnostics
    are a genuine improvement ('improvesHealthFor': no NEW diagnostic and at
    least one removed, or compiles clean); 'Nothing' when none improve.
    Comparing diagnostic SETS (not counts) rejects a candidate that trades one
    error for another; knock-on scope errors for the cell's own defined names
    are excluded. A revert marks the notebook stale for 'restoreIfStale'. -}
    verifyAndRevert _ _ _ [] = pure Nothing
    verifyAndRevert staleRef res priorSrc (cand : rest)
        -- A candidate that drops a declared name substituted the deliverable
        -- (the signature-rename class); never apply it, however green.
        | not (preservesDeclarations priorSrc cand) =
            verifyAndRevert staleRef res priorSrc rest
    verifyAndRevert staleRef res priorSrc (cand : rest) = do
        modifyNotebook (appNotebook app) (updateCellSource cid cand)
        broadcastNotebook app
        newRes <- executeCell app rn cid cancelTok
        let defined = fst (cellNames priorSrc)
            kept =
                improvesHealthFor
                    defined
                    (healthOfResult res)
                    (healthOfResult newRes)
        debugDumpVerify res newRes kept
        if kept
            then writeIORef staleRef False >> pure (Just newRes)
            else do
                modifyNotebook (appNotebook app) (updateCellSource cid priorSrc)
                broadcastNotebook app
                writeIORef staleRef True
                verifyAndRevert staleRef res priorSrc rest

    -- Verify verdicts appended to the file SABELA_DEBUG_VERIFY names.
    debugDumpVerify oldRes newRes kept = do
        mp <- lookupEnv "SABELA_DEBUG_VERIFY"
        case mp of
            Just p
                | not (null p)
                , p /= "0" ->
                    appendFile p $
                        "verify kept="
                            <> show kept
                            <> "\n  old: "
                            <> show (healthOfResult oldRes)
                            <> "\n  new: "
                            <> show (healthOfResult newRes)
                            <> "\n"
            _ -> pure ()

{- | The repair-tier firing order — the list the driver actually iterates, so
the compile-only speculative tier provably runs before the session-committing
resolvers. Pinned by @Test.RepairEngineSpec@.
-}
repairTierOrder :: [Text]
repairTierOrder =
    ["firstFix", "moduleDep", "speculative", "resolvers", "restart"]

-- | Most automatic deterministic repairs a single run will attempt.
repairCap :: Int
repairCap = 3

-- | Most type-clean speculative candidates one round will execute and vet.
speculativeCap :: Int
speculativeCap = 3

{- | The cascade's wall-clock allowance in seconds, from
@SABELA_REPAIR_BUDGET_SECS@; covers the repair rounds, not the cell's own run.
-}
parseRepairBudget :: Maybe String -> Double
parseRepairBudget m = fromMaybe 150 (m >>= readMaybe)
