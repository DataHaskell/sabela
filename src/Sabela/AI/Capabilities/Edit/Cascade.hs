{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | G2: self_heal's propose-verify-disclose repair cascade. A cell that just
ran and failed may be repaired by up to 'repairCap' rounds of candidate
rewrites (arity, hole search/fit, module rename, add-import, hoogle,
type-discover, path near-miss). Every candidate is gate-checked
("Sabela.AI.Capabilities.Edit.Cascade.Commit") BEFORE it may ever touch the
notebook: a rewrite that does not compile is never committed (no more
commit-then-revert), and a rewrite that only compiles by adding a dependency
the model never wrote is downgraded to a disclosed suggestion, never applied
(G2 hard rules 1 and 2). A cell holding a typed-hole probe (@_ :: T@, G3's
deliverable diagnostic) is never touched at all (hard rule 3).
-}
module Sabela.AI.Capabilities.Edit.Cascade (
    executeWithRepair,
    parseRepairBudget,
    repairTierOrder,
) where

import Data.Aeson (Value)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Time (diffUTCTime, getCurrentTime)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Sabela.AI.Capabilities.Edit.Assemble (applicationCandidates)
import Sabela.AI.Capabilities.Edit.Cascade.Commit (
    applyFresh,
    proposeDependency,
    restoreIfStale,
    verifyAndRevert,
 )
import Sabela.AI.Capabilities.Edit.Exec (executeCell)
import Sabela.AI.Capabilities.Edit.HoleSearch (
    argInsertCandidates,
    holeFitCandidates,
    holeSearchCandidates,
 )
import Sabela.AI.Capabilities.Edit.Repair (
    dependencySuggestion,
    firstFix,
    moduleDepStep,
 )
import Sabela.AI.Capabilities.Edit.Repair.Mitigate.Loop (runMitigations)
import Sabela.AI.Capabilities.Edit.Repair.Resolvers (
    hoogleCandidates,
    moduleResolveCandidates,
    pathNearMissCandidates,
    typeDiscoverCandidates,
 )
import Sabela.AI.Capabilities.Edit.TypeSelect (selectCleanByTypeCheck)
import Sabela.AI.Health (healthOfResult, isClean)
import Sabela.AI.Repair (firstJustM)
import Sabela.AI.RepairTrace (RepairEvent (..), recordRepair)
import Sabela.AI.Store (AIStore)
import Sabela.AI.TypedHole (containsTypedHole)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Anthropic.Types (CancelToken)
import Sabela.Handlers (ReactiveNotebook)
import Sabela.Model
import Sabela.State

{- | Run a cell, and on failure run the propose-verify-disclose repair
cascade: each round tries the first tier ('repairTierOrder') that commits a
gate-verified, health-improving rewrite. A cell holding a typed-hole probe is
never entered — it passes through untouched. Returns the settled result, every
gate-verified dependency suggestion the cascade declined to apply, and (G6)
the mitigation-table disclosure when the diagnostic-class loop ever fired.
-}
executeWithRepair ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    Int ->
    CancelToken ->
    IO (Either Text ExecutionResult, [Value], Maybe Value)
executeWithRepair app store rn cid cancelTok = do
    res0 <- executeCell app rn cid cancelTok
    mCell0 <- lookupCell cid <$> readNotebook (appNotebook app)
    staleRef <- newIORef False
    sugRef <- newIORef []
    mitigateRef <- newIORef Nothing
    start <- getCurrentTime
    budget <- parseRepairBudget <$> lookupEnv "SABELA_REPAIR_BUDGET_SECS"
    final <-
        if maybe False (containsTypedHole . cellSource) mCell0
            then pure res0
            else go start budget staleRef sugRef mitigateRef repairCap res0
    suggestions <- reverse <$> readIORef sugRef
    mitigations <- readIORef mitigateRef
    pure (final, suggestions, mitigations)
  where
    {- The driver: run the first tier (in 'repairTierOrder') that produces a
    kept result and loop; all declined → restore the notebook if a reverted
    candidate left it stale, and return the original result. -}
    go start budget staleRef sugRef mitigateRef n res
        | n <= 0 = restoreIfStale app rn cancelTok cid staleRef res
        | otherwise = do
            -- Wall-clock budget, checked per round: the cascade must not eat
            -- the episode's clock — a decline is cheaper than a late heal.
            now <- getCurrentTime
            if realToFrac (diffUTCTime now start) > budget
                then restoreIfStale app rn cancelTok cid staleRef res
                else do
                    fired <-
                        firstJustM
                            (\nm -> tierBody nm staleRef sugRef mitigateRef res)
                            repairTierOrder
                    case fired of
                        Just (_, newRes) ->
                            go start budget staleRef sugRef mitigateRef (n - 1) newRes
                        Nothing -> restoreIfStale app rn cancelTok cid staleRef res
    tierBody nm = case nm of
        "mitigate" -> tierMitigate
        "firstFix" -> tierFirstFix
        "moduleDep" -> tierModuleDep
        "speculative" -> tierSpeculative
        "resolvers" -> tierResolvers
        "restart" -> tierRestart
        _ -> \_ _ _ _ -> pure Nothing
    -- \| G6: the diagnostic-class mitigation table, tried first — it owns its
    --    own bounded round loop (up to 'mitigationRoundCap'), so by the time this
    --    tier declines the OTHER tiers see either a clean cell or diagnostics no
    --    table row claims. Fires once per outer round like every other tier; its
    --    disclosure (the ordered fix chain, any fact lists, the honest remainder)
    --    is captured into @mitigateRef@ for 'executeWithRepair' to return.
    tierMitigate staleRef sugRef mitigateRef res = do
        (result, disclosure) <-
            runMitigations app store rn cid cancelTok sugRef staleRef mitigationRoundCap res
        case disclosure of
            Just v -> writeIORef mitigateRef (Just v)
            Nothing -> pure ()
        pure result
    tierFirstFix staleRef sugRef _mitigateRef res = do
        nb <- readNotebook (appNotebook app)
        case lookupCell cid nb of
            Nothing -> pure Nothing
            Just c -> do
                let src = cellSource c
                -- Independent of whether an auto-appliable fixer wins below:
                -- a dependency-only fix is always disclosed as a suggestion,
                -- never applied (G2 hard rule 1).
                proposeDependency app cid sugRef src (dependencySuggestion res src)
                case firstFix res src of
                    Just newSrc -> applyFresh app rn cancelTok cid staleRef src newSrc
                    Nothing -> pure Nothing
    -- Phase 1 of B1: propose a near-miss module's package (G2: suggestion
    -- only, never committed — a dep change restarts the kernel and a new
    -- dependency is never an automatic act); phase 2 renames in 'tierResolvers'.
    tierModuleDep staleRef sugRef _mitigateRef res = withCellSrc $ \src -> do
        mDep <- moduleDepStep res src
        verifyAndRevert
            app
            rn
            cancelTok
            cid
            sugRef
            staleRef
            res
            src
            (maybe [] pure mDep)
    {- Compile-only, NON-COMMITTING type-directed tier — tried BEFORE the
    session-committing name resolvers so a type-correct in-scope fix
    (takeWhile1 → takeWhileP) wins without the keyword resolvers ever running;
    they cross-import wrong packages (attoparsec) and pollute the live session
    on the vetting attempt. Picks by a non-executing :type check; the winner is
    still gate- and HEALTH-checked (a substitution that :type-checks in
    isolation but trades the error for a type mismatch is never applied). -}
    tierSpeculative staleRef sugRef _mitigateRef res = withCellSrc $ \src -> do
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
        verifyAndRevert
            app
            rn
            cancelTok
            cid
            sugRef
            staleRef
            res
            src
            (take speculativeCap wins)
    -- Ambiguous-occurrence and add-import are now G6 mitigation-table rows
    -- (selection-by-proof, via 'tierMitigate'); this tier keeps only the
    -- resolvers the table does not own, so neither fires twice under two
    -- different selection laws.
    tierResolvers staleRef sugRef _mitigateRef res = withCellSrc $ \src -> do
        modCands <- moduleResolveCandidates app res src
        pathCands <- pathNearMissCandidates app res src
        verifyAndRevert
            app
            rn
            cancelTok
            cid
            sugRef
            staleRef
            res
            src
            (modCands ++ pathCands)
    {- Restart-causing tier: every candidate here adds a dependency by
    construction, so 'verifyAndRevert''s classify demotes each to a disclosed
    suggestion — this tier never commits (G2 hard rule 1). -}
    tierRestart staleRef sugRef _mitigateRef res = withCellSrc $ \src -> do
        typeCands <- typeDiscoverCandidates app store res src
        hoogCands <- hoogleCandidates app store res src
        traceRestart res typeCands hoogCands
        verifyAndRevert
            app
            rn
            cancelTok
            cid
            sugRef
            staleRef
            res
            src
            (typeCands ++ hoogCands)
    withCellSrc k = do
        mCell <- lookupCell cid <$> readNotebook (appNotebook app)
        maybe (pure Nothing) (k . cellSource) mCell
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

{- | The repair-tier firing order — the list the driver actually iterates, so
the compile-only speculative tier provably runs before the session-committing
resolvers, and G6's mitigation table runs before anything else (a parse-
blocking missing extension must clear before any other tier can see past it).
Pinned by @Test.RepairEngineSpec@.
-}
repairTierOrder :: [Text]
repairTierOrder =
    ["mitigate", "firstFix", "moduleDep", "speculative", "resolvers", "restart"]

-- | Most automatic deterministic repairs a single run will attempt.
repairCap :: Int
repairCap = 3

{- | G6 task 7's K: most rounds the mitigation table's OWN internal
convergence loop will run before disclosing an honest partial result. Five
clears the documented worst case (a cell with four independent defects, one
per round) with one round to spare.
-}
mitigationRoundCap :: Int
mitigationRoundCap = 5

-- | Most type-clean speculative candidates one round will execute and vet.
speculativeCap :: Int
speculativeCap = 3

{- | The cascade's wall-clock allowance in seconds, from
@SABELA_REPAIR_BUDGET_SECS@; covers the repair rounds, not the cell's own run.
-}
parseRepairBudget :: Maybe String -> Double
parseRepairBudget m = fromMaybe 150 (m >>= readMaybe)
