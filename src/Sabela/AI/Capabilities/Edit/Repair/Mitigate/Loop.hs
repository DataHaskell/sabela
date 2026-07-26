{-# LANGUAGE OverloadedStrings #-}

{- | G6 tasks 3 and 7: selection by proof, and the round-bounded convergence
loop over "Sabela.AI.Capabilities.Edit.Repair.Mitigate"'s table. See
'foldRow', 'compileFold', and 'runMitigations' for how each part works.
-}
module Sabela.AI.Capabilities.Edit.Repair.Mitigate.Loop (
    MitigationFix (..),
    runMitigations,
    mitigationDisclosure,
) where

import Data.Aeson (Value, object, (.=))
import Data.IORef (IORef)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.Cascade.Commit (verifyAndRevert)
import Sabela.AI.Capabilities.Edit.CompileGate.Render (renderForDiagnostics)
import Sabela.AI.Capabilities.Edit.Repair.Mitigate (
    Discharge (..),
    MitigationRow (..),
    mitigationTable,
    rootErrors,
 )
import Sabela.AI.Health (healthOfResult, isClean)
import Sabela.AI.RepairTrace (RepairEvent (..), recordRepair)
import Sabela.AI.SelfHeal (sourceDelta)
import Sabela.AI.Store (AIStore)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Anthropic.Types (CancelToken)
import Sabela.Errors (parseErrors)
import Sabela.Handlers (ReactiveNotebook)
import Sabela.Model (Cell (..), CellError (..), CellType (..), lookupCell)
import Sabela.Session.Materialize (
    CandidateSpec (..),
    DisposableResult (..),
    DisposableVerdict (..),
    runDisposableTry,
 )
import Sabela.SessionTypes (CellLang (..))
import Sabela.State

-- | One applied mitigation, in the order it was folded into its round.
data MitigationFix = MitigationFix
    { mfClass :: Text
    , mfRemoved :: [Text]
    , mfAdded :: [Text]
    }

{- | Fact-list disclosure for an ambiguous row: every candidate that
independently clears its own diagnostic, named so the model can choose.
-}
factEntry :: Text -> [Text] -> Value
factEntry cls cands = object ["class" .= cls, "compilingCandidates" .= cands]

{- | Non-committing probe, rendered per binder ('renderForDiagnostics') so
independent defects are all visible: GHC stops a block at its first error, and
selection-by-proof needs to see what a fix leaves behind. Deliberately more
permissive than the commit gate, which stays 'renderNonExecuting'.
-}
probeResult ::
    App -> Int -> CellLang -> CellType -> Text -> IO (Either Text ExecutionResult)
probeResult app cid lang ty candidate
    | ty /= CodeCell || lang /= Haskell =
        pure (Right (ExecutionResult [] Nothing [] []))
    | otherwise = do
        result <-
            runDisposableTry
                app
                CandidateSpec
                    { candidateMetadataSource = candidate
                    , candidateSetup = renderForDiagnostics candidate
                    , candidateExpression = Nothing
                    , candidateReplacesCellId = Just cid
                    , -- Diagnosing a REAL cell: judge it as the live session
                      -- will, not under the trial's Safe Haskell.
                      candidateDeliberate = True
                    }
        pure $ case disposableVerdict result of
            DisposableOk -> Right (ExecutionResult [] Nothing [] [])
            _ ->
                let errs = parseErrors (disposableStderr result)
                    holistic = if null errs then Just (disposableStderr result) else Nothing
                 in Right (ExecutionResult [] holistic errs [])

{- | Fold @row@'s fix into @curSrc@: keeps only candidates whose remaining
roots are a SUBSET of "the baseline's, minus this row's target" (task 3).
@missing-extension@ is exempt: unblocking a parse only REVEALS diagnostics.
-}
foldRow ::
    App ->
    AIStore ->
    Int ->
    CellLang ->
    CellType ->
    MitigationRow ->
    Either Text ExecutionResult ->
    Text ->
    IO (Maybe (Text, Text, Int), Maybe Value, Text)
foldRow app store cid lang ty row observed curSrc = do
    baseRes <- probeResult app cid lang ty curSrc
    {- Detection sees the cell's OBSERVED errors too, not only what a fresh
    typecheck reproduces. A runtime diagnostic — @No instance for Show
    Picture@ from GHCi printing an undisplayable result — never appears in a
    typecheck probe, so every row was reachable only for compile-time
    classes and `unshowable-display` could not fire on the very case it was
    written for (live_test28/29).

    Proof is unaffected: `clears` below still requires the candidate to
    TYPECHECK with no new root errors, which is real evidence the rewrite is
    valid. For this class it is also sufficient — the failure was GHCi
    printing a result with no Show instance, and wrapping it in an IO action
    provably removes the print. -}
    let baseRootErrs = rootErrors curSrc baseRes <> observedRoots
        observedRoots =
            [ce | ce <- rootErrors curSrc observed, mitDetect row ce]
        -- The probe's own result, carrying the observed diagnostics too.
        withObserved (Right er) = Right er{erErrors = erErrors er <> observedRoots}
        withObserved other = other
        expected =
            Set.fromList
                [ceMessage ce | ce <- baseRootErrs, not (mitDetect row ce)]
        parseBlocking = mitClass row == "missing-extension"
    if not (any (mitDetect row) baseRootErrs)
        then pure (Nothing, Nothing, curSrc)
        else do
            {- The generator must see what the DETECTOR matched on. It was
            handed the typecheck probe, which is clean for a runtime
            diagnostic, so a row could detect a runtime error and then
            generate nothing — the last link in the unshowable-display
            failure. -}
            cands <- mitGenerate row app store (withObserved baseRes) curSrc
            probed <- mapM (\c -> (,) c <$> probeResult app cid lang ty c) cands
            let clears (c, r) =
                    let newRoots = rootErrors c r
                     in if parseBlocking
                            then not (any (mitDetect row) newRoots)
                            else Set.fromList (map ceMessage newRoots) `Set.isSubsetOf` expected
                cleared = [c | (c, r) <- probed, clears (c, r)]
            pure $ case (mitDischarge row, cleared) of
                (_, []) -> (Nothing, Nothing, curSrc)
                -- A dependency stays the model's deliberate act (G2), so a
                -- proven fix is served as committable source, never applied.
                (ServeAsArtifact, cs) ->
                    (Nothing, Just (factEntry (mitClass row) cs), curSrc)
                (Apply, [one]) ->
                    (Just (mitClass row, one, length cands), Nothing, one)
                (Apply, many) ->
                    (Nothing, Just (factEntry (mitClass row) many), curSrc)

{- | 'foldRow' over every table row, threading the growing composite source;
each step re-probes fresh, so a line-shifting fix earlier in the fold cannot
mistarget a later one.
-}
compileFold ::
    App ->
    AIStore ->
    Int ->
    CellLang ->
    CellType ->
    Either Text ExecutionResult ->
    Text ->
    IO ([Value], [(Text, Text, Int)], Text)
compileFold app store cid lang ty observed src0 = go mitigationTable src0 [] []
  where
    go [] curSrc facts applied = pure (reverse facts, reverse applied, curSrc)
    go (row : rest) curSrc facts applied = do
        (mApplied, mFact, nextSrc) <-
            foldRow app store cid lang ty row observed curSrc
        let facts' = maybe facts (: facts) mFact
            applied' = maybe applied (: applied) mApplied
        go rest nextSrc facts' applied'

{- | The round-bounded convergence loop (G6 task 7). Rounds compose in an
uncommitted buffer, since one fix can unblock the diagnostics the next needs;
G1 still means the notebook only advances when a composite gate-cleans.
-}
runMitigations ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    Int ->
    CancelToken ->
    IORef [Value] ->
    IORef Bool ->
    Int ->
    Either Text ExecutionResult ->
    IO (Maybe (Either Text ExecutionResult), Maybe Value)
runMitigations app store rn cid cancelTok sugRef staleRef k0 res0 = do
    (fixes, facts, finalRes, committedAny, remaining) <-
        go [] [] k0 res0 False Nothing
    pure
        ( if committedAny then Just finalRes else Nothing
        , mitigationDisclosure fixes remaining facts
        )
  where
    currentSrc = maybe "" cellSource . lookupCell cid <$> readNotebook (appNotebook app)

    -- \| The source this round folds over: the uncommitted composite carried
    --    from the previous round when there is one, else the committed cell.
    --
    workingSrc = maybe currentSrc pure

    cellLangType =
        maybe (Haskell, CodeCell) (\c -> (cellLang c, cellType c)) . lookupCell cid
            <$> readNotebook (appNotebook app)
    go fixes facts k res committedAny mWorking
        | isClean (healthOfResult res) =
            pure (reverse fixes, facts, res, committedAny, [])
        | k <= (0 :: Int) = do
            src <- workingSrc mWorking
            pure
                (reverse fixes, facts, res, committedAny, map ceMessage (rootErrors src res))
        | otherwise = do
            src <- workingSrc mWorking
            (lang, ty) <- cellLangType
            (newFacts, applied, composed) <- compileFold app store cid lang ty res src
            if null applied
                then
                    pure
                        ( reverse fixes
                        , facts ++ newFacts
                        , res
                        , committedAny
                        , map ceMessage (rootErrors src res)
                        )
                else do
                    mKept <- verifyAndRevert app rn cancelTok cid sugRef staleRef res src [composed]
                    let stepsList = steps src applied
                        newFixes =
                            [ MitigationFix cls removed added
                            | (cls, before, after, _) <- stepsList
                            , let (removed, added) = sourceDelta before after
                            ]
                    case mKept of
                        -- Every fold step verified genuine progress even though the FULL
                        -- composite could not gate-clean yet (G1's whole-cell invariant).
                        -- Carry it into the next round uncommitted: one round only ever
                        -- sees the diagnostics its predecessors unblocked.
                        Nothing -> do
                            composedRes <- probeResult app cid lang ty composed
                            if composed /= src
                                then
                                    go
                                        (reverse newFixes ++ fixes)
                                        (facts ++ newFacts)
                                        (k - 1)
                                        composedRes
                                        committedAny
                                        (Just composed)
                                else do
                                    let remaining = map ceMessage (rootErrors composed composedRes)
                                    pure
                                        ( reverse fixes ++ newFixes
                                        , facts ++ newFacts
                                        , res
                                        , committedAny
                                        , remaining
                                        )
                        Just newRes -> do
                            mapM_
                                ( \(cls, _, _, n) ->
                                    recordRepair (envWorkDir (appEnv app)) (RepairEvent cid [(cls, n)] (Just cls))
                                )
                                stepsList
                            go (reverse newFixes ++ fixes) (facts ++ newFacts) (k - 1) newRes True Nothing
    steps _ [] = []
    steps before ((cls, after, n) : rest) = (cls, before, after, n) : steps after rest

{- | The disclosed summary: every applied fix in order, fact lists, and an
honest resolved\/total count; @note@ names the first remaining diagnostic
rather than a false "done" when stopped short. 'Nothing' if never touched.
-}
mitigationDisclosure :: [MitigationFix] -> [Text] -> [Value] -> Maybe Value
mitigationDisclosure fixes remaining facts
    | null fixes && null facts = Nothing
    | otherwise =
        Just $
            object
                [ "appliedInOrder" .= map fixJSON fixes
                , "factLists" .= facts
                , "resolved" .= n
                , "total" .= (n + length remaining)
                , "remaining" .= remaining
                , "status" .= (if null remaining then "complete" else "partial" :: Text)
                , "note" .= noteFor
                ]
  where
    n = length fixes
    fixJSON f = object ["class" .= mfClass f, "removed" .= mfRemoved f, "added" .= mfAdded f]
    noteFor
        | null remaining =
            "resolved "
                <> tshow n
                <> " diagnostic"
                <> plural
                <> "; notebook compiles clean."
        | otherwise =
            "resolved "
                <> tshow n
                <> " of "
                <> tshow (n + length remaining)
                <> " diagnostics; "
                <> tshow (n + 1)
                <> " remains: "
                <> fromMaybe "" (listToMaybe remaining)
    plural = if n == 1 then "" else "s" :: Text
    tshow = T.pack . show
