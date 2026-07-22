{-# LANGUAGE OverloadedStrings #-}

{- | The agent's red-cell repair: ONE dispatcher keyed on the GHC diagnostic
class feeds tier candidates through verify-and-revert under the notebook-scope
acceptance law, reporting one bounded line — never rejected samples (R7.5-7.7).
-}
module Siza.Agent.Repair (
    Dispatch,
    substituteAndVerify,
    repairRedCells,
    repairOne,
    compiled,
    snapshot,
) where

import Control.Monad (unless, void)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.CellResult (CellId)
import Sabela.AI.Health (Health, healthOfCellError, isClean)
import Sabela.AI.HoleRepair (
    goalFromError,
    holeFitRewrites,
    holeTypeFromDiagnostic,
 )
import Sabela.AI.Repair (firstJustM)
import Sabela.AI.RepairDispatch (
    DiagClass,
    RepairReport (..),
    RepairTier (..),
    acceptRepair,
    classifyDiag,
    renderRepairReport,
    tierRequiresRestart,
    tiersFor,
 )
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.RepairGuard (goalFromErrorInCell, selfDeclaredSigs)
import Siza.Agent.RepairLocate (discoverModules, discoverRenames)
import Siza.Agent.RepairTiers (
    Candidate (..),
    TierInput (..),
    candidatesFor,
 )

type Dispatch = ToolCall -> IO (Either Text ToolOutcome)

-- | Candidate vets per red cell before the cascade reports and stops.
repairBudget :: Int
repairBudget = 4

repairRedCells ::
    Dispatch -> [(CellId, Text)] -> IO [(ToolCall, Either Text ToolOutcome)]
repairRedCells disp =
    fmap catMaybes . mapM (uncurry (repairOne disp))

-- | Dispatch one red cell by its diagnostic class through the tier cascade.
repairOne ::
    Dispatch -> CellId -> Text -> IO (Maybe (ToolCall, Either Text ToolOutcome))
repairOne disp cid errText = case tiersFor cls of
    [] -> pure Nothing
    tiers -> do
        msrc <- readCellSource disp cid
        case msrc of
            Nothing -> pure Nothing
            Just src -> do
                input <- buildInput disp errText src tiers
                case take repairBudget (candidatesFor tiers input) of
                    [] -> pure Nothing
                    cands -> do
                        before <- snapshot disp
                        runCascade disp cls cid src before cands
  where
    cls = classifyDiag errText

-- | The tier resources: hole fits and the discover-backed locators (R7.6).
buildInput :: Dispatch -> Text -> Text -> [RepairTier] -> IO TierInput
buildInput disp errText src tiers = do
    let holeGoal = case goalFromErrorInCell (selfDeclaredSigs src) errText of
            Just (_, ty) -> Just ty
            Nothing -> holeTypeFromDiagnostic errText
    blob <- case holeGoal of
        Just ty | TierHoleFit `elem` tiers -> queryHoleFits disp ty
        _ -> pure ""
    located <-
        if TierAddImport `elem` tiers
            then discoverModules disp errText
            else pure []
    mods <-
        if TierModuleRename `elem` tiers
            then discoverRenames disp errText
            else pure []
    let locate n = [(m, p) | (w, m, p) <- located, w == n]
        modLocate w = [m | (w0, m) <- mods, w0 == w]
    pure (TierInput errText src blob locate modLocate)

{- | Try each candidate; keep the first the acceptance law admits, else
restore the original and report attempted-and-reverted (R7.5). A restart-
requiring candidate is applied and disclosed as unvalidated (R7.3).
-}
runCascade ::
    Dispatch ->
    DiagClass ->
    CellId ->
    Text ->
    [(Text, Health, [Text])] ->
    [Candidate] ->
    IO (Maybe (ToolCall, Either Text ToolOutcome))
runCascade disp cls cid src before cands = go 1 cands
  where
    key = T.pack (show cid)
    budget = length cands
    defined =
        Set.fromList (concat [ds | (k, _, ds) <- before, k == key])
    healths snaps = [(k, h) | (k, h, _) <- snaps]
    lawApplies = any (\(k, _, _) -> k == key) before
    report n = RepairReport cls n budget
    go n (c : rest)
        -- R7.3/R7.5: a restart-requiring repair is applied then RE-CHECKED
        -- against the post-restart snapshot — confirmed when the target is
        -- clean, else flagged kept-but-unconfirmed (never silently red).
        | tierRequiresRestart (cdTier c) = do
            let call = replaceCall cid (cdSource c)
            out <- disp call
            after <- snapshot disp
            let kept = Just (cdTier c, firstLine (cdSource c))
                confirmed = maybe False isClean (lookup key (healths after))
                rep
                    | confirmed =
                        report n "kept; post-restart re-check: cell clean" kept []
                    | otherwise =
                        report
                            n
                            "kept-but-unconfirmed: cell still red after the \
                            \restart re-check"
                            kept
                            [firstLine (cdSource c)]
            pure (Just (call, withReport rep out))
        | otherwise = do
            let call = replaceCall cid (cdSource c)
            out <- disp call
            after <- snapshot disp
            let ok
                    | lawApplies =
                        acceptRepair defined (healths before) (healths after) key
                    | otherwise = compiled out
            if ok
                then do
                    let rep =
                            report n "kept" (Just (cdTier c, firstLine (cdSource c))) []
                    pure (Just (call, withReport rep out))
                else go (n + 1) rest
    go _ [] = do
        let call = replaceCall cid src
        out <- disp call
        let rep = report budget "all candidates reverted" Nothing []
        pure (Just (call, withReport rep out))

-- | Attach the bounded repair report to the surviving outcome (R7.7).
withReport ::
    RepairReport -> Either Text ToolOutcome -> Either Text ToolOutcome
withReport rep (Right (ToolOk (Object o))) =
    Right
        ( ToolOk
            (Object (KM.insert "repair" (String (renderRepairReport rep)) o))
        )
withReport _ out = out

firstLine :: Text -> Text
firstLine t = case T.lines t of
    (l : _) -> l
    [] -> t

{- | The notebook health snapshot the acceptance law compares: every cell's
(id, health, defines) from one preview listing, error text fetched only for
red cells (bounded by the red count).
-}
snapshot :: Dispatch -> IO [(Text, Health, [Text])]
snapshot disp = do
    out <- disp (ToolCall "list_cells" (object []))
    case out of
        Right (ToolOk v) -> mapM enrich (cellRows v)
        _ -> pure []
  where
    enrich (cid, hasErr, defs)
        | not hasErr = pure (cid, healthOfCellError Nothing, defs)
        | otherwise = do
            r <- disp (ToolCall "read_cell" (object ["cell_id" .= cid]))
            let err = case r of
                    Right (ToolOk (Object o)) -> lookupText "error" o
                    _ -> "unreadable red cell"
            pure (cid, healthOfCellError (Just err), defs)
    cellRows (Object o)
        | Just (Array cells) <- KM.lookup "cells" o =
            [ (idText c, boolAt "hasError" c, definesOf c)
            | Object c <- toList cells
            ]
    cellRows _ = []
    idText c = case KM.lookup "id" c of
        Just (Number n) -> T.pack (show (round n :: Int))
        Just (String s) -> s
        _ -> ""
    boolAt k c = KM.lookup (K.fromText k) c == Just (Bool True)
    definesOf c = case KM.lookup "defines" c of
        Just (Array ds) -> [d | String d <- toList ds]
        _ -> []

{- | Repair a red cell from GHC hole-fits: try each plain fit for the goal the
error implies, keeping the first whose re-run compiles (verify-and-backtrack, not
first-fit), else restore the original. Refinement skeletons are left alone.
-}
substituteAndVerify ::
    Dispatch -> CellId -> Text -> IO (Maybe (ToolCall, Either Text ToolOutcome))
substituteAndVerify disp cid errText = case goalFromError errText of
    Nothing -> pure Nothing
    Just (wrong, ty) -> do
        blob <- queryHoleFits disp ty
        msrc <- readCellSource disp cid
        case msrc of
            Nothing -> pure Nothing
            -- The self-declared guard: a name the cell defines here is not a
            -- producer-hunt target (the topMonth false goal).
            Just src
                | wrong `elem` selfDeclaredSigs src -> pure Nothing
                | otherwise -> do
                    let subs = holeFitRewrites wrong blob src
                    hit <- firstJustM (verifyReplace disp cid) subs
                    case hit of
                        Just (_, (call, out)) -> pure (Just (call, out))
                        Nothing -> do
                            unless (null subs) (void (disp (replaceCall cid src)))
                            pure Nothing

{- | Replace the cell with a candidate source and keep the call + outcome only
when the re-run compiled, so the search backtracks past a non-compiling fit.
-}
verifyReplace ::
    Dispatch ->
    CellId ->
    Text ->
    IO (Maybe (ToolCall, Either Text ToolOutcome))
verifyReplace disp cid newSrc = do
    let call = replaceCall cid newSrc
    out <- disp call
    pure (if compiled out then Just (call, out) else Nothing)

-- | True when a cell-execution outcome reports a successful run (@execution.ok@).
compiled :: Either Text ToolOutcome -> Bool
compiled (Right (ToolOk (Object o))) = case KM.lookup "execution" o of
    Just (Object e) -> KM.lookup "ok" e == Just (Bool True)
    _ -> False
compiled _ = False

queryHoleFits :: Dispatch -> Text -> IO Text
queryHoleFits disp ty = do
    out <-
        disp
            ( ToolCall
                "find_by_type"
                (object ["goal" .= ("_ :: " <> ty)])
            )
    pure (strField "result" out)

readCellSource :: Dispatch -> CellId -> IO (Maybe Text)
readCellSource disp cid = do
    out <- disp (ToolCall "read_cell" (object ["cell_id" .= cid]))
    pure $ case out of
        Right (ToolOk (Object o)) -> Just (lookupText "source" o)
        _ -> Nothing

replaceCall :: CellId -> Text -> ToolCall
replaceCall cid src =
    ToolCall "replace_cell_source" (object ["cell_id" .= cid, "new_source" .= src])

strField :: Text -> Either Text ToolOutcome -> Text
strField k (Right (ToolOk (Object o))) = lookupText k o
strField _ _ = ""

lookupText :: Text -> KM.KeyMap Value -> Text
lookupText k o = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
