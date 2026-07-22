{- | The IORef seam of the search history ledger: created per episode by the
loop and wrapped around dispatch exactly like the futility guard, so dedup,
the miss ladder and the post-nudge gate see every discover call.
-}
module Siza.Agent.Discover.HistoryGuard (
    newSearchLedger,
    closeSearchLedger,
    closeSearchLedgerRanked,
    guardDiscover,
    heldCallReady,
    seedSearchLedger,
    setSearchPressure,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Set as Set
import Data.Text (Text)

import Sabela.AI.Health (Health (..), healthOfTypeQuery)
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover (
    declaredPackages,
    declaresDepsCall,
    executionSucceeded,
    isOwningTool,
    toolCallSource,
 )
import Siza.Agent.Discover.Envelope (boundEnvelope)
import Siza.Agent.Discover.FactSelect (factContext, selectFacts)
import Siza.Agent.Discover.Goal (injectGoal, standingGoal)
import Siza.Agent.Discover.History (
    SearchLedger,
    callReadyFacts,
    emptyLedger,
    heldFacts,
    ledgerClose,
    ledgerPressure,
    ledgerRecord,
    ledgerResolve,
    ledgerSeed,
    ledgerShortcut,
    ledgerWorldChanged,
    missClusters,
 )
import Siza.Agent.Discover.Interpret (envFromCells, parseCells)
import Siza.Agent.Discover.Ledger (SearchLedger (..), ledgerDeclare)
import Siza.Agent.Discover.Resolved (provenNames)
import Siza.Agent.Discover.Types (NotebookEnv (..))
import Siza.Agent.DiscoverTool (discoverKey)

newSearchLedger :: IO (IORef SearchLedger)
newSearchLedger = newIORef emptyLedger

{- | Seed the assertion ledger at episode start (search-api.md section 11):
the notebook's imported modules and the prompt-documented builtins are
asserted facts from turn 0 — deniable by nothing.
-}
seedSearchLedger ::
    (ToolCall -> IO (Either Text ToolOutcome)) ->
    IORef SearchLedger ->
    IO ()
seedSearchLedger dispatch ref = do
    r <- dispatch (ToolCall "list_cells" (object ["full" .= True]))
    let cells = parseCells (payloadOf r)
        env = envFromCells cells
        declared = concatMap (declaredPackages . fst) cells
    atomicModifyIORef' ref $ \l ->
        (ledgerDeclare declared (ledgerSeed (seedFacts env) l), ())
  where
    payloadOf :: Either Text ToolOutcome -> Value
    payloadOf (Right (ToolOk v)) = v
    payloadOf _ = object []

-- | The turn-0 facts of an environment: imports, aliases and builtins.
seedFacts :: NotebookEnv -> [Text]
seedFacts env =
    map fst (neImportCells env)
        ++ map snd (neAliases env)
        ++ neBuiltins env
        ++ neBuiltinModules env

-- | Close the ledger and return the held facts for the nudge (R5.7).
closeSearchLedger :: IORef SearchLedger -> IO [Text]
closeSearchLedger ref =
    atomicModifyIORef' ref (\led -> (ledgerClose led, heldFacts led))

{- | 'closeSearchLedger' with the R7-T4 ranked selection: the returned facts
are filtered by the deliverable's relevance key (goal text, cell sources,
the ledger's own active miss-clusters) — the full ledger stays as history.
-}
closeSearchLedgerRanked :: Text -> [Text] -> IORef SearchLedger -> IO [Text]
closeSearchLedgerRanked goal cells ref =
    atomicModifyIORef' ref $ \led ->
        ( ledgerClose led
        , selectFacts
            (factContext goal cells (missClusters led))
            (heldFacts led)
        )

-- | Does the ledger hold a call-ready (name + signature) fact (R5.6)?
heldCallReady :: IORef SearchLedger -> IO Bool
heldCallReady ref = not . null . callReadyFacts <$> readIORef ref

-- | The loop's per-turn budget-pressure seam ('ledgerPressure').
setSearchPressure :: IORef SearchLedger -> Int -> IO ()
setSearchPressure ref n = atomicModifyIORef' ref (\l -> (ledgerPressure n l, ()))

{- | Wrap dispatch: discover calls flow through the ledger (dedup, ladder,
close gate) keyed on query PLUS knobs ('discoverKey' — a re-scoped repeat is
not a duplicate); an install or restart bumps the world generation. The wipe
runs BEFORE 'provenOf' records, so a dep-declaring landed write's own
compiler proof survives the world change it triggers (section 3.3, R7-T1).
-}
guardDiscover ::
    IORef SearchLedger ->
    (ToolCall -> IO (Either Text ToolOutcome)) ->
    ToolCall ->
    IO (Either Text ToolOutcome)
guardDiscover ref inner tc = case discoverKey (tcName tc) (tcArgs tc) of
    Nothing -> do
        r <- inner tc
        case r of
            Right o -> do
                atomicModifyIORef' ref $ \l ->
                    let l1 = if worldChanging l tc o then ledgerWorldChanged l else l
                        l2
                            | isOwningTool (tcName tc)
                            , executionSucceeded o =
                                ledgerDeclare (declaredPackages (toolCallSource tc)) l1
                            | otherwise = l1
                     in (l2, ())
                case provenOf tc o of
                    [] -> pure ()
                    ns -> atomicModifyIORef' ref (\l -> (ledgerResolve ns l, ()))
            _ -> pure ()
        pure r
    Just q -> do
        led <- readIORef ref
        case ledgerShortcut led q of
            Just v -> pure (Right (ToolOk v))
            Nothing -> do
                -- The standing goal rides the call's arguments (section 8.3),
                -- so ledger provenance reaches producer ranking downstream.
                let goalArgs = injectGoal (standingGoal (heldFacts led)) (tcArgs tc)
                r <- inner tc{tcArgs = goalArgs}
                case r of
                    Right (ToolOk v) -> do
                        v' <-
                            atomicModifyIORef'
                                ref
                                (\l -> let (l2, out) = ledgerRecord q v l in (l2, out))
                        -- R3.9: record annotations (worldChange, steer) must
                        -- not push a bounded answer back over the budget.
                        pure (Right (ToolOk (boundEnvelope v')))
                    _ -> pure r

{- | Names this call's outcome compiler-proved (R7-T1): a clean check_type
resolution proves its expression's names; a landed compile proves the cell's
identifier tokens — the type checker outranks the lexical index for both.
-}
provenOf :: ToolCall -> ToolOutcome -> [Text]
provenOf tc o
    | tcName tc == "check_type"
    , ToolOk (Object payload) <- o
    , Just (String res) <- KM.lookup "result" payload
    , healthCompileOk (healthOfTypeQuery res) =
        provenNames (argText "expr" (tcArgs tc))
    | isOwningTool (tcName tc)
    , executionSucceeded o =
        provenNames (toolCallSource tc)
    | otherwise = []

argText :: Text -> Value -> Text
argText k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
argText _ _ = ""

{- | A call that changes the searchable world: restart, or a landed write
declaring a package the ledger has not seen declared — re-declaring an
already-declared dep installs nothing (the R1.4 spurious-banner class).
-}
worldChanging :: SearchLedger -> ToolCall -> ToolOutcome -> Bool
worldChanging led tc o = case tcName tc of
    "kernel_restart" -> isOk o
    n ->
        isOwningTool n
            && declaresDepsCall tc
            && executionSucceeded o
            && any
                (`Set.notMember` slDeclaredPkgs led)
                (declaredPackages (toolCallSource tc))
  where
    isOk (ToolOk _) = True
    isOk _ = False
