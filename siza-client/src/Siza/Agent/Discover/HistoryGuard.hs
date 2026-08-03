module Siza.Agent.Discover.HistoryGuard (
    newSearchLedger,
    closeSearchLedger,
    closeSearchLedgerRanked,
    guardDiscover,
    heldCallReady,
    recordProbeFacts,
    seedSearchLedger,
    setSearchPressure,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.AnswerSource (viaIsCompilerBacked)
import Sabela.AI.Health (Health (..), healthOfTypeQuery)
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.AI.WriteAck (
    AckEnvelope (..),
    WriteAck (..),
    parseAckEnvelope,
 )
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover (
    declaredPackages,
    declaresDepsCall,
    executionSucceeded,
    isOwningTool,
    refusedSource,
    toolCallSource,
 )
import Siza.Agent.Discover.Dedup (ledgerShortcutStep)
import Siza.Agent.Discover.Envelope (boundEnvelope)
import Siza.Agent.Discover.FactSelect (factContext, selectFacts)
import Siza.Agent.Discover.Facts (compilerFact, factPackages)
import Siza.Agent.Discover.Goal (injectGoal, injectRecent, standingGoal)
import Siza.Agent.Discover.GoalEscalate (spendEscalation)
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
    ledgerWorldChanged,
    missClusters,
    takeEscalation,
 )
import Siza.Agent.Discover.Interpret (envFromCells, parseCells)
import Siza.Agent.Discover.Ledger (
    SearchLedger (..),
    ledgerDeclare,
    ledgerProbe,
    ledgerRefute,
    refutedBefore,
 )
import Siza.Agent.Discover.Resolved (provenNames)
import Siza.Agent.Discover.Types (NotebookEnv (..), StandingGoal)
import Siza.Agent.DiscoverTool (discoverKey)

newSearchLedger :: IO (IORef SearchLedger)
newSearchLedger = newIORef emptyLedger

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

seedFacts :: NotebookEnv -> [Text]
seedFacts env =
    map fst (neImportCells env)
        ++ map snd (neAliases env)
        ++ neBuiltins env
        ++ neBuiltinModules env

closeSearchLedger :: IORef SearchLedger -> IO [Text]
closeSearchLedger ref =
    atomicModifyIORef' ref (\led -> (ledgerClose led, heldFacts led))

closeSearchLedgerRanked :: Text -> [Text] -> IORef SearchLedger -> IO [Text]
closeSearchLedgerRanked goal cells ref =
    atomicModifyIORef' ref $ \led ->
        ( ledgerClose led
        , selectFacts
            (factContext goal cells (missClusters led))
            (heldFacts led)
        )

recordProbeFacts :: IORef SearchLedger -> [Text] -> IO ()
recordProbeFacts ref fs = atomicModifyIORef' ref (\l -> (ledgerProbe fs l, ()))

heldCallReady :: IORef SearchLedger -> IO Bool
heldCallReady ref = not . null . callReadyFacts <$> readIORef ref

setSearchPressure :: IORef SearchLedger -> Int -> IO ()
setSearchPressure ref n = atomicModifyIORef' ref (\l -> (ledgerPressure n l, ()))

guardDiscover ::
    IORef SearchLedger ->
    (ToolCall -> IO (Either Text ToolOutcome)) ->
    ToolCall ->
    IO (Either Text ToolOutcome)
guardDiscover ref inner tc = case discoverKey (tcName tc) (tcArgs tc) of
    Nothing -> do
        before <- readIORef ref
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
                        l3 = case refusedSource tc o of
                            Just src -> ledgerRefute src (diagnosticOf o) l2
                            Nothing -> l2
                     in (l3, ())
                case provenOf tc o of
                    [] -> pure ()
                    ns -> atomicModifyIORef' ref (\l -> (ledgerResolve ns l, ()))
                case confirmedFactOf tc o of
                    Nothing -> pure ()
                    Just f -> recordProbeFacts ref [f]
                pure (Right (noteRepeat before tc o))
            _ -> pure r
    Just q -> do
        shortcut <-
            atomicModifyIORef'
                ref
                (\l -> let (l', out) = ledgerShortcutStep l q in (l', out))
        led <- readIORef ref
        case shortcut of
            Just v -> do
                mEsc <- atomicModifyIORef' ref takeEscalation
                v' <- spendPending ref inner (factPackages (heldFacts led)) q mEsc v
                pure (Right (ToolOk (boundEnvelope v')))
            Nothing -> do
                let facts = heldFacts led
                    pkgs = factPackages facts
                    goalArgs =
                        injectRecent
                            pkgs
                            (injectGoal (standingGoal facts) (tcArgs tc))
                r <- inner tc{tcArgs = goalArgs}
                case r of
                    Right (ToolOk v) -> do
                        (v', mEsc) <-
                            atomicModifyIORef' ref $ \l ->
                                let (l2, out) = ledgerRecord q v l
                                    (l3, esc) = takeEscalation l2
                                 in (l3, (out, esc))
                        v'' <- spendPending ref inner pkgs q mEsc v'
                        pure (Right (ToolOk (boundEnvelope v'')))
                    _ -> pure r

{- | Spend whatever escalation the rung armed, whichever rung armed it. Both
the miss rung and the dedup rung reach the type query through here, so the one
query per goal cluster is spent and disclosed the same way on either.
-}
spendPending ::
    IORef SearchLedger ->
    (ToolCall -> IO (Either Text ToolOutcome)) ->
    [Text] ->
    Text ->
    Maybe StandingGoal ->
    Value ->
    IO Value
spendPending _ _ _ _ Nothing v = pure v
spendPending ref dispatch pkgs q (Just sg) v =
    spendEscalation ref dispatch sg pkgs q v

{- | C1-17b: a write whose source was already refused, whitespace aside, says
so on its own payload. What happened this time is read off the write
acknowledgement, never assumed from the outcome constructor.
-}
noteRepeat :: SearchLedger -> ToolCall -> ToolOutcome -> ToolOutcome
noteRepeat led tc o
    | not (isOwningTool (tcName tc)) = o
    | Just earlier <- refutedBefore led (toolCallSource tc) = stamped earlier
    | otherwise = o
  where
    stamped earlier = case o of
        ToolOk v -> ToolOk (setNote v (refusedBase <> ackClause v earlier))
        ToolErr v -> ToolErr (setNote v (refusedBase <> ackClause v earlier))
    ackClause v earlier = case parseAckEnvelope v of
        Just (EnvWrite wa) ->
            "; it committed this time as cell " <> T.pack (show (waCellId wa))
        Just (EnvRefusal _) -> diagClause earlier
        _ -> diagClauseWhenDiagnostic earlier
    diagClauseWhenDiagnostic earlier
        | T.null (diagnosticOf o) = ""
        | otherwise = diagClause earlier
    diagClause earlier =
        "; the diagnostic is "
            <> (if earlier == diagnosticOf o then "unchanged" else "changed")
    setNote (Object m) note =
        Object (KM.insert "repeatOf" (String note) m)
    setNote v _ = v

refusedBase :: Text
refusedBase = "this source was refused earlier (whitespace aside)"

diagnosticOf :: ToolOutcome -> Text
diagnosticOf o = case o of
    ToolErr (Object m) -> textAt m
    ToolOk (Object m) -> textAt m
    _ -> ""
  where
    textAt m = case KM.lookup "diagnostic" m of
        Just (String s) -> s
        _ -> ""

provenOf :: ToolCall -> ToolOutcome -> [Text]
provenOf tc o
    | Just _ <- compilerAnswer tc o =
        provenNames (argText "expr" (tcArgs tc))
    | isOwningTool (tcName tc)
    , executionSucceeded o =
        provenNames (toolCallSource tc)
    | otherwise = []

confirmedFactOf :: ToolCall -> ToolOutcome -> Maybe Text
confirmedFactOf tc o
    | Just res <- compilerAnswer tc o
    , expr <- T.strip (argText "expr" (tcArgs tc))
    , not (T.null expr) =
        Just (compilerFact expr (signatureOf expr res))
    | otherwise = Nothing

{- | A check_type result a compiler stood behind. An index answer proves the
world holds the name; only a session answer proves this session's scope, so
only a session answer may resolve a name against later denial.
-}
compilerAnswer :: ToolCall -> ToolOutcome -> Maybe Text
compilerAnswer tc o
    | tcName tc == "check_type"
    , ToolOk (Object payload) <- o
    , Just (String via) <- KM.lookup "via" payload
    , viaIsCompilerBacked via
    , Just (String res) <- KM.lookup "result" payload
    , healthCompileOk (healthOfTypeQuery res) =
        Just res
    | otherwise = Nothing

signatureOf :: Text -> Text -> Text
signatureOf expr res = case T.breakOn " :: " firstLine of
    (lhs, rest)
        | not (T.null rest)
        , T.strip lhs == expr || T.null (T.strip lhs) ->
            T.strip (T.drop 4 rest)
    _ -> T.strip firstLine
  where
    firstLine = fromMaybe res (listToMaybe (T.lines res))

argText :: Text -> Value -> Text
argText k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
argText _ _ = ""

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
