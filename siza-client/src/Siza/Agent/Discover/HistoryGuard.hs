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

import Sabela.AI.Health (Health (..), healthOfTypeQuery)
import Sabela.AI.Types (ToolOutcome (..))
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
 )
import Siza.Agent.Discover.Interpret (envFromCells, parseCells)
import Siza.Agent.Discover.Ledger (
    SearchLedger (..),
    ledgerDeclare,
    ledgerProbe,
    ledgerRefute,
 )
import Siza.Agent.Discover.Resolved (provenNames)
import Siza.Agent.Discover.Types (NotebookEnv (..))
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
                        l3 = maybe l2 (`ledgerRefute` l2) (refusedSource tc o)
                     in (l3, ())
                case provenOf tc o of
                    [] -> pure ()
                    ns -> atomicModifyIORef' ref (\l -> (ledgerResolve ns l, ()))
                case confirmedFactOf tc o of
                    Nothing -> pure ()
                    Just f -> recordProbeFacts ref [f]
            _ -> pure ()
        pure r
    Just q -> do
        shortcut <-
            atomicModifyIORef'
                ref
                (\l -> let (l', out) = ledgerShortcutStep l q in (l', out))
        led <- readIORef ref
        case shortcut of
            Just v -> pure (Right (ToolOk v))
            Nothing -> do
                let facts = heldFacts led
                    goalArgs =
                        injectRecent
                            (factPackages facts)
                            (injectGoal (standingGoal facts) (tcArgs tc))
                r <- inner tc{tcArgs = goalArgs}
                case r of
                    Right (ToolOk v) -> do
                        v' <-
                            atomicModifyIORef'
                                ref
                                (\l -> let (l2, out) = ledgerRecord q v l in (l2, out))
                        pure (Right (ToolOk (boundEnvelope v')))
                    _ -> pure r

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

confirmedFactOf :: ToolCall -> ToolOutcome -> Maybe Text
confirmedFactOf tc o
    | tcName tc == "check_type"
    , ToolOk (Object payload) <- o
    , Just (String res) <- KM.lookup "result" payload
    , healthCompileOk (healthOfTypeQuery res)
    , expr <- T.strip (argText "expr" (tcArgs tc))
    , not (T.null expr) =
        Just (compilerFact expr (signatureOf expr res))
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
