{- | The per-call machinery both entry points share: the chat agent loop and
the stdio MCP server. Everything here works from a 'Dispatch' alone, so a
layer added to it lands on both paths rather than on whichever one was edited.

The loop keeps what needs the model or the transcript — sampling, the emit
ledger, wrap-up, compaction. Those cannot cross, and do not live here.
-}
module Siza.Agent.Stack (
    Dispatch,
    Surface (..),
    SurfacePolicy (..),
    surfacePolicy,
    sessionPolicy,
    ssHealReds,
    StackSession (..),
    newSessionFor,
    newStackSession,
    stackDispatch,
    stackLayers,
    takeGoal,
    sessionGoal,
    setGoal,
    recordCall,
    ownedCells,
    ownedReds,
    sessionRejectionRepeats,
    markDelivered,
    isDelivered,
    freshNote,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (
    IORef,
    atomicModifyIORef',
    newIORef,
    readIORef,
    writeIORef,
 )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.CellResult (CellId)
import Sabela.AI.Types (ToolOutcome)
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover (GrammarMode (..))
import Siza.Agent.Discover.History (SearchLedger)
import Siza.Agent.Discover.HistoryGuard (guardDiscover, newSearchLedger)
import Siza.Agent.Futility (
    FutilityGuard,
    guardDispatch,
    newFutilityGuard,
    rejectionRepeats,
 )
import Siza.Agent.Owned (OwnedCell (..), recordOwned)
import Siza.Agent.ToolRoute (normalizeToolCall)

type Dispatch = ToolCall -> IO (Either Text ToolOutcome)

-- | An entry point that drives the shared stack.
data Surface = ChatSurface | McpSurface
    deriving (Bounded, Enum, Eq, Show)

{- | Everything the two entry points do differently, in one place. Derived from
the surface rather than passed, so a third entry point cannot pick a
combination nobody meant.
-}
data SurfacePolicy = SurfacePolicy
    { spHealsReds :: Bool
    , spPreflights :: Bool
    , spElides :: Bool
    , spFoldsDiscovery :: Bool
    }
    deriving (Eq, Show)

{- | A tools/call is the only beat the MCP server sees, so it repairs and
discovers within the call; the loop has a later turn for both, and is the only
surface that elides, so it is the only one that offers the recall tool.
-}
surfacePolicy :: Surface -> SurfacePolicy
surfacePolicy ChatSurface =
    SurfacePolicy
        { spHealsReds = False
        , spPreflights = False
        , spElides = True
        , spFoldsDiscovery = False
        }
surfacePolicy McpSurface =
    SurfacePolicy
        { spHealsReds = True
        , spPreflights = True
        , spElides = False
        , spFoldsDiscovery = True
        }

{- | State a driving session accumulates across calls. In the loop it spans an
episode; in the MCP server it spans the process.
-}
data StackSession = StackSession
    { ssFutility :: FutilityGuard
    , ssLedger :: IORef SearchLedger
    , ssOwned :: IORef (Map CellId OwnedCell)
    , ssGoal :: IORef Text
    , ssDelivered :: IORef Bool
    , ssSeenNotes :: IORef (Set Text)
    , ssGrammar :: GrammarMode
    , ssSurface :: Surface
    }

sessionPolicy :: StackSession -> SurfacePolicy
sessionPolicy = surfacePolicy . ssSurface

ssHealReds :: StackSession -> Bool
ssHealReds = spHealsReds . sessionPolicy

{- | @newSessionFor surface grammar goal@. The loop seeds the goal with the
episode prompt; the MCP server starts blank and learns it from a write.
-}
newSessionFor :: Surface -> GrammarMode -> Text -> IO StackSession
newSessionFor surface grammar goal =
    StackSession
        <$> newFutilityGuard
        <*> newSearchLedger
        <*> newIORef Map.empty
        <*> newIORef goal
        <*> newIORef False
        <*> newIORef Set.empty
        <*> pure grammar
        <*> pure surface

{- | The chat loop's constructor. The Bool it still passes is ignored: repair
beats belong to the surface, not to the caller.
-}
newStackSession :: GrammarMode -> Bool -> Text -> IO StackSession
newStackSession grammar _ = newSessionFor ChatSurface grammar

{- | The pre-dispatch stack, outermost first. 'takeGoal' sits outside the
futility guard so the guard's key stays goal-insensitive: two byte-identical
retries carrying different goals are still the same retry.
-}
stackDispatch :: StackSession -> Dispatch -> Dispatch
stackDispatch ss inner =
    captureGoal
        ss
        (guardDiscover (ssLedger ss) (guardDispatch (ssFutility ss) inner))
        . normalizeToolCall

{- | The layer names, in application order. A parity test asserts on this so it
cannot pass by exercising nothing.
-}
stackLayers :: [Text]
stackLayers = ["normalize", "goal", "discover-ledger", "futility"]

captureGoal :: StackSession -> Dispatch -> Dispatch
captureGoal ss inner call = do
    let (mGoal, args') = takeGoal (tcArgs call)
    mapM_ (setGoal ss) mGoal
    inner call{tcArgs = args'}

{- | Split an optional @goal@ off an argument object. The field is a hint to
the client-side repair layers; the server never sees it.
-}
takeGoal :: Value -> (Maybe Text, Value)
takeGoal (Object o) = case KM.lookup goalKey o of
    Just (String g)
        | not (T.null (T.strip g)) ->
            (Just (T.strip g), Object (KM.delete goalKey o))
    Just _ -> (Nothing, Object (KM.delete goalKey o))
    Nothing -> (Nothing, Object o)
  where
    goalKey = K.fromText "goal"
takeGoal v = (Nothing, v)

sessionGoal :: StackSession -> IO Text
sessionGoal = readIORef . ssGoal

setGoal :: StackSession -> Text -> IO ()
setGoal ss = writeIORef (ssGoal ss)

-- | Fold a completed call into the owned-cell map.
recordCall :: StackSession -> (ToolCall, Either Text ToolOutcome) -> IO ()
recordCall ss step =
    atomicModifyIORef' (ssOwned ss) (\m -> (recordOwned step m, ()))

ownedCells :: StackSession -> IO (Map CellId OwnedCell)
ownedCells = readIORef . ssOwned

{- | The cells this session wrote that are currently red, with their
diagnostics. Cells the session did not write are never listed.
-}
ownedReds :: StackSession -> IO [(CellId, Text)]
ownedReds ss = do
    owned <- ownedCells ss
    pure
        [(cid, ocDiagnostic oc) | (cid, oc) <- Map.toList owned, not (ocHealthy oc)]

{- | How often each rejection's diagnostic class recurred in this session. A
rejection creates no cell, so this is the only session state a write refused
by the gate leaves behind.
-}
sessionRejectionRepeats :: StackSession -> IO (Map Text Int)
sessionRejectionRepeats = rejectionRepeats . ssFutility

markDelivered :: StackSession -> IO ()
markDelivered ss = writeIORef (ssDelivered ss) True

isDelivered :: StackSession -> IO Bool
isDelivered = readIORef . ssDelivered

-- | True the first time this session is asked about a given note text.
freshNote :: StackSession -> Text -> IO Bool
freshNote ss t =
    atomicModifyIORef'
        (ssSeenNotes ss)
        (\s -> (Set.insert t s, not (Set.member t s)))
