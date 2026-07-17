{- | The composed 'App' record and the small accessors over it. Split from
"Sabela.State" so the AI-config subsystem ("Sabela.State.AIConfig") can depend on
'App' without a cycle; 'Sabela.State' re-exports this module.
-}
module Sabela.State.App (
    App (..),
    clearCompiledModules,
    setBuilding,
    withBuilding,
    getAIStore,
    setAIStore,
    broadcastNotebook,
    resolveCliHandleStore,
) where

import Control.Concurrent.MVar (
    MVar,
    modifyMVar,
    modifyMVar_,
    readMVar,
 )
import Control.Exception (bracket_)
import Data.IORef (IORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client (Manager)

import Sabela.AI.Handles (HandleStore, newHandleStore)
import Sabela.AI.Store (AIStore)
import Sabela.Model (NotebookEvent (..))
import Sabela.State.BridgeStore
import Sabela.State.DependencyTracker
import Sabela.State.Environment
import Sabela.State.EventBus
import Sabela.State.NotebookStore
import Sabela.State.SessionManager
import Sabela.State.WidgetStore

data App = App
    { appEnv :: Environment
    , appNotebook :: NotebookStore
    , appEvents :: EventBus
    , appSessions :: SessionManager
    , appDeps :: DependencyTracker
    , appWidgets :: WidgetStore
    , appBridge :: BridgeStore
    , appCompiledModules :: IORef (M.Map Text Text)
    {- ^ Module name → rendered source currently loaded into the live GHCi
    session. Drives the compile-phase skip; cleared on session restart.
    -}
    , appAI :: MVar (Maybe AIStore)
    , appHttpMgr :: Maybe Manager
    , appAiToken :: Maybe Text
    {- ^ If set, `/api/ai/*` requires `Authorization: Bearer <token>`.
    Comes from the @SABELA_AI_TOKEN@ env var at startup.
    -}
    , appCliSessions :: MVar (M.Map Text HandleStore)
    {- ^ Per-session handle stores for external CLI clients, keyed by
    the @X-Sabela-Session@ header. Created lazily on first request.
    -}
    , appBuilding :: IORef Bool
    {- ^ True while the kernel is doing off-lock build work — installing a
    cabal env, spawning/cold-starting GHCi, or compiling a @-- compile@
    module. Distinct from the run-lock @running@ axis so a driver can tell a
    cold start from a hung cell ('kernel_status' surfaces it as @compiling@).
    -}
    , appBuildingSince :: IORef (Maybe Word64)
    {- ^ Monotonic-ns start of the current build ('Nothing' when idle), kept in
    lock-step with 'appBuilding' by 'setBuilding'. Lets 'kernel_status' tell
    "compiling 2s" from "wedged 5min" instead of a driver retrying blindly.
    -}
    , appAINumCtx :: IORef Int
    {- ^ Ollama @num_ctx@ (context window) for the in-notebook assistant,
    configurable from the AI settings modal and persisted to config.json.
    Baked into the Ollama provider when it is (re)built.
    -}
    , appAIToolLimit :: IORef Int
    -- ^ Per-turn cap on tool-call rounds the agentic loop will run.
    }

-- | Forget which compiled modules the live session has loaded.
clearCompiledModules :: App -> IO ()
clearCompiledModules app = writeIORef (appCompiledModules app) M.empty

{- | Flip the off-lock build flag (see 'appBuilding'), stamping (or clearing)
'appBuildingSince' in lock-step so 'kernel_status' can report build elapsed.
-}
setBuilding :: App -> Bool -> IO ()
setBuilding app True = do
    t <- getMonotonicTimeNSec
    writeIORef (appBuildingSince app) (Just t)
    writeIORef (appBuilding app) True
setBuilding app False = do
    writeIORef (appBuilding app) False
    writeIORef (appBuildingSince app) Nothing

{- | Run an action with the build flag raised, lowering it again even on
exception. Wrap cabal-env installs, cold starts, and @-- compile@ builds so
'kernel_status' reports @compiling@ while they run.
-}
withBuilding :: App -> IO a -> IO a
withBuilding app = bracket_ (setBuilding app True) (setBuilding app False)

-- | Read the current AI store (if configured).
getAIStore :: App -> IO (Maybe AIStore)
getAIStore = readMVar . appAI

-- | Set the AI store.
setAIStore :: App -> Maybe AIStore -> IO ()
setAIStore app val = modifyMVar_ (appAI app) (const (pure val))

{- | Read the current notebook and broadcast it as an @EvNotebookChanged@ SSE
event. Call this after any mutation that changes the cell list, cell order, or
cell source outside the reactive execute pipeline — AI tool mutations, HTTP
insert/delete/reorder handlers, accepted edits.
-}
broadcastNotebook :: App -> IO ()
broadcastNotebook app = do
    nb <- readNotebook (appNotebook app)
    broadcast (appEvents app) (EvNotebookChanged nb)

{- | Look up (or lazily create) a per-CLI-session HandleStore keyed by the
@X-Sabela-Session@ header value. Isolates @explore_result@ handles between
concurrent external CLI clients.
-}
resolveCliHandleStore :: App -> Text -> IO HandleStore
resolveCliHandleStore app sid = modifyMVar (appCliSessions app) $ \m ->
    case M.lookup sid m of
        Just hs -> pure (m, hs)
        Nothing -> do
            hs <- newHandleStore
            pure (M.insert sid hs m, hs)
