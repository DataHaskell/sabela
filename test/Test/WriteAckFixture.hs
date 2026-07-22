{-# LANGUAGE OverloadedStrings #-}

{- | Shared fixture for the write-ack invariant specs: a real @executeTool@
dispatch over an inert kernel backend, with a barrier-controlled slow
reactive notebook and a fast one. Mirrors @Test.WriteAckSpec@'s fixture so
the schedule/shape properties drive the same production seam.
-}
module Test.WriteAckFixture (
    withAckEnv,
    mkFixture,
    mkScriptedFixture,
    slowRn,
    fastRn,
    callTool,
    insertSrc,
    cellCount,
    field,
    textField,
) where

import Control.Concurrent (forkIO, readMVar, threadDelay)
import Control.Concurrent.MVar (MVar)
import Control.Exception (bracket_)
import Control.Monad (join, void)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, newIORef, readIORef)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Unique (newUnique)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import System.Environment (setEnv, unsetEnv)

import Sabela.AI.Capabilities (executeTool)
import qualified Sabela.AI.Store as AIStore
import Sabela.AI.Types (ToolOutcome, toolOutcomeValue)
import Sabela.Anthropic.Types (AnthropicConfig (..), newCancelToken)
import Sabela.Handlers (ReactiveNotebook (..))
import Sabela.Model (Notebook (..), NotebookEvent (..))
import Sabela.Server (newApp)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), broadcast, readNotebook)
import Sabela.State.SessionManager (setHaskellSession)

-- | 1s ack deadline + zero repair budget, restored after each example.
withAckEnv :: IO a -> IO a
withAckEnv =
    bracket_
        ( setEnv "SABELA_WRITE_ACK_SECS" "1"
            >> setEnv "SABELA_REPAIR_BUDGET_SECS" "0"
        )
        ( unsetEnv "SABELA_WRITE_ACK_SECS"
            >> unsetEnv "SABELA_REPAIR_BUDGET_SECS"
        )

inertBackend :: IO ST.SessionBackend
inertBackend = do
    uid <- newUnique
    let backend =
            ST.SessionBackend
                { ST.sbSessionId = uid
                , ST.sbJsonDiagnostics = False
                , ST.sbRunBlock = \_ -> pure ("", "")
                , ST.sbRunBlockStreaming = \_ _ -> pure ("", "")
                , ST.sbClose = pure ()
                , ST.sbReset = pure backend
                , ST.sbInterrupt = pure ()
                , ST.sbBusy = pure False
                , ST.sbSessionGen = pure 0
                , ST.sbRequestStale = \_ -> pure False
                , ST.sbQueryComplete = \_ -> pure []
                , ST.sbQueryType = \_ -> pure "it :: ()"
                , ST.sbQueryInfo = \_ -> pure ""
                , ST.sbQueryKind = \_ -> pure ""
                , ST.sbQueryBrowse = \_ -> pure ""
                , ST.sbQueryBindings = pure ""
                , ST.sbQueryDoc = \_ -> pure ""
                , ST.sbQueryHoleFits = \_ -> pure ""
                }
    pure backend

mkFixture :: IO (App, AIStore.AIStore)
mkFixture = do
    mgr <- newManager defaultManagerSettings
    app <- newApp "." Set.empty (Just mgr) Nothing []
    backend <- inertBackend
    setHaskellSession (appSessions app) (Just backend)
    let cfg =
            AnthropicConfig
                { acApiKey = ""
                , acModel = "placeholder"
                , acBaseUrl = "https://api.anthropic.com"
                }
    store <- AIStore.newAIStore cfg mgr
    pure (app, store)

{- | As 'mkFixture', but the backend's @sbBusy@ is a swappable sampler, so a
spec can script a post-settle release tail or a persistent foreign busy.
-}
mkScriptedFixture :: IO (App, AIStore.AIStore, IORef (IO Bool))
mkScriptedFixture = do
    (app, store) <- mkFixture
    busyRef <- newIORef (pure False)
    backend <- inertBackend
    setHaskellSession
        (appSessions app)
        (Just backend{ST.sbBusy = join (readIORef busyRef)})
    pure (app, store, busyRef)

{- | A reactive notebook whose forced run completes (broadcasts the cell's
result) only once the barrier fills — a deliberately long-running cell.
-}
slowRn :: App -> MVar () -> ReactiveNotebook
slowRn app barrier =
    (fastRn app)
        { rnRunCellForced = \cid -> void . forkIO $ do
            readMVar barrier
            broadcast (appEvents app) (EvCellResult cid [] Nothing [] [])
        }

-- | A reactive notebook whose forced run settles almost immediately.
fastRn :: App -> ReactiveNotebook
fastRn app =
    ReactiveNotebook
        { rnCellEdit = \_ _ -> pure ()
        , rnRunCell = \_ -> pure ()
        , rnRunCellForced = \cid -> void . forkIO $ do
            threadDelay 100000
            broadcast (appEvents app) (EvCellResult cid [] Nothing [] [])
        , rnRunAll = pure ()
        , rnReset = pure ()
        , rnRestartKernel = pure ()
        , rnWidgetCell = \_ -> pure ()
        }

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField k v = case field k v of
    Just (String s) -> Just s
    _ -> Nothing

callTool ::
    App -> AIStore.AIStore -> ReactiveNotebook -> Text -> Value -> IO ToolOutcome
callTool app store rn name input = do
    ct <- newCancelToken
    executeTool app store rn ct name input

insertSrc :: App -> AIStore.AIStore -> ReactiveNotebook -> Text -> IO Value
insertSrc app store rn src =
    toolOutcomeValue
        <$> callTool app store rn "insert_cell" (object ["source" .= src])

cellCount :: App -> IO Int
cellCount app = length . nbCells <$> readNotebook (appNotebook app)
