{-# LANGUAGE OverloadedStrings #-}

{- | Pins the §1.4 atomic admission fold at the REAL @executeTool@ dispatch:
two simultaneous kernel-needing tool calls share one 'AIStore', and exactly
one is admitted to the backend while the other is bounced @busy@. This drives
the production code path (the @admit@ gate in @executeTool@), not a synthetic
parallel MVar harness, so it covers the live TOCTOU rather than a stand-in.
-}
module Test.AtomicAdmissionSpec (spec) where

import Control.Concurrent (
    forkIO,
    newEmptyMVar,
    putMVar,
    readMVar,
    takeMVar,
    threadDelay,
 )
import Control.Concurrent.MVar (MVar)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (partition)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Unique (newUnique)
import Network.HTTP.Client (defaultManagerSettings, newManager)

import Sabela.AI.Capabilities (executeTool)
import qualified Sabela.AI.Store as AIStore
import Sabela.AI.Types (
    ToolOutcome,
    toolOutcomeIsError,
    toolOutcomeValue,
 )
import Sabela.Anthropic.Types (AnthropicConfig (..), newCancelToken)
import Sabela.Handlers (ReactiveNotebook (..))
import Sabela.Server (newApp)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.SessionManager (setHaskellSession)
import Test.Hspec

{- | A fake backend whose @sbQueryType@ blocks on a barrier and counts each
arrival, so the test can observe how many callers actually reached the kernel.
@sbBusy@ stays 'False' — the legacy admission read alone never blocks here, so
only the atomic gate can keep the second caller out.
-}
barrierBackend :: IORefCounter -> MVar () -> IO ST.SessionBackend
barrierBackend reachedRef gate = do
    uid <- newUnique
    let blockingQuery _ = do
            atomicModifyIORef' reachedRef (\n -> (n + 1, ()))
            readMVar gate -- peek; held until the test fills it
            pure "result"
        backend =
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
                , ST.sbQueryType = blockingQuery
                , ST.sbQueryInfo = \_ -> pure ""
                , ST.sbQueryKind = \_ -> pure ""
                , ST.sbQueryBrowse = \_ -> pure ""
                , ST.sbQueryBindings = pure ""
                , ST.sbQueryDoc = \_ -> pure ""
                , ST.sbQueryHoleFits = \_ -> pure ""
                }
    pure backend

type IORefCounter = IORef Int

mkApp :: ST.SessionBackend -> IO App
mkApp backend = do
    mgr <- newManager defaultManagerSettings
    app <- newApp "." Set.empty (Just mgr) Nothing []
    setHaskellSession (appSessions app) (Just backend)
    pure app

mkStore :: IO AIStore.AIStore
mkStore = do
    mgr <- newManager defaultManagerSettings
    let cfg =
            AnthropicConfig
                { acApiKey = ""
                , acModel = "placeholder"
                , acBaseUrl = "https://api.anthropic.com"
                }
    AIStore.newAIStore cfg mgr

inertRn :: ReactiveNotebook
inertRn =
    ReactiveNotebook
        { rnCellEdit = \_ _ -> pure ()
        , rnRunCell = \_ -> pure ()
        , rnRunCellForced = \_ -> pure ()
        , rnRunAll = pure ()
        , rnReset = pure ()
        , rnRestartKernel = pure ()
        , rnWidgetCell = \_ -> pure ()
        }

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

queryInput :: Value
queryInput = object ["op" .= ("type" :: Text), "arg" .= ("map" :: Text)]

{- | Race two @ghci_query@ calls through one shared 'AIStore' and
'executeTool'. The barrier in the backend keeps the admitted caller inside
the kernel while the loser is decided, so the simultaneity is real. Returns
(outcomes, number of callers that actually reached the backend).
-}
raceTwoQueries :: IO ([ToolOutcome], Int)
raceTwoQueries = do
    reached <- newIORef 0
    gate <- newEmptyMVar
    backend <- barrierBackend reached gate
    app <- mkApp backend
    store <- mkStore
    results <- newEmptyMVar
    let call = do
            ct <- newCancelToken
            out <- executeTool app store inertRn ct "ghci_query" queryInput
            putMVar results out
    _ <- forkIO call
    _ <- forkIO call
    threadDelay 50000 -- let the admitted caller enter the blocking query
    n <- readIORef reached
    putMVar gate () -- release the held query so the winner completes
    o1 <- takeMVar results
    o2 <- takeMVar results
    pure ([o1, o2], n)

spec :: Spec
spec = describe "atomic admission at the real executeTool dispatch (§1.4)" $ do
    it "exactly one of two simultaneous kernel calls reaches the backend" $ do
        (_outcomes, reached) <- raceTwoQueries
        reached `shouldBe` 1

    it "the loser is bounced with a busy tool error, the winner succeeds" $ do
        (outcomes, _reached) <- raceTwoQueries
        let (errs, oks) = partition toolOutcomeIsError outcomes
        length oks `shouldBe` 1
        length errs `shouldBe` 1
        field "busy" (toolOutcomeValue (head errs)) `shouldBe` Just (Bool True)

    it "a lone kernel call is admitted and returns its result" $ do
        gate <- newEmptyMVar
        putMVar gate () -- pre-open: no blocking for the solo path
        reached <- newIORef 0
        backend <- barrierBackend reached gate
        app <- mkApp backend
        store <- mkStore
        ct <- newCancelToken
        out <- executeTool app store inertRn ct "ghci_query" queryInput
        toolOutcomeIsError out `shouldBe` False
        readIORef reached `shouldReturn` 1
