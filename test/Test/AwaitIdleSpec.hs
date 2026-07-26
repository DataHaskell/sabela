{-# LANGUAGE OverloadedStrings #-}

module Test.AwaitIdleSpec (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Unique (newUnique)
import GHC.Clock (getMonotonicTimeNSec)

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Sabela.AI.Capabilities.Kernel (execAwaitIdle)
import qualified Sabela.AI.Store as AIStore
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.Anthropic.Types (AnthropicConfig (..))
import Sabela.Model (NotebookEvent (..))
import Sabela.Server (newApp)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), setBuilding)
import Sabela.State.EventBus (
    AwaitResult (..),
    awaitExecutionDone,
    broadcast,
 )
import Sabela.State.SessionManager (setHaskellSession)
import Test.Hspec

fakeBackend :: Bool -> IO ST.SessionBackend
fakeBackend busy = do
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
                , ST.sbBusy = pure busy
                , ST.sbSessionGen = pure 0
                , ST.sbRequestStale = \_ -> pure False
                , ST.sbQueryComplete = \_ -> pure []
                , ST.sbQueryType = \_ -> pure ""
                , ST.sbQueryInfo = \_ -> pure ""
                , ST.sbQueryKind = \_ -> pure ""
                , ST.sbQueryBrowse = \_ -> pure ""
                , ST.sbQueryBindings = pure ""
                , ST.sbQueryDoc = \_ -> pure ""
                , ST.sbQueryHoleFits = \_ -> pure ""
                , ST.sbEvalPureLive = \req -> pure (ST.pureEvalUnavailableResult req "fake backend")
                }
    pure backend

liveApp :: Bool -> IO (App, AIStore.AIStore)
liveApp busy = do
    app <- newApp "." Set.empty Nothing Nothing []
    backend <- fakeBackend busy
    setHaskellSession (appSessions app) (Just backend)
    mgr <- newManager defaultManagerSettings
    store <-
        AIStore.newAIStore
            (AnthropicConfig "" "placeholder" "https://api.anthropic.com")
            mgr
    pure (app, store)

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

floodEvent :: NotebookEvent
floodEvent = EvCellResult 1 [] Nothing [] []

forkFlood :: App -> Int -> IO (IO ())
forkFlood app gapUs = do
    tid <- forkIO $ forever $ do
        broadcast (appEvents app) floodEvent
        threadDelay gapUs
    pure (killThread tid)

spec :: Spec
spec = describe "await_idle long-poll (execAwaitIdle / awaitExecutionDone)" $ do
    it "returns immediately when the kernel is already idle" $ do
        (app, store) <- liveApp False
        v <- toolOutcomeValue <$> execAwaitIdle app store
        field "waited" v `shouldBe` Just (String "idle")

    it "carries a fresh kernel status snapshot in the reply" $ do
        (app, store) <- liveApp False
        v <- toolOutcomeValue <$> execAwaitIdle app store
        (field "state" =<< field "state" =<< field "status" v)
            `shouldBe` Just (String "idle")
        (field "kernel" =<< field "status" v)
            `shouldBe` Nothing

    it "while building (idle run-lock) it still long-polls, not idle" $ do
        (app, store) <- liveApp False
        setBuilding app True
        done <- newEmptyMVar
        _ <- forkIO (execAwaitIdle app store >>= putMVar done . toolOutcomeValue)
        threadDelay 50000
        broadcast (appEvents app) EvExecutionDone
        v <- takeMVar done
        field "waited" v `shouldBe` Just (String "settled")

    it "a busy kernel settles only when EvExecutionDone fires" $ do
        (app, store) <- liveApp True
        done <- newEmptyMVar
        _ <- forkIO (execAwaitIdle app store >>= putMVar done . toolOutcomeValue)
        threadDelay 50000
        broadcast (appEvents app) (EvCellResult 1 [] Nothing [] [])
        threadDelay 50000
        broadcast (appEvents app) EvExecutionDone
        v <- takeMVar done
        field "waited" v `shouldBe` Just (String "settled")

    it "settles on the fence even when the kernel reports busy throughout" $ do
        (app, _) <- liveApp True
        res <- newEmptyMVar
        _ <-
            forkIO
                ( awaitExecutionDone
                    (appEvents app)
                    5000000
                    (pure True)
                    >>= putMVar res
                )
        threadDelay 50000
        broadcast (appEvents app) EvExecutionDone
        r <- takeMVar res
        r `shouldBe` AwaitSettled

    it "kill-aware: a dead kernel returns a terminal state, not a wedge" $ do
        (app, _) <- liveApp True
        res <- newEmptyMVar
        _ <-
            forkIO
                ( awaitExecutionDone
                    (appEvents app)
                    5000000
                    (pure False)
                    >>= putMVar res
                )
        r <- takeMVar res
        r `shouldBe` AwaitKernelDead

    it "the bounded budget elapses to a non-terminal timeout (caller re-loops)" $ do
        (app, _) <- liveApp True
        r <-
            awaitExecutionDone
                (appEvents app)
                150000
                (pure True)
        r `shouldBe` AwaitTimedOut

    it "a steady non-fence flood still times out within budget (wall-clock bound)" $ do
        (app, _) <- liveApp True
        stop <- forkFlood app 1000
        r <-
            awaitExecutionDone
                (appEvents app)
                200000
                (pure True)
        stop
        r `shouldBe` AwaitTimedOut

    it "a non-fence flood does not defeat the ~budget wall-clock ceiling" $ do
        (app, _) <- liveApp True
        stop <- forkFlood app 1000
        start <- getMonotonicTimeNSec
        _ <- awaitExecutionDone (appEvents app) 200000 (pure True)
        end <- getMonotonicTimeNSec
        stop
        let elapsedUs = fromIntegral ((end - start) `div` 1000) :: Int
        elapsedUs `shouldSatisfy` (< 2000000)

    it "mid-stream kernel death under a flood returns AwaitKernelDead, not a wedge" $ do
        (app, _) <- liveApp True
        alive <- newIORef True
        stop <- forkFlood app 1000
        _ <- forkIO (threadDelay 50000 >> writeIORef alive False)
        r <-
            awaitExecutionDone
                (appEvents app)
                5000000
                (readIORef alive)
        stop
        r `shouldBe` AwaitKernelDead

    it "execAwaitIdle on a kernel that dies mid-wait reports kernelDead" $ do
        (app, store) <- liveApp True
        done <- newEmptyMVar
        _ <- forkIO (execAwaitIdle app store >>= putMVar done . toolOutcomeValue)
        threadDelay 50000
        setHaskellSession (appSessions app) Nothing
        v <- takeMVar done
        field "waited" v `shouldBe` Just (String "kernelDead")
