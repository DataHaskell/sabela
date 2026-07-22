{-# LANGUAGE OverloadedStrings #-}

{- | Pins R6.1/R6.2/R6.4 at the real @executeTool@ dispatch: an insert whose
cell runs long acks @{cellId, status: executing}@ well under the transport
timeout; retrying the byte-identical write never duplicates the cell (the
retry says the original landed); a kernel-needing call during one's own
in-flight write is bounced with the writing cell named plus elapsed time;
and @await_idle@ later reconciles the settled outcome. The slow/fast class
is decided purely by a wall-clock race — never by cell content.
-}
module Test.WriteAckSpec (spec) where

import Control.Concurrent (
    forkIO,
    newEmptyMVar,
    putMVar,
    readMVar,
    threadDelay,
 )
import Control.Concurrent.MVar (MVar)
import Control.Exception (bracket_)
import Control.Monad (void)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (newUnique)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import System.Environment (setEnv, unsetEnv)
import System.Timeout (timeout)

import Sabela.AI.Capabilities (executeTool)
import qualified Sabela.AI.Store as AIStore
import Sabela.AI.Types (
    ToolOutcome,
    toolOutcomeIsError,
    toolOutcomeValue,
 )
import Sabela.Anthropic.Types (AnthropicConfig (..), newCancelToken)
import Sabela.Handlers (ReactiveNotebook (..))
import Sabela.Model (Notebook (..), NotebookEvent (..))
import Sabela.Server (newApp)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), broadcast, readNotebook)
import Sabela.State.SessionManager (setHaskellSession)
import Test.Hspec

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

-- | Insert against a never-released barrier and hand back the executing ack.
ackExecuting :: IO (App, AIStore.AIStore, ReactiveNotebook, MVar (), Value)
ackExecuting = do
    (app, store) <- mkFixture
    barrier <- newEmptyMVar
    let rn = slowRn app barrier
    mv <- timeout 15000000 (insertSrc app store rn "x = 1")
    case mv of
        Nothing -> do
            expectationFailure "insert did not ack within 15s"
            error "unreachable"
        Just v -> pure (app, store, rn, barrier, v)

spec :: Spec
spec = around_ withAckEnv $ describe "write-ack (R6.1/R6.2/R6.4)" $ do
    it "a long-running insert acks {cellId, status: executing} under the deadline" $ do
        t0 <- getMonotonicTimeNSec
        (_, _, _, barrier, v) <- ackExecuting
        t1 <- getMonotonicTimeNSec
        (t1 - t0) < 10000000000 `shouldBe` True
        textField "status" v `shouldBe` Just "executing"
        field "cellId" v `shouldBe` Just (Number 0)
        putMVar barrier ()

    it "the executing ack states the write landed and how to reconcile" $ do
        (_, _, _, barrier, v) <- ackExecuting
        let note = fromMaybe "" (textField "note" v)
        note `shouldSatisfy` T.isInfixOf "landed"
        note `shouldSatisfy` T.isInfixOf "await_idle"
        putMVar barrier ()

    it "retrying the identical write while executing does not duplicate (R6.2)" $ do
        (app, store, rn, barrier, v) <- ackExecuting
        retry <- insertSrc app store rn "x = 1"
        field "duplicate" retry `shouldBe` Just (Bool True)
        field "cellId" retry `shouldBe` field "cellId" v
        fromMaybe "" (textField "note" retry)
            `shouldSatisfy` T.isInfixOf "landed"
        cellCount app `shouldReturn` 1
        putMVar barrier ()

    it
        "a kernel call during one's own write is bounced naming cell + elapsed (R6.4)"
        $ do
            (app, store, rn, barrier, _) <- ackExecuting
            out <-
                callTool app store rn "execute_cell" (object ["cell_id" .= (0 :: Int)])
            toolOutcomeIsError out `shouldBe` True
            let v = toolOutcomeValue out
            field "busy" v `shouldBe` Just (Bool True)
            textField "cause" v `shouldBe` Just "own-write"
            field "cellId" v `shouldBe` Just (Number 0)
            isJust (field "elapsedMs" v) `shouldBe` True
            fromMaybe "" (textField "hint" v)
                `shouldSatisfy` T.isInfixOf "await_idle"
            putMVar barrier ()

    it "await_idle reconciles the settled outcome exactly once (R6.1)" $ do
        (app, store, rn, barrier, _) <- ackExecuting
        putMVar barrier ()
        threadDelay 300000
        v <- toolOutcomeValue <$> callTool app store rn "await_idle" (object [])
        case field "writes" v of
            Just (Array _) -> pure ()
            other -> expectationFailure ("no writes reconciled: " <> show other)
        let cellIds = case field "writes" v of
                Just ws -> field "cellId" <$> arrayItems ws
                Nothing -> []
        cellIds `shouldBe` [Just (Number 0)]
        v2 <- toolOutcomeValue <$> callTool app store rn "await_idle" (object [])
        field "writes" v2 `shouldBe` Nothing

    it "retrying after settle says the original landed, with its outcome" $ do
        (app, store, rn, barrier, _) <- ackExecuting
        putMVar barrier ()
        threadDelay 300000
        retry <- insertSrc app store rn "x = 1"
        field "duplicate" retry `shouldBe` Just (Bool True)
        textField "status" retry `shouldBe` Just "completed"
        isJust (field "execution" retry) `shouldBe` True
        cellCount app `shouldReturn` 1

    it "a fast insert still completes inline with its execution summary" $ do
        (app, store) <- mkFixture
        let rn = fastRn app
        v <- insertSrc app store rn "x = 1"
        textField "status" v `shouldBe` Just "completed"
        isJust (field "execution" v) `shouldBe` True
        cellCount app `shouldReturn` 1

    it "a different source is never deduped" $ do
        (app, store) <- mkFixture
        let rn = fastRn app
        _ <- insertSrc app store rn "x = 1"
        v <- insertSrc app store rn "y = 2"
        field "duplicate" v `shouldBe` Nothing
        field "cellId" v `shouldBe` Just (Number 1)
        cellCount app `shouldReturn` 2

    it "a prose insert completes at once and its retry is deduped" $ do
        (app, store) <- mkFixture
        let rn = fastRn app
        let input =
                object
                    [ "source" .= ("hello world" :: Text)
                    , "cell_type" .= ("ProseCell" :: Text)
                    ]
        v <- toolOutcomeValue <$> callTool app store rn "insert_cell" input
        textField "status" v `shouldBe` Just "completed"
        retry <- toolOutcomeValue <$> callTool app store rn "insert_cell" input
        field "duplicate" retry `shouldBe` Just (Bool True)
        cellCount app `shouldReturn` 1

arrayItems :: Value -> [Value]
arrayItems (Array xs) = foldr (:) [] xs
arrayItems _ = []
