{-# LANGUAGE OverloadedStrings #-}

{- | Pins the §1.8 'ToolOutcome' mapping by driving the REAL @aiToolH@ /
@executeTool@ dispatch (not a reconstructed Aeson value). A cell that fails
to compile is a *successful tool call reporting a failed cell*, so the
envelope is 'ToolOk' (@isError: false@); a tool-mechanics failure (Busy,
bad arguments, unknown tool) is 'ToolErr' (@isError: true@). This guards the
backwards-boolean regression the envelope exists to kill.
-}
module Test.ToolOutcomeWireSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Unique (newUnique)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant (runHandler)

import Sabela.AI.Capabilities (executeTool)
import Sabela.AI.CellResult (CellOutcome (..), CellResult (..), toToolOutcome)
import qualified Sabela.AI.Store as AIStore
import Sabela.AI.Types (
    ToolOutcome (..),
    toolOutcomeIsError,
    toolOutcomeValue,
 )
import Sabela.Anthropic.Types (AnthropicConfig (..), newCancelToken)
import Sabela.Handlers (ReactiveNotebook (..))
import Sabela.Model (CellError (..), Notebook (..), NotebookEvent (..))
import Sabela.Server (newApp)
import Sabela.Server.Ai (aiToolH)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.EventBus (broadcast)
import Sabela.State.NotebookStore (modifyNotebook)
import Sabela.State.SessionManager (setHaskellSession)
import Test.Hspec
import Test.TopoSpec.Helpers (mkCell)

-- | A fake backend with a fixed busy flag; everything else is inert.
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
                }
    pure backend

{- | App with a real HTTP manager (so 'ensureAIStoreForTools' builds a store)
and a fake Haskell session of the given busy state.
-}
mkApp :: Bool -> IO App
mkApp busy = do
    mgr <- newManager defaultManagerSettings
    app <- newApp "." Set.empty (Just mgr) Nothing []
    backend <- fakeBackend busy
    setHaskellSession (appSessions app) (Just backend)
    modifyNotebook (appNotebook app) $ \nb ->
        nb{nbCells = [mkCell 1 "x = 1", mkCell 2 "y = 2"]}
    pure app

-- | A throwaway 'AIStore' for driving 'executeTool' directly.
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

{- | A 'ReactiveNotebook' whose run broadcasts a compile-error 'EvCellResult'
for the given cell. The broadcast is delayed so 'executeCell' has subscribed
to the bus before the result lands. @execute_cell@ drives @rnRunCellForced@,
so both run fields share the broadcaster.
-}
compileErrorRn :: App -> CellError -> ReactiveNotebook
compileErrorRn app err =
    ReactiveNotebook
        { rnCellEdit = \_ _ -> pure ()
        , rnRunCell = runBroadcast
        , rnRunCellForced = runBroadcast
        , rnRunAll = pure ()
        , rnReset = pure ()
        , rnRestartKernel = pure ()
        , rnWidgetCell = \_ -> pure ()
        }
  where
    runBroadcast cid = void $ forkIO $ do
        threadDelay 50000
        broadcast (appEvents app) (EvCellResult cid [] Nothing [err] [])

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

-- | Run the real REST bridge handler and pull out the @isError@ wire field.
toolWire :: App -> ReactiveNotebook -> Text -> Value -> IO Value
toolWire app rn name input = do
    let body = object ["name" .= name, "input" .= input]
    res <- runHandler (aiToolH app rn Nothing body)
    case res of
        Right v -> pure v
        Left e -> error ("aiToolH failed: " <> show e)

cerr :: CellError
cerr = CellError (Just 3) (Just 5) "Variable not in scope: foo"

spec :: Spec
spec = describe "ToolOutcome envelope mapping (§1.8)" $ do
    describe "toToolOutcome is total into ToolOk" $ do
        it "Succeeded maps to ToolOk (not an error)" $
            toolOutcomeIsError (toToolOutcome (CellResult Succeeded [] []))
                `shouldBe` False
        it "a compile error (Rejected) maps to ToolOk" $
            toolOutcomeIsError (toToolOutcome (CellResult (Rejected [cerr]) [] []))
                `shouldBe` False
        it "Raised maps to ToolOk" $
            toolOutcomeIsError (toToolOutcome (CellResult (Raised "boom") [] []))
                `shouldBe` False
        it "the ToolOk value carries the typed outcome tag" $
            case toToolOutcome (CellResult (Rejected [cerr]) [] []) of
                ToolOk v ->
                    (field "outcome" v >>= field "tag")
                        `shouldBe` Just (String "Rejected")
                ToolErr _ -> expectationFailure "expected ToolOk"

    describe "the real aiToolH/executeTool dispatch" $ do
        it "a compile-error cell yields isError:false (NOT a tool error)" $ do
            app <- mkApp False
            let rn = compileErrorRn app cerr
            v <- toolWire app rn "execute_cell" (object ["cell_id" .= (1 :: Int)])
            field "isError" v `shouldBe` Just (Bool False)

        it "the compile error rides in the typed CellResult, ok:false" $ do
            app <- mkApp False
            let rn = compileErrorRn app cerr
            v <- toolWire app rn "execute_cell" (object ["cell_id" .= (1 :: Int)])
            let result = field "result" v
            (result >>= field "ok") `shouldBe` Just (Bool False)
            (result >>= field "outcome" >>= field "tag")
                `shouldBe` Just (String "Rejected")

        it "a busy kernel yields isError:true (tool-mechanics failure)" $ do
            app <- mkApp True
            let rn = compileErrorRn app cerr
            v <- toolWire app rn "execute_cell" (object ["cell_id" .= (1 :: Int)])
            field "isError" v `shouldBe` Just (Bool True)
            (field "result" v >>= field "busy") `shouldBe` Just (Bool True)

        it "a bad argument (missing cell_id) yields isError:true" $ do
            app <- mkApp False
            let rn = compileErrorRn app cerr
            v <- toolWire app rn "execute_cell" (object [])
            field "isError" v `shouldBe` Just (Bool True)

        it "an unknown tool yields isError:true" $ do
            app <- mkApp False
            let rn = compileErrorRn app cerr
            v <- toolWire app rn "no_such_tool" (object [])
            field "isError" v `shouldBe` Just (Bool True)

    describe "executeTool returns the typed envelope directly" $ do
        it "a compile-error cell is ToolOk through executeTool" $ do
            app <- mkApp False
            store <- mkStore
            let rn = compileErrorRn app cerr
            ct <- newCancelToken
            out <-
                executeTool app store rn ct "execute_cell" $
                    object ["cell_id" .= (2 :: Int)]
            toolOutcomeIsError out `shouldBe` False
            let v = toolOutcomeValue out
            (field "outcome" v >>= field "tag") `shouldBe` Just (String "Rejected")
            field "cellId" v `shouldBe` Just (Number 2)

        it "a busy kernel is ToolErr through executeTool" $ do
            app <- mkApp True
            store <- mkStore
            let rn = compileErrorRn app cerr
            ct <- newCancelToken
            out <-
                executeTool app store rn ct "execute_cell" $
                    object ["cell_id" .= (2 :: Int)]
            toolOutcomeIsError out `shouldBe` True
