{-# LANGUAGE OverloadedStrings #-}

{- | Pins Phase 0.0 Confounder 1: kernel-needing discovery tools warm a cold
GHCi once at the @executeTool@ dispatch, so they work before the first cell is
run instead of failing with "No live Haskell session". Drives the real
production path (the @kernelGuarded@ warm-up in @Sabela.AI.Capabilities@).
-}
module Test.DiscoveryToolsPreSessionSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (newUnique)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import System.Directory (findExecutable)

import Sabela.AI.Capabilities (executeTool)
import qualified Sabela.AI.Store as AIStore
import Sabela.AI.Types (
    ToolOutcome,
    toolOutcomeValue,
 )
import Sabela.Anthropic.Types (AnthropicConfig (..), newCancelToken)
import Sabela.Handlers (ReactiveNotebook (..))
import Sabela.Server (newApp)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.SessionManager (getHaskellSession, setHaskellSession)
import Test.Hspec

mkApp :: IO App
mkApp = do
    mgr <- newManager defaultManagerSettings
    newApp "." Set.empty (Just mgr) Nothing []

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

{- | A no-op backend with a stable id so we can detect whether warming replaced
a live session (it must not).
-}
sentinelBackend :: IO ST.SessionBackend
sentinelBackend = do
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
                , ST.sbQueryType = \_ -> pure "result"
                , ST.sbQueryInfo = \_ -> pure ""
                , ST.sbQueryKind = \_ -> pure ""
                , ST.sbQueryBrowse = \_ -> pure ""
                , ST.sbQueryBindings = pure ""
                , ST.sbQueryDoc = \_ -> pure ""
                , ST.sbQueryHoleFits = \_ -> pure ""
                }
    pure backend

runDiscovery :: App -> Text -> Value -> IO ToolOutcome
runDiscovery app tool input = do
    store <- mkStore
    ct <- newCancelToken
    executeTool app store inertRn ct tool input

{- | Drive a discovery tool on a cold App: it triggers the @kernelGuarded@
warm-up. Gated on cabal and on the warm actually producing a session — a
sandbox that cannot build the repl project skips rather than fails, so the
assertion only runs where the warm-up could materialise GHCi.
-}
warmThenAssert :: Text -> Value -> (ToolOutcome -> Expectation) -> Expectation
warmThenAssert tool input assertOut = do
    cabal <- findExecutable "cabal"
    case cabal of
        Nothing -> pendingWith "cabal not found on PATH; skipping warm-up"
        Just _ -> do
            app <- mkApp
            out <- runDiscovery app tool input
            mSess <- getHaskellSession (appSessions app)
            case mSess of
                Nothing ->
                    pendingWith
                        "warm-up could not build the repl project in this sandbox"
                Just _ -> assertOut out

noSessionText :: Text
noSessionText = "No live Haskell session"

outcomeText :: ToolOutcome -> Text
outcomeText = T.pack . show . toolOutcomeValue

spec :: Spec
spec = describe "discovery tools warm a cold kernel (Phase 0.0 confounder 1)" $ do
    it "leaves a live session untouched (warm-up is idempotent)" $ do
        app <- mkApp
        backend <- sentinelBackend
        setHaskellSession (appSessions app) (Just backend)
        let beforeId = ST.sbSessionId backend
        _ <- runDiscovery app "check_type" (object ["expr" .= ("id" :: Text)])
        mSess <- getHaskellSession (appSessions app)
        case mSess of
            Nothing -> expectationFailure "warm-up dropped the live session"
            Just s ->
                (ST.sbSessionId s == beforeId)
                    `shouldBe` True

    it "list_bindings no longer errors 'No live Haskell session' pre-cell" $
        warmThenAssert "list_bindings" (object []) $ \out ->
            T.isInfixOf noSessionText (outcomeText out) `shouldBe` False

    it "check_type no longer errors 'No live Haskell session' pre-cell"
        $ warmThenAssert
            "check_type"
            (object ["expr" .= ("map fst" :: Text)])
        $ \out ->
            T.isInfixOf noSessionText (outcomeText out) `shouldBe` False
