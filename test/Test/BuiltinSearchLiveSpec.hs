{-# LANGUAGE OverloadedStrings #-}

module Test.BuiltinSearchLiveSpec (spec) where

import Control.Exception (bracket)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Test.Live (requireLiveFor)

import Sabela.AI.Capabilities (executeTool)
import qualified Sabela.AI.Store as AIStore
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.Anthropic.Types (AnthropicConfig (..), newCancelToken)
import Sabela.Handlers (ReactiveNotebook, setupReactive)
import Sabela.Server (newApp)
import Sabela.Session.Project (buildTimeSupportDir)
import Sabela.State (App (..), forceResetAllSessions)

requireLiveIntegration :: Expectation
requireLiveIntegration = requireLiveFor "builtin search"

withFixture ::
    String -> ((App, AIStore.AIStore, ReactiveNotebook) -> IO a) -> IO a
withFixture label action =
    withSystemTempDirectory label $ \dir ->
        bracket (newFixture dir) releaseFixture action

releaseFixture :: (App, AIStore.AIStore, ReactiveNotebook) -> IO ()
releaseFixture (app, _, _) = forceResetAllSessions (appSessions app)

newFixture :: FilePath -> IO (App, AIStore.AIStore, ReactiveNotebook)
newFixture dir = do
    mgr <- newManager defaultManagerSettings
    app <- newApp dir Set.empty (Just mgr) Nothing [buildTimeSupportDir]
    rn <- setupReactive app
    let cfg =
            AnthropicConfig
                { acApiKey = ""
                , acModel = "placeholder"
                , acBaseUrl = "https://api.anthropic.com"
                }
    store <- AIStore.newAIStore cfg mgr
    pure (app, store, rn)

callTool ::
    App -> AIStore.AIStore -> ReactiveNotebook -> Text -> Value -> IO Value
callTool app store rn name input = do
    ct <- newCancelToken
    toolOutcomeValue <$> executeTool app store rn ct name input

matchNames :: Value -> [Text]
matchNames v =
    [ n
    | key <- ["matches", "hits"]
    , Just (Array rows) <- [field key v]
    , Object row <- toList rows
    , Just (String n) <- [KM.lookup (Key.fromText "name") row]
    ]
  where
    field k (Object o) = KM.lookup (Key.fromText k) o
    field _ _ = Nothing

spec :: Spec
spec = describe "the notebook's own vocabulary is keyword-findable" $ do
    it "picture-undisplayable: Picture's display route is findable" $ do
        requireLiveIntegration
        withFixture "sabela-picture-route" $ \(app, store, rn) -> do
            _ <-
                callTool
                    app
                    store
                    rn
                    "insert_cell"
                    (object ["source" .= ("sabelaWarm = (1 :: Int)" :: Text)])
            let findFn q =
                    matchNames
                        <$> callTool app store rn "find_function" (object ["query" .= (q :: Text)])
            names <- concat <$> mapM findFn ["Picture", "render", "svg", "display"]
            (names `elem'` "displayPicture") `shouldBe` True

    it "surfaces lineChart/plot/animate for a plain keyword (live_test13)" $ do
        requireLiveIntegration
        withFixture "sabela-builtin-search" $ \(app, store, rn) -> do
            _ <-
                callTool
                    app
                    store
                    rn
                    "insert_cell"
                    (object ["source" .= ("sabelaWarm = (1 :: Int)" :: Text)])
            let findFn q =
                    matchNames
                        <$> callTool app store rn "find_function" (object ["query" .= (q :: Text)])
            findFn "chart" `shouldReturn'` "lineChart"
            findFn "plot" `shouldReturn'` "plot"
            findFn "animate" `shouldReturn'` "animate"
  where
    shouldReturn' act name = do
        names <- act
        (name, names `elem'` name) `shouldBe` (name, True)
    elem' names n = n `elem` names || any (T.isInfixOf n) names
