{-# LANGUAGE OverloadedStrings #-}

{- | The notebook's own vocabulary must be keyword-findable. Against a REAL
kernel: @find_function@ indexes the bare import-completion list, which is
alphabetical and truncated, so @Sabela.*@ fell off the end and no builtin was
reachable by keyword — live_test13's model hand-rolled SVG because @plot@,
@lineChart@ and @displayPicture@ returned nothing for any natural query.
-}
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
import System.Directory (doesFileExist, findExecutable)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Sabela.AI.Capabilities (executeTool)
import qualified Sabela.AI.Store as AIStore
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.Anthropic.Types (AnthropicConfig (..), newCancelToken)
import Sabela.Handlers (ReactiveNotebook, setupReactive)
import Sabela.Server (newApp)
import Sabela.Session.Project (buildTimeSupportDir)
import Sabela.State (App (..), forceResetAllSessions)

requireLiveIntegration :: IO ()
requireLiveIntegration = do
    cabal <- findExecutable "cabal"
    case cabal of
        Nothing -> pendingWith "cabal not found on PATH; skipping builtin search"
        Just _ -> pure ()
    present <- doesFileExist (buildTimeSupportDir </> "sabela-notebook.cabal")
    if present
        then pure ()
        else pendingWith "sabela-notebook support source not on disk; skipping"

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

-- | The names @find_function@ returned, whatever the wire calls the array.
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
spec = describe "the notebook's own vocabulary is keyword-findable" $
    it "surfaces lineChart/plot/animate for a plain keyword (live_test13)" $ do
        requireLiveIntegration
        withFixture "sabela-builtin-search" $ \(app, store, rn) -> do
            -- Any kernel-needing call warms GHCi; the index needs a session.
            _ <- callTool app store rn "insert_cell" (object ["source" .= ("sabelaWarm = (1 :: Int)" :: Text)])
            let findFn q = matchNames <$> callTool app store rn "find_function" (object ["query" .= (q :: Text)])
            -- "chart" matches no Sabela name exactly; before the fix it
            -- returned nothing at all.
            findFn "chart" `shouldReturn'` "lineChart"
            findFn "plot" `shouldReturn'` "plot"
            findFn "animate" `shouldReturn'` "animate"
  where
    shouldReturn' act name = do
        names <- act
        (name, names `elem'` name) `shouldBe` (name, True)
    elem' names n = n `elem` names || any (T.isInfixOf n) names
