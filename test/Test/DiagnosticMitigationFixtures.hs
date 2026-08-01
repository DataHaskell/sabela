{-# LANGUAGE OverloadedStrings #-}

module Test.DiagnosticMitigationFixtures (
    requireLiveIntegration,
    field,
    textField,
    classesOf,
    newFixture,
    callTool,
    insertSrc,
    bypassInsert,
    cellSourceOf,
    settledExecutionFor,
    mitigate,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set
import Data.Text (Text)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Test.Hspec (Expectation)

import Test.Live (requireLiveFor)

import Sabela.AI.Capabilities (executeTool)
import Sabela.AI.Capabilities.Edit.Cascade (executeWithRepair)
import Sabela.AI.Health (healthOfResult, isClean)
import qualified Sabela.AI.Store as AIStore
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.Anthropic.Types (AnthropicConfig (..), newCancelToken)
import Sabela.Handlers (ReactiveNotebook, checkedAppend, setupReactive)
import Sabela.Model
import Sabela.Server (newApp)
import Sabela.Session.Project (buildTimeSupportDir)
import Sabela.SessionTypes (CellLang (..))
import Sabela.State (App (..), atomicEditNotebook, freshCellId, readNotebook)

requireLiveIntegration :: Expectation
requireLiveIntegration = requireLiveFor "mitigation-gate integration"

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField k v = case field k v of
    Just (String s) -> Just s
    _ -> Nothing

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

insertSrc :: App -> AIStore.AIStore -> ReactiveNotebook -> Text -> IO Value
insertSrc app store rn src =
    callTool app store rn "insert_cell" (object ["source" .= src])

bypassInsert :: App -> Text -> IO Int
bypassInsert app src = do
    nid <- freshCellId (appNotebook app)
    let cell = Cell nid CodeCell Haskell src [] Nothing True
    result <- atomicEditNotebook (appNotebook app) $ \nb ->
        case checkedAppend cell nb of
            Left v -> (nb, Left v)
            Right nb' -> (nb', Right ())
    case result of
        Right () -> pure nid
        Left _ -> error "bypassInsert: checkedAppend rejected the fixture cell"

cellSourceOf :: App -> Int -> IO (Maybe Text)
cellSourceOf app cid = fmap cellSource . lookupCell cid <$> readNotebook (appNotebook app)

settledExecutionFor ::
    App ->
    AIStore.AIStore ->
    ReactiveNotebook ->
    Int ->
    Maybe Value ->
    IO (Maybe Value)
settledExecutionFor _ _ _ 0 _ = pure Nothing
settledExecutionFor app store rn n mCid = do
    v <- callTool app store rn "await_idle" (object [])
    case field "writes" v of
        Just (Array ws)
            | Just w <- findMaybe (matches mCid) (foldr (:) [] ws) ->
                pure (field "execution" w)
        _ -> settledExecutionFor app store rn (n - 1) mCid
  where
    matches cid w = field "cellId" w == cid
    findMaybe p = foldr (\x acc -> if p x then Just x else acc) Nothing

mitigate :: FilePath -> Text -> IO (Bool, Maybe Value, Maybe Text)
mitigate dir src = do
    (app, store, rn) <- newFixture dir
    _ <- insertSrc app store rn "sabelaWarmup = (1 :: Int)"
    cid <- bypassInsert app src
    ct <- newCancelToken
    (result, _suggestions, mitigations) <- executeWithRepair app store rn cid ct
    post <- cellSourceOf app cid
    pure (isClean (healthOfResult result), mitigations, post)

classesOf :: Value -> [Text]
classesOf v = case field "appliedInOrder" v of
    Just (Array xs) -> [c | fixV <- foldr (:) [] xs, Just c <- [field "class" fixV >>= asText]]
    _ -> []
  where
    asText (String s) = Just s
    asText _ = Nothing
