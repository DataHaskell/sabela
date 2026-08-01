{-# LANGUAGE OverloadedStrings #-}

{- | A live App the write gates can be driven through: a temp work dir, a real
reactive notebook, and the tool entry point insert_cell goes through.
-}
module Test.GateFixture (
    withFixture,
    callTool,
    insertSrc,
    cellCount,
    generationOf,
    sessionIdentity,
    withBuildTimeout,
    field,
    textField,
) where

import Control.Exception (bracket, bracket_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (readIORef)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Unique (hashUnique)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import System.Environment (setEnv, unsetEnv)
import System.IO.Temp (withSystemTempDirectory)

import Sabela.AI.Capabilities (executeTool)
import qualified Sabela.AI.Store as AIStore
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.Anthropic.Types (AnthropicConfig (..), newCancelToken)
import Sabela.Handlers (ReactiveNotebook, setupReactive)
import Sabela.Model (Notebook (..))
import Sabela.Server (newApp)
import Sabela.Session.Project (buildTimeSupportDir)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), readNotebook)
import Sabela.State.EventBus (EventBus (..))
import Sabela.State.SessionManager (
    forceResetAllSessions,
    getHaskellSession,
 )

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField k v = case field k v of
    Just (String s) -> Just s
    _ -> Nothing

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

insertSrc :: App -> AIStore.AIStore -> ReactiveNotebook -> Text -> IO Value
insertSrc app store rn src =
    callTool app store rn "insert_cell" (object ["source" .= src])

cellCount :: App -> IO Int
cellCount app = length . nbCells <$> readNotebook (appNotebook app)

generationOf :: App -> IO Int
generationOf app = readIORef (ebGeneration (appEvents app))

sessionIdentity :: App -> IO (Maybe Int)
sessionIdentity app =
    fmap (hashUnique . ST.sbSessionId) <$> getHaskellSession (appSessions app)

withBuildTimeout :: String -> IO a -> IO a
withBuildTimeout secs =
    bracket_
        ( setEnv "SABELA_TRY_BUILD_TIMEOUT_SECONDS" secs
            >> setEnv "SABELA_BUILD_TIMEOUT_SECONDS" secs
        )
        ( unsetEnv "SABELA_TRY_BUILD_TIMEOUT_SECONDS"
            >> unsetEnv "SABELA_BUILD_TIMEOUT_SECONDS"
        )
