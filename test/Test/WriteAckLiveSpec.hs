{-# LANGUAGE OverloadedStrings #-}

module Test.WriteAckLiveSpec (spec) where

import Control.Exception (bracket, bracket_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import System.Directory (doesFileExist, findExecutable, makeAbsolute)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Sabela.AI.Capabilities (executeTool)
import qualified Sabela.AI.Store as AIStore
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.Anthropic.Types (AnthropicConfig (..), newCancelToken)
import Sabela.Handlers (ReactiveNotebook, setupReactive)
import Sabela.Model (Notebook (..))
import Sabela.Server (newApp)
import Sabela.State (App (..), forceResetAllSessions, readNotebook)
import Test.Hspec

withLiveEnv :: IO a -> IO a
withLiveEnv =
    bracket_
        (setEnv "SABELA_WRITE_ACK_SECS" "2")
        (unsetEnv "SABELA_WRITE_ACK_SECS")

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField k v = case field k v of
    Just (String s) -> Just s
    _ -> Nothing

callTool ::
    App -> AIStore.AIStore -> ReactiveNotebook -> Text -> Value -> IO Value
callTool app store rn name input = do
    ct <- newCancelToken
    toolOutcomeValue <$> executeTool app store rn ct name input

insertSrc :: App -> AIStore.AIStore -> ReactiveNotebook -> Text -> IO Value
insertSrc app store rn src =
    callTool app store rn "insert_cell" (object ["source" .= src])

awaitWrites :: App -> AIStore.AIStore -> ReactiveNotebook -> Int -> IO [Value]
awaitWrites _ _ _ 0 = pure []
awaitWrites app store rn n = do
    v <- callTool app store rn "await_idle" (object [])
    case field "writes" v of
        Just (Array ws) -> pure (foldr (:) [] ws)
        _ -> awaitWrites app store rn (n - 1)

slowSrc :: Text
slowSrc =
    "import Control.Concurrent\n\
    \main = threadDelay 20000000 >> putStrLn \"write-ack-done\""

spec :: Spec
spec = describe "R10(c) live-kernel write-ack" $
    it "slow cell acks executing; retry never duplicates; await_idle reconciles" $ do
        mGhc <- findExecutable "ghc"
        case mGhc of
            Nothing -> pendingWith "ghc not on PATH"
            Just _ -> withLiveEnv $
                withFixture "sabela-writeack" $ \(app, store, rn) -> do
                    _ <- insertSrc app store rn "sabelaWarmup = (1 :: Int)"
                    _ <- awaitWrites app store rn 8
                    t0 <- getMonotonicTimeNSec
                    ack <- insertSrc app store rn slowSrc
                    t1 <- getMonotonicTimeNSec
                    ((t1 - t0) < 15000000000) `shouldBe` True
                    textField "status" ack `shouldBe` Just "executing"
                    let mCid = field "cellId" ack
                    isJust mCid `shouldBe` True
                    countBefore <- length . nbCells <$> readNotebook (appNotebook app)
                    retry <- insertSrc app store rn slowSrc
                    field "duplicate" retry `shouldBe` Just (Bool True)
                    field "cellId" retry `shouldBe` mCid
                    countAfter <- length . nbCells <$> readNotebook (appNotebook app)
                    countAfter `shouldBe` countBefore
                    writes <- awaitWrites app store rn 8
                    (field "cellId" <$> writes) `shouldBe` [mCid]
                    (textField "status" =<< headMaybe writes)
                        `shouldBe` Just "completed"

headMaybe :: [a] -> Maybe a
headMaybe (x : _) = Just x
headMaybe [] = Nothing

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
    localPkgs <- supportOverlay
    app <- newApp dir Set.empty (Just mgr) Nothing localPkgs
    rn <- setupReactive app
    let cfg =
            AnthropicConfig
                { acApiKey = ""
                , acModel = "placeholder"
                , acBaseUrl = "https://api.anthropic.com"
                }
    store <- AIStore.newAIStore cfg mgr
    pure (app, store, rn)

supportOverlay :: IO [FilePath]
supportOverlay = do
    present <- doesFileExist ("sabela-notebook" </> "sabela-notebook.cabal")
    if present
        then (: []) <$> makeAbsolute "sabela-notebook"
        else pure []
