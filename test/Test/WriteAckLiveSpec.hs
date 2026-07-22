{-# LANGUAGE OverloadedStrings #-}

{- | The R10(c) live-kernel check for R6.1/R6.2: against a REAL GHCi kernel
and the real reactive pipeline, a deliberately long-running cell gets
@{cellId, status: executing}@ well under the transport timeout, retrying
the same write does not duplicate the cell, and @await_idle@ later
reconciles the settled outcome. Skipped when ghc is not on PATH.
-}
module Test.WriteAckLiveSpec (spec) where

import Control.Exception (bracket_)
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
import Sabela.State (App (..), readNotebook)
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

{- | Loop @await_idle@ (bounded) until it either drains a settled write for
the cell or the kernel reports idle with nothing pending. Returns the drained
writes seen.
-}
awaitWrites :: App -> AIStore.AIStore -> ReactiveNotebook -> Int -> IO [Value]
awaitWrites _ _ _ 0 = pure []
awaitWrites app store rn n = do
    v <- callTool app store rn "await_idle" (object [])
    case field "writes" v of
        Just (Array ws) -> pure (foldr (:) [] ws)
        _ -> awaitWrites app store rn (n - 1)

-- | A cell that sleeps 8s: long enough to outlive the 2s ack deadline.
slowSrc :: Text
slowSrc =
    "import Control.Concurrent\n\
    \main = threadDelay 8000000 >> putStrLn \"write-ack-done\""

spec :: Spec
spec = describe "R10(c) live-kernel write-ack" $
    it "slow cell acks executing; retry never duplicates; await_idle reconciles" $ do
        mGhc <- findExecutable "ghc"
        case mGhc of
            Nothing -> pendingWith "ghc not on PATH"
            Just _ -> withLiveEnv $
                withSystemTempDirectory "sabela-writeack" $ \dir -> do
                    mgr <- newManager defaultManagerSettings
                    -- The repl project needs the sabela-notebook overlay the
                    -- production main resolves; without it GHCi cannot spawn.
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
                    -- Warm the kernel through a trivial write so the slow
                    -- insert's timing is not dominated by the cold spawn.
                    _ <- insertSrc app store rn "sabelaWarmup = (1 :: Int)"
                    _ <- awaitWrites app store rn 8
                    t0 <- getMonotonicTimeNSec
                    ack <- insertSrc app store rn slowSrc
                    t1 <- getMonotonicTimeNSec
                    -- Well under the 60s transport timeout AND under the
                    -- cell's own 8s runtime: the ack outran execution.
                    ((t1 - t0) < 7000000000) `shouldBe` True
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

-- | The repo's sabela-notebook dir as a local package overlay, when present.
supportOverlay :: IO [FilePath]
supportOverlay = do
    present <- doesFileExist ("sabela-notebook" </> "sabela-notebook.cabal")
    if present
        then (: []) <$> makeAbsolute "sabela-notebook"
        else pure []
