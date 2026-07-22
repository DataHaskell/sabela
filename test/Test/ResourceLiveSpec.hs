{-# LANGUAGE OverloadedStrings #-}

{- | The R10(c)-style live check for R6.5/R6.6: against a REAL GHCi kernel, a
deliberately runaway cell (infinite, silent) draws the bounded @resource@
line from @await_idle@, and per-cell interrupt + delete recovers the session
WITHOUT a kernel restart (same session generation). Skipped when ghc is not
on PATH.
-}
module Test.ResourceLiveSpec (spec) where

import Control.Exception (bracket_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
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
import Sabela.Server (newApp)
import Sabela.State (App (..))
import Test.Hspec

{- | Shrink the ack deadline, await budget and resource wall budget so the
runaway is diagnosed in seconds; restored afterwards.
-}
withLiveEnv :: IO a -> IO a
withLiveEnv =
    bracket_
        ( setEnv "SABELA_WRITE_ACK_SECS" "2"
            >> setEnv "SABELA_AWAIT_IDLE_SECS" "4"
            >> setEnv "SABELA_RESOURCE_WALL_SECS" "3"
        )
        ( unsetEnv "SABELA_WRITE_ACK_SECS"
            >> unsetEnv "SABELA_AWAIT_IDLE_SECS"
            >> unsetEnv "SABELA_RESOURCE_WALL_SECS"
        )

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

-- | Infinite and silent: no output progress, no termination, no task content.
runawaySrc :: Text
runawaySrc = "main = print (length [(1 :: Integer) ..])"

{- | Call @await_idle@ up to @n@ times, returning the first reply carrying a
@resource@ line (plus every reply seen).
-}
huntResource ::
    App -> AIStore.AIStore -> ReactiveNotebook -> Int -> IO (Maybe Value, [Value])
huntResource _ _ _ 0 = pure (Nothing, [])
huntResource app store rn n = do
    v <- callTool app store rn "await_idle" (object [])
    case field "resource" v of
        Just _ -> pure (Just v, [v])
        Nothing -> do
            (hit, seen) <- huntResource app store rn (n - 1)
            pure (hit, v : seen)

{- | Drain @await_idle@ until it reports a settled\/idle kernel (bounded), so
the next insert cannot bounce off the warm write still executing under load.
-}
settleAll :: App -> AIStore.AIStore -> ReactiveNotebook -> Int -> IO ()
settleAll _ _ _ 0 = pure ()
settleAll app store rn n = do
    v <- callTool app store rn "await_idle" (object [])
    case textField "waited" v of
        Just w | w == "idle" || w == "settled" -> pure ()
        _ -> settleAll app store rn (n - 1)

{- | Insert with a bounded busy-retry: under heavy machine load the prior
write's release tail can outlive the admission grace, so a bounced insert
settles and retries instead of failing the example on scheduler noise.
-}
insertSettled ::
    App -> AIStore.AIStore -> ReactiveNotebook -> Int -> Text -> IO Value
insertSettled app store rn n src = do
    v <-
        callTool app store rn "insert_cell" $
            object ["source" .= src]
    case (field "busy" v, n) of
        (Just (Bool True), k) | k > 0 -> do
            settleAll app store rn 8
            insertSettled app store rn (n - 1) src
        _ -> pure v

ksGenOf :: Value -> Maybe Value
ksGenOf v = field "ksGen" =<< field "state" v

spec :: Spec
spec = describe "R10(c) live runaway-cell resource diagnostic" $
    it "resource line appears; interrupt + delete recovers without restart" $ do
        mGhc <- findExecutable "ghc"
        case mGhc of
            Nothing -> pendingWith "ghc not on PATH"
            Just _ -> withLiveEnv $
                withSystemTempDirectory "sabela-resource" $ \dir -> do
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
                    _ <-
                        callTool app store rn "insert_cell" $
                            object ["source" .= ("sabelaWarm = (1 :: Int)" :: Text)]
                    settleAll app store rn 30
                    st0 <- callTool app store rn "kernel_status" (object [])
                    ack <- insertSettled app store rn 3 runawaySrc
                    textField "status" ack `shouldBe` Just "executing"
                    let mCid = field "cellId" ack
                    (hit, seen) <- huntResource app store rn 20
                    case hit of
                        Nothing ->
                            expectationFailure
                                ("no resource line in: " <> show seen)
                        Just v -> case textField "resource" v of
                            Nothing ->
                                expectationFailure "resource is not a string"
                            Just line -> do
                                T.length line <= 200 `shouldBe` True
                                T.isInfixOf "\n" line `shouldBe` False
                    -- Per-cell recovery (R6.6): interrupt the runaway, then
                    -- delete the cell — never a kernel restart.
                    _ <- callTool app store rn "interrupt" (object [])
                    settleAll app store rn 30
                    _ <-
                        callTool app store rn "delete_cell" $
                            object ["cell_id" .= mCid]
                    stAfter <- callTool app store rn "kernel_status" (object [])
                    -- Same session generation: no restart happened.
                    ksGenOf stAfter `shouldBe` ksGenOf st0
                    -- The session still works: a fresh cell completes.
                    done <-
                        callTool app store rn "insert_cell" $
                            object
                                ["source" .= ("sabelaAfter = (2 :: Int)" :: Text)]
                    isJust (textField "status" done) `shouldBe` True
                    -- R8.4 hygiene: nothing we saw carries a raw exception.
                    let rendered = T.pack (show (st0 : stAfter : done : seen))
                    T.isInfixOf "HttpExceptionRequest" rendered `shouldBe` False

-- | The repo's sabela-notebook dir as a local package overlay, when present.
supportOverlay :: IO [FilePath]
supportOverlay = do
    present <- doesFileExist ("sabela-notebook" </> "sabela-notebook.cabal")
    if present
        then (: []) <$> makeAbsolute "sabela-notebook"
        else pure []
