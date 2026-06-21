{-# LANGUAGE OverloadedStrings #-}

{- | Drives the real 'execKernelStatus' producer (design 1.1, slice R2-1):
the typed @state@\/@ksGen@\/@ebGeneration@ fields ARE the wire, and the legacy
@kernel\/running\/compiling\/sessionGen@ blob is gone — these specs assert its
absence. Pinned against the producer, not a hand-built object.
-}
module Test.KernelStateWireSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (writeIORef)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Unique (newUnique)

import Sabela.AI.Capabilities.Kernel (execKernelStatus)
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.Server (newApp)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), setBuilding)
import Sabela.State.EventBus (EventBus (..))
import Sabela.State.SessionManager (setHaskellSession)
import Test.Hspec

-- | A fake backend with fixed busy state and session generation.
fakeBackend :: Bool -> Int -> IO ST.SessionBackend
fakeBackend busy gen = do
    uid <- newUnique
    let backend =
            ST.SessionBackend
                { ST.sbSessionId = uid
                , ST.sbRunBlock = \_ -> pure ("", "")
                , ST.sbRunBlockStreaming = \_ _ -> pure ("", "")
                , ST.sbClose = pure ()
                , ST.sbReset = pure backend
                , ST.sbInterrupt = pure ()
                , ST.sbBusy = pure busy
                , ST.sbSessionGen = pure gen
                , ST.sbRequestStale = \_ -> pure False
                , ST.sbQueryComplete = \_ -> pure []
                , ST.sbQueryType = \_ -> pure ""
                , ST.sbQueryInfo = \_ -> pure ""
                , ST.sbQueryKind = \_ -> pure ""
                , ST.sbQueryBrowse = \_ -> pure ""
                , ST.sbQueryDoc = \_ -> pure ""
                }
    pure backend

-- | App with no Haskell session attached (kernel absent → Cold).
coldApp :: IO App
coldApp = newApp "." Set.empty Nothing Nothing []

-- | App with a fake session of the given busy\/gen\/building\/ebGen reads.
liveApp :: Bool -> Int -> Bool -> Int -> IO App
liveApp busy gen building ebGen = do
    app <- newApp "." Set.empty Nothing Nothing []
    backend <- fakeBackend busy gen
    setHaskellSession (appSessions app) (Just backend)
    setBuilding app building
    writeIORef (ebGeneration (appEvents app)) ebGen
    pure app

statusValue :: App -> IO Value
statusValue app = toolOutcomeValue <$> execKernelStatus app

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

statePath :: Text -> Value -> Maybe Value
statePath k v = field "state" v >>= field k

spec :: Spec
spec = describe "kernel_status typed state (execKernelStatus)" $ do
    it "emits only the typed fields — the legacy 4-field blob is gone" $ do
        app <- liveApp False 0 False 0
        v <- statusValue app
        statePath "state" v `shouldBe` Just (String "idle")
        field "ksGen" v `shouldBe` Just (Number 0)
        mapM_
            (\k -> field k v `shouldBe` Nothing)
            ["kernel", "running", "compiling", "sessionGen"]

    it "an absent session reports Cold, not gen-inferred idle" $ do
        app <- coldApp
        v <- statusValue app
        field "kernel" v `shouldBe` Nothing
        statePath "state" v `shouldBe` Just (String "cold")

    it "alive && !busy && !compiling is idle" $ do
        app <- liveApp False 3 False 0
        v <- statusValue app
        statePath "state" v `shouldBe` Just (String "idle")

    it "alive && busy is executing" $ do
        app <- liveApp True 3 False 0
        v <- statusValue app
        statePath "state" v `shouldBe` Just (String "executing")

    it "alive && compiling (idle run-lock) is building" $ do
        app <- liveApp False 3 True 0
        v <- statusValue app
        statePath "state" v `shouldBe` Just (String "building")

    it "building and executing co-occur: neither swallows the other" $ do
        app <- liveApp True 3 True 0
        v <- statusValue app
        statePath "state" v `shouldBe` Just (String "executing")
        statePath "building" v `shouldBe` Just (Bool True)

    it "ksGen is present on every Alive state (== sessionGen)" $ do
        app <- liveApp True 7 False 0
        v <- statusValue app
        statePath "ksGen" v `shouldBe` Just (Number 7)
        field "ksGen" v `shouldBe` Just (Number 7)

    it "ebGeneration is a distinct field, separate from ksGen" $ do
        app <- liveApp False 7 False 42
        v <- statusValue app
        field "ksGen" v `shouldBe` Just (Number 7)
        field "ebGeneration" v `shouldBe` Just (Number 42)
        (field "ksGen" v == field "ebGeneration" v) `shouldBe` False

    it "carries only the typed keys; the legacy keys are absent" $ do
        app <- liveApp False 0 False 0
        v <- statusValue app
        let present k = field k v `shouldNotBe` Nothing
            absent k = field k v `shouldBe` Nothing
        mapM_ present ["state", "ksGen", "ebGeneration"]
        mapM_ absent ["kernel", "running", "compiling", "sessionGen"]
