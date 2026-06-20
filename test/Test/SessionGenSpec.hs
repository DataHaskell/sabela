{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Pins the two-axis kernel status and the session-generation tag (P1
stress cases 35, 36). 'kernel_status' reports both a busy axis and a
generation axis, so a client can tell "busy/slow" from "wedged" and also
detect that a restart advanced the generation (discarding stale results).
The generation is born at 'firstSessionGen' and 'bumpSessionGen' seeds a
fresh, strictly-higher generation on restart. Pure logic over a dummy
'Session' plus a fake 'SessionBackend'; no live kernel.
-}
module Test.SessionGenSpec (spec) where

import Control.Concurrent.MVar (newMVar)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (newIORef, readIORef)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Unique (newUnique)
import Sabela.AI.Capabilities.Kernel (execKernelStatus)
import Sabela.AI.Types (toolOutcomeValue)
import Sabela.Server (newApp)
import Sabela.Session (Session (..), SessionConfig (..), readSessionGen)
import Sabela.Session.Proc (ProcSession (..))
import Sabela.Session.Process (bumpSessionGen, firstSessionGen)
import Sabela.Session.Reader (newOutQueue)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), setBuilding)
import Sabela.State.SessionManager (setHaskellSession)
import Test.Hspec (Spec, describe, it, shouldBe)

defaultCfg :: SessionConfig
defaultCfg =
    SessionConfig
        { scProjectDir = "."
        , scWorkDir = "."
        , scExecutionTimeoutUs = 120_000_000
        , scResyncTimeoutUs = 5_000_000
        }

dummySession :: Int -> IO Session
dummySession g = do
    q <- newOutQueue
    lock <- newMVar ()
    queryLock <- newMVar ()
    errRef <- newIORef []
    ctrRef <- newIORef 0
    cbRef <- newIORef (\_ -> pure ())
    killLock <- newMVar ()
    uid <- newUnique
    busy <- newIORef False
    lastIntRef <- newIORef Nothing
    gen <- newIORef g
    let ps =
            ProcSession
                { psId = uid
                , psProc = error "dummySession: psProc"
                , psPgid = Nothing
                , psKillLock = killLock
                , psStdin = error "dummySession: psStdin"
                , psStdout = error "dummySession: psStdout"
                , psStderr = error "dummySession: psStderr"
                , psQueue = q
                }
    pure
        Session
            { sessProcSess = ps
            , sessLock = lock
            , sessQueryLock = queryLock
            , sessErrBuf = errRef
            , sessCounter = ctrRef
            , sessConfig = defaultCfg
            , sessErrCallback = cbRef
            , sessBusy = busy
            , sessNonce = 12_345
            , sessLastInterruptTime = lastIntRef
            , sessionGen = gen
            }

-- | A fake backend with a fixed busy state and generation.
fakeBackend :: Bool -> Int -> IO ST.SessionBackend
fakeBackend busy g = do
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
                , ST.sbSessionGen = pure g
                , ST.sbRequestStale = \_ -> pure False
                , ST.sbQueryComplete = \_ -> pure []
                , ST.sbQueryType = \_ -> pure ""
                , ST.sbQueryInfo = \_ -> pure ""
                , ST.sbQueryKind = \_ -> pure ""
                , ST.sbQueryBrowse = \_ -> pure ""
                , ST.sbQueryDoc = \_ -> pure ""
                }
    pure backend

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

statusWith :: Bool -> Int -> IO Value
statusWith busy g = do
    app <- newApp "." Set.empty Nothing Nothing []
    backend <- fakeBackend busy g
    setHaskellSession (appSessions app) (Just backend)
    toolOutcomeValue <$> execKernelStatus app

spec :: Spec
spec = do
    describe "session generation (stress case 36)" $ do
        it "is born at firstSessionGen" $
            firstSessionGen `shouldBe` 1

        it "reads the live generation tag" $ do
            sess <- dummySession firstSessionGen
            readSessionGen sess `shouldReturnVal` firstSessionGen

        it "bumpSessionGen seeds a strictly higher generation" $ do
            sess <- dummySession firstSessionGen
            g1 <- readSessionGen sess
            g2 <- bumpSessionGen sess
            (g2 > g1) `shouldBe` True
            g2 `shouldBe` g1 + 1

        it "repeated bumps keep advancing" $ do
            sess <- dummySession firstSessionGen
            _ <- bumpSessionGen sess
            _ <- bumpSessionGen sess
            g <- bumpSessionGen sess
            g `shouldBe` firstSessionGen + 3

    describe "two-axis kernel_status wire shape (stress cases 35, 36)" $ do
        it "reports kernel alive, running, and sessionGen" $ do
            v <- statusWith True 7
            field "kernel" v `shouldBe` Just (String "alive")
            field "running" v `shouldBe` Just (Bool True)
            field "sessionGen" v `shouldBe` Just (Number 7)

        it "running axis is independent of the generation axis" $ do
            v <- statusWith False 3
            field "running" v `shouldBe` Just (Bool False)
            field "sessionGen" v `shouldBe` Just (Number 3)

        it "absent kernel reports gen 0, not alive" $ do
            app <- newApp "." Set.empty Nothing Nothing []
            v <- toolOutcomeValue <$> execKernelStatus app
            field "kernel" v `shouldBe` Just (String "absent")
            field "running" v `shouldBe` Just (Bool False)
            field "sessionGen" v `shouldBe` Just (Number 0)

        it "reports a compiling axis, false at rest" $ do
            v <- statusWith False 3
            field "compiling" v `shouldBe` Just (Bool False)

        it "compiling axis is independent of running — raised by off-lock builds" $ do
            app <- newApp "." Set.empty Nothing Nothing []
            setBuilding app True
            v <- toolOutcomeValue <$> execKernelStatus app
            field "compiling" v `shouldBe` Just (Bool True)
            field "running" v `shouldBe` Just (Bool False)

        it "the kernel_status object carries exactly the four documented keys" $ do
            v <- statusWith True 1
            v
                `shouldBe` object
                    [ "kernel" .= ("alive" :: Text)
                    , "running" .= True
                    , "compiling" .= False
                    , "sessionGen" .= (1 :: Int)
                    ]
  where
    shouldReturnVal act expected = act >>= (`shouldBe` expected)
