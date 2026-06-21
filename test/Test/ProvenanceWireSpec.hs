{-# LANGUAGE OverloadedStrings #-}

{- | Pins the Part 7 server-side provenance record. Two guarantees:

* A 'SessionEvent' round-trips through @toJSON@/@fromJSON@ preserving the
  reused contract values (the typed 'ToolName', the 'ToolOutcome' isError
  axis, the 'KernelState' tag), the @(session, gen)@ correlation key, and the
  actor tag.
* Driving the REAL @aiToolH@ appends one JSONL record AND leaves the
  @{isError,result}@ response byte-identical whether or not a log dir is set.
-}
module Test.ProvenanceWireSpec (spec) where

import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.Unique (newUnique)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant (runHandler)
import System.Directory (doesFileExist)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.KernelState (Activity (..), KernelState (..))
import Sabela.AI.Provenance (
    Actor (..),
    SessionEvent (..),
    actorTag,
    recordToolCall,
    sessionLogPath,
 )
import Sabela.AI.Types (ToolOutcome (..), toolOutcomeIsError)
import Sabela.Server (newApp)
import Sabela.Server.Ai (aiToolH)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import Sabela.State.SessionManager (setHaskellSession)
import Test.Hspec

sampleEvent :: SessionEvent
sampleEvent =
    SessionEvent
        { seAt = UTCTime (fromGregorian 2026 6 20) (secondsToDiffTime 3600)
        , seSession = "siza-abc123"
        , seNotebook = "my-notebook"
        , seActor = Agent
        , seCall = ExecuteCell
        , seInput = object ["cell_id" .= (3 :: Int)]
        , seOutcome = ToolErr (object ["busy" .= True])
        , seKernelBefore =
            Alive{ksGen = 2, ksActivity = Executing, ksBuilding = True}
        , seGen = 7
        , sePrev = Nothing
        }

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

-- | An inert backend so 'aiToolH' can run a read-only tool with no kernel.
inertBackend :: IO ST.SessionBackend
inertBackend = do
    uid <- newUnique
    let backend =
            ST.SessionBackend
                { ST.sbSessionId = uid
                , ST.sbRunBlock = \_ -> pure ("", "")
                , ST.sbRunBlockStreaming = \_ _ -> pure ("", "")
                , ST.sbClose = pure ()
                , ST.sbReset = pure backend
                , ST.sbInterrupt = pure ()
                , ST.sbBusy = pure False
                , ST.sbSessionGen = pure 0
                , ST.sbRequestStale = \_ -> pure False
                , ST.sbQueryComplete = \_ -> pure []
                , ST.sbQueryType = \_ -> pure ""
                , ST.sbQueryInfo = \_ -> pure ""
                , ST.sbQueryKind = \_ -> pure ""
                , ST.sbQueryBrowse = \_ -> pure ""
                , ST.sbQueryDoc = \_ -> pure ""
                }
    pure backend

mkApp :: FilePath -> IO App
mkApp workDir = do
    mgr <- newManager defaultManagerSettings
    app <- newApp workDir Set.empty (Just mgr) Nothing []
    backend <- inertBackend
    setHaskellSession (appSessions app) (Just backend)
    pure app

{- | @export_notebook@ needs no kernel and never errors, so the response is a
stable target for the byte-identity assertion.
-}
callExport :: App -> Maybe Text -> IO Value
callExport app mSession = do
    let body = object ["name" .= ("export_notebook" :: Text), "input" .= object []]
    res <- runHandler (aiToolH app inertRn mSession body)
    either (error . show) pure res
  where
    inertRn = error "rn unused for export_notebook"

spec :: Spec
spec = describe "server-side provenance (Part 7)" $ do
    describe "SessionEvent JSON round-trip" $ do
        it "preserves every reused contract value through encode/decode" $ do
            let decoded = decode (encode sampleEvent) :: Maybe SessionEvent
            case decoded of
                Nothing -> expectationFailure "SessionEvent failed to decode"
                Just ev -> do
                    seSession ev `shouldBe` "siza-abc123"
                    seGen ev `shouldBe` 7
                    seCall ev `shouldBe` ExecuteCell
                    toolOutcomeIsError (seOutcome ev) `shouldBe` True
                    seKernelBefore ev
                        `shouldBe` Alive 2 Executing True
                    seActor ev `shouldBe` Agent

        it "carries the (session, gen) correlation key on the wire" $ do
            let v = either error id (decodeValue (encode sampleEvent))
            field "session" v `shouldBe` Just (String "siza-abc123")
            field "gen" v `shouldBe` Just (Number 7)

        it "tags the actor" $
            field "actor" (either error id (decodeValue (encode sampleEvent)))
                `shouldBe` Just (String (actorTag Agent))

        it "round-trips a Cold kernel before-state" $ do
            let ev = sampleEvent{seKernelBefore = Cold}
            (seKernelBefore <$> (decode (encode ev) :: Maybe SessionEvent))
                `shouldBe` Just Cold

    describe "aiToolH log side effect is additive (response unchanged)" $ do
        it "appends one JSONL record under XDG_STATE_HOME" $
            withSystemTempDirectory "siza-prov" $ \tmp -> do
                setEnv "XDG_STATE_HOME" tmp
                app <- mkApp "."
                _ <- callExport app (Just "siza-xyz")
                path <- sessionLogPath (envWorkDir (appEnv app)) "siza-xyz"
                doesFileExist path `shouldReturn` True
                logged <- LBS.lines <$> LBS.readFile path
                length logged `shouldBe` 1
                let v = either error id (decodeValue (head logged))
                field "tool" v `shouldBe` Just (String "export_notebook")
                field "session" v `shouldBe` Just (String "siza-xyz")
                unsetEnv "XDG_STATE_HOME"

        it "returns the same {isError,result} when the log write fails" $
            withSystemTempDirectory "siza-prov" $ \tmp -> do
                setEnv "XDG_STATE_HOME" tmp
                app1 <- mkApp "."
                vWithLog <- callExport app1 (Just "with-log-home")
                -- Point XDG at a regular FILE so the log dir cannot be created:
                -- recordEvent must swallow the IO failure and leave the wire intact.
                let blocker = tmp </> "blocker"
                writeFile blocker ""
                setEnv "XDG_STATE_HOME" blocker
                app2 <- mkApp "."
                vBlocked <- callExport app2 (Just "blocked-home")
                field "isError" vBlocked `shouldBe` field "isError" vWithLog
                field "result" vBlocked `shouldBe` field "result" vWithLog
                unsetEnv "XDG_STATE_HOME"

    describe "in-browser chat records its own provenance (C-1)" $
        it "tags actor=InBrowserChat under the browser-session sentinel" $
            withSystemTempDirectory "siza-prov" $ \tmp -> do
                setEnv "XDG_STATE_HOME" tmp
                -- The chat path passes Nothing for the session (no
                -- X-Sabela-Session header), so the log defaults to "browser".
                recordToolCall
                    "."
                    Nothing
                    InBrowserChat
                    "list_cells"
                    (object [])
                    (ToolOk (object ["ok" .= True]))
                    Cold
                    0
                path <- sessionLogPath "." "browser"
                doesFileExist path `shouldReturn` True
                logged <- LBS.lines <$> LBS.readFile path
                length logged `shouldBe` 1
                let v = either error id (decodeValue (head logged))
                field "actor" v `shouldBe` Just (String (actorTag InBrowserChat))
                field "session" v `shouldBe` Just (String "browser")
                field "tool" v `shouldBe` Just (String "list_cells")
                unsetEnv "XDG_STATE_HOME"

decodeValue :: LBS.ByteString -> Either String Value
decodeValue = maybe (Left "not JSON") Right . decode
