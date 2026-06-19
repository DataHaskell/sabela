{-# LANGUAGE OverloadedStrings #-}

{- | Pins the P0 AI-bridge admission layer: the four kernel-control tools
('kernel_status', 'interrupt', 'kernel_restart', 'export_notebook') are in
the catalogue and round-trip through the dispatcher, and admission control
short-circuits kernel-needing tools while the Haskell kernel is busy.
-}
module Test.AdmissionSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Unique (newUnique)

import Sabela.AI.Capabilities (admissionBlocked)
import Sabela.AI.Capabilities.ToolName (
    ToolName (..),
    parseToolName,
    toolWireName,
 )
import Sabela.AI.Capabilities.Tools (chatTools)
import Sabela.AI.Types (ToolOutcome, toolOutcomeIsError, toolOutcomeValue)
import Sabela.Anthropic.Types (ToolDef (..))
import Sabela.Server (newApp)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.SessionManager (setHaskellSession)
import Test.Hspec

newKernelTools :: [ToolName]
newKernelTools = [KernelStatus, Interrupt, KernelRestart, ExportNotebook]

-- | A fake backend whose busy state is fixed at construction.
fakeBackend :: Bool -> IO ST.SessionBackend
fakeBackend busy = do
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

mkApp :: Bool -> IO App
mkApp busy = do
    app <- newApp "." Set.empty Nothing Nothing []
    backend <- fakeBackend busy
    setHaskellSession (appSessions app) (Just backend)
    pure app

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

spec :: Spec
spec = do
    describe "kernel-control tool names" $ do
        it "every new tool parses from its wire name" $
            mapMaybe (parseToolName . toolWireName) newKernelTools
                `shouldBe` newKernelTools
        it "wire names are the documented strings" $
            map toolWireName newKernelTools
                `shouldBe` [ "kernel_status"
                           , "interrupt"
                           , "kernel_restart"
                           , "export_notebook"
                           ]
        it "all four appear in the chat catalogue" $
            let names = map tdName chatTools
             in map toolWireName newKernelTools
                    `shouldSatisfy` all (`elem` names)

    describe "admission control" $ do
        it "blocks a kernel-needing tool while the kernel is busy" $ do
            app <- mkApp True
            mOut <- admissionBlocked app ExecuteCell
            case mOut of
                Nothing -> expectationFailure "expected a busy outcome"
                Just out -> do
                    toolOutcomeIsError out `shouldBe` True
                    field "busy" (toolOutcomeValue out) `shouldBe` Just (Bool True)
        it "passes a kernel-needing tool through when the kernel is idle" $ do
            app <- mkApp False
            mOut <- admissionBlocked app GhciQuery
            (mOut :: Maybe ToolOutcome) `shouldBe` Nothing
        it "never blocks a kernel-control tool, even while busy" $ do
            app <- mkApp True
            mOut <- admissionBlocked app KernelStatus
            mOut `shouldBe` Nothing
        it "never blocks a read-only tool, even while busy" $ do
            app <- mkApp True
            mOut <- admissionBlocked app ListCells
            mOut `shouldBe` Nothing
