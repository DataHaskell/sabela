{-# LANGUAGE OverloadedStrings #-}

module Test.ThrowawayExecuteSpec (spec) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Test.Hspec

import Sabela.ThrowawayExecute

spec :: Spec
spec = describe "throwaway execute fail-closed gate" $ do
    it "round-trips arbitrary protocol payloads through a length-delimited frame" $ do
        mapM_
            (\payload -> decodeFrame (encodeFrame payload) `shouldBe` Right payload)
            ["", "42", "line one\nline two", "\955\128\57856"]

    it "rejects an unmatched helper without invoking candidate execution" $ do
        invoked <- newIORef False
        result <-
            admitExecute
                (qualified "9.12.2" "9.10.1")
                (writeIORef invoked True)
        executeVerdict result `shouldBe` ExecuteUnavailable
        executeReason result `shouldBe` Just HelperVersionMismatch
        readIORef invoked `shouldReturn` False

    it "rejects an unqualified sandbox without invoking candidate execution" $ do
        invoked <- newIORef False
        result <-
            admitExecute
                (qualified "9.12.2" "9.12.2")
                    { qualificationContainment = ContainmentUnproven
                    }
                (writeIORef invoked True)
        executeVerdict result `shouldBe` ExecuteUnavailable
        executeReason result `shouldBe` Just ContainmentNotProven
        readIORef invoked `shouldReturn` False

    it "keeps the execute feature flag default-off" $ do
        executeFlagEnabled Nothing `shouldBe` False
        executeFlagEnabled (Just "0") `shouldBe` False
        executeFlagEnabled (Just "1") `shouldBe` True

qualified :: String -> String -> ExecuteQualification
qualified notebook helper =
    ExecuteQualification
        { qualificationNotebookGhc = notebook
        , qualificationHelperGhc = helper
        , qualificationSandbox = SandboxMacOS
        , qualificationContainment = ContainmentProven
        }
