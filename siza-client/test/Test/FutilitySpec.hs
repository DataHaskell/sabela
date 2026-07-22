{-# LANGUAGE OverloadedStrings #-}

module Test.FutilitySpec (futilitySpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Futility (
    futilityNote,
    guardDispatch,
    newFutilityGuard,
 )

call :: Text -> ToolCall
call src = ToolCall "insert_cell" (object ["source" .= src])

failingWith :: Text -> ToolCall -> IO (Either Text ToolOutcome)
failingWith e _ = pure (Left e)

hasNote :: Either Text ToolOutcome -> Bool
hasNote (Left e) = "byte-identical" `T.isInfixOf` e
hasNote (Right (ToolErr (Object o))) = KM.member "futility" o
hasNote _ = False

futilitySpec :: Spec
futilitySpec = describe "Siza.Agent.Futility (retry-futility guard)" $ do
    it "does not annotate a first failure" $ do
        g <- newFutilityGuard
        r <- guardDispatch g (failingWith "boom") (call "x = 1")
        hasNote r `shouldBe` False

    it "annotates the second byte-identical identically-failing call" $ do
        g <- newFutilityGuard
        _ <- guardDispatch g (failingWith "boom") (call "x = 1")
        r <- guardDispatch g (failingWith "boom") (call "x = 1")
        hasNote r `shouldBe` True
        case r of
            Left e -> do
                e `shouldSatisfy` T.isInfixOf "boom"
                e `shouldSatisfy` T.isInfixOf futilityNote
            _ -> expectationFailure "expected Left"

    it "directs away from payload rewriting, toward a different approach" $ do
        futilityNote `shouldSatisfy` T.isInfixOf "not the fault"
        futilityNote `shouldSatisfy` T.isInfixOf "Change approach"

    it "does not annotate when the arguments differ" $ do
        g <- newFutilityGuard
        _ <- guardDispatch g (failingWith "boom") (call "x = 1")
        r <- guardDispatch g (failingWith "boom") (call "x = 2")
        hasNote r `shouldBe` False

    it "does not annotate when the failure differs" $ do
        g <- newFutilityGuard
        errs <- newIORef (["a", "b"] :: [Text])
        let d _ = do
                es <- readIORef errs
                modifyIORef' errs (drop 1)
                pure (Left (case es of e : _ -> e; [] -> "z"))
        _ <- guardDispatch g d (call "x = 1")
        r <- guardDispatch g d (call "x = 1")
        hasNote r `shouldBe` False

    it "a success clears the memory for that call" $ do
        g <- newFutilityGuard
        _ <- guardDispatch g (failingWith "boom") (call "x = 1")
        _ <-
            guardDispatch
                g
                (\_ -> pure (Right (ToolOk (object []))))
                (call "x = 1")
        r <- guardDispatch g (failingWith "boom") (call "x = 1")
        hasNote r `shouldBe` False

    it "annotates a repeated identical ToolErr via the futility field" $ do
        g <- newFutilityGuard
        let d _ = pure (Right (ToolErr (object ["error" .= ("bad" :: Text)])))
        _ <- guardDispatch g d (call "x = 1")
        r <- guardDispatch g d (call "x = 1")
        hasNote r `shouldBe` True
