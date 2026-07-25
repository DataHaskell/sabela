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

-- | The futility note a guarded outcome carries, if any.
noteOf :: Either Text ToolOutcome -> Maybe Text
noteOf (Left e) = Just e
noteOf (Right (ToolErr (Object o))) = case KM.lookup "futility" o of
    Just (String s) -> Just s
    _ -> Nothing
noteOf _ = Nothing

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

    {- G5.7: a deterministic rejection's fault IS the payload. live_test5 told
    the model "the payload is not the fault" about a compile-gate rejection and
    steered it at kernel_status, away from the one-character fix. -}
    describe "truthful futility for a deterministic rejection" $ do
        let gateRejection =
                ToolErr
                    ( object
                        [ "refusal" .= ("compile-gate" :: Text)
                        , "verdict" .= ("diagnostic" :: Text)
                        , "diagnostic"
                            .= ("<interactive>:238:1: error: [GHC-88464]" :: Text)
                        ]
                    )
            rejecting _ = pure (Right gateRejection)
            secondNote = do
                g <- newFutilityGuard
                _ <- guardDispatch g rejecting (call "x = 1 +")
                out <- guardDispatch g rejecting (call "x = 1 +")
                pure (noteOf out)

        it "never tells the model the payload is not the fault" $ do
            n <- secondNote
            n `shouldSatisfy` maybe False (not . T.isInfixOf "not the fault")

        it "names the source as the fault" $ do
            n <- secondNote
            n `shouldSatisfy` maybe False (T.isInfixOf "source")

        it "never steers a source error at state-inspection tools" $ do
            n <- secondNote
            n `shouldSatisfy` maybe False (not . T.isInfixOf "kernel_status")

        it "still gives the environmental note for a transport failure" $ do
            g <- newFutilityGuard
            _ <- guardDispatch g (failingWith "boom") (call "x = 1")
            out <- guardDispatch g (failingWith "boom") (call "x = 1")
            hasNote out `shouldBe` True

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
