{-# LANGUAGE OverloadedStrings #-}

module Test.DeadlineStartSpec (deadlineStartSpec) where

import Data.Aeson (Value, object, (.=))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..))
import Siza.Agent.Check (CheckResult (..))
import Siza.Agent.Loop (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (..),
    runEpisodeSeeded,
 )

scaffoldPrompt :: Text
scaffoldPrompt = "load ./examples/data/housing.csv and print the first rows"

slowScaffoldDriver :: IORef Double -> Double -> IO Driver
slowScaffoldDriver clock buildSecs = do
    turns <- newIORef (0 :: Int)
    let dispatch tc = do
            case tcName tc of
                "insert_cell" -> modifyIORef' clock (+ buildSecs)
                _ -> pure ()
            pure (Right (ToolOk (object ["ok" .= True])))
        chat _ = do
            n <- readIORef turns
            modifyIORef' turns (+ 1)
            pure . Right $
                if n < 3
                    then Turn (object []) "" [ToolCall "list_bindings" (object [])]
                    else Turn (object []) "done" []
    pure
        Driver
            { drvChat = chat
            , drvDispatch = dispatch
            , drvNow = readIORef clock
            , drvVerify = const (pure (CheckNotApplicable, Nothing))
            }

runWithBuild :: Double -> Double -> IO AgentRun
runWithBuild deadline buildSecs = do
    clock <- newIORef 0
    driver <- slowScaffoldDriver clock buildSecs
    runEpisodeSeeded
        []
        (const (pure ()))
        GrammarOff
        (EpisodeBudget{ebMaxRepairs = 4, ebDeadlineSecs = deadline})
        driver
        scaffoldPrompt
        12

deadlineStartSpec :: Spec
deadlineStartSpec = describe "the deadline starts at the model's first turn" $ do
    it "scaffold-eats-deadline: a slow setup build does not end the episode" $ do
        run <- runWithBuild 600 900
        arStopped run `shouldNotBe` "deadline"
        arTurns run `shouldSatisfy` (> 0)

    it "a fast setup leaves the episode unaffected (negative control)" $ do
        run <- runWithBuild 600 0
        arStopped run `shouldNotBe` "deadline"
        arTurns run `shouldSatisfy` (> 0)

    it "the model's OWN time still counts against the deadline" $ do
        run <- runWithBuild 0 0
        arStopped run `shouldBe` "deadline"
