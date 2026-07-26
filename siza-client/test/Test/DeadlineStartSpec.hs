{-# LANGUAGE OverloadedStrings #-}

{- | The episode deadline starts at the MODEL's first turn, not at process
start. Harness-owned setup — above all the scaffold's dependency build,
which can run for minutes — is not the model's to pay for.

live_test24 is the specimen: the scaffold installed @dataframe@ (~4 minutes
of wall clock), and the model then opened its episode being told "the time
budget is nearly spent", made four tool calls, and stopped on @deadline@
with the deliverable unwritten. Every driver in the other loop specs uses a
frozen clock (@drvNow = pure 0@), which is exactly why nothing caught it.
-}
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

-- | A prompt naming a data file, so the scaffold stage fires.
scaffoldPrompt :: Text
scaffoldPrompt = "load ./examples/data/housing.csv and print the first rows"

{- | A driver whose clock only moves when the SCAFFOLD write is dispatched,
standing in for the dependency build. The model's own calls are free, so any
deadline the episode reports is attributable to setup alone.
-}
slowScaffoldDriver :: IORef Double -> Double -> IO Driver
slowScaffoldDriver clock buildSecs = do
    turns <- newIORef (0 :: Int)
    let dispatch tc = do
            -- The scaffold's insert is what costs; everything else is free.
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
            , drvVerify = pure (CheckNotApplicable, Nothing)
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
        -- The build alone exceeds the whole deadline; the model must still run.
        run <- runWithBuild 600 900
        arStopped run `shouldNotBe` "deadline"
        arTurns run `shouldSatisfy` (> 0)

    it "a fast setup leaves the episode unaffected (negative control)" $ do
        run <- runWithBuild 600 0
        arStopped run `shouldNotBe` "deadline"
        arTurns run `shouldSatisfy` (> 0)

    it "the model's OWN time still counts against the deadline" $ do
        -- Nothing is charged for setup here, so an episode whose deadline is
        -- already zero stops immediately: the clock is not simply ignored.
        run <- runWithBuild 0 0
        arStopped run `shouldBe` "deadline"
