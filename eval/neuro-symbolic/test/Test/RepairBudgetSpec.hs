{-# LANGUAGE OverloadedStrings #-}

module Test.RepairBudgetSpec (spec) where

import Data.Aeson (object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Check (CheckResult (..))
import Test.Hspec

import Eval.Agent (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    defaultBudget,
    runEpisodeWith,
 )
import Eval.Ollama (ToolCall (..), Turn (..))
import Eval.Task (Grader (..), Task (..))

dummyTask :: Task
dummyTask = Task "t" "do the thing" (ByValue "True")

{- | A code-tool call following the content protocol: the Haskell rides in a
```haskell block in the message, not in the tool args.
-}
callTurn :: Text -> Turn
callTurn name =
    Turn
        (object ["role" .= ("assistant" :: Text)])
        "```haskell\nx = 1\n```"
        [ToolCall name (object [])]

doneTurn :: Turn
doneTurn = Turn (object ["role" .= ("assistant" :: Text)]) "done" []

redWithDiag :: ToolCall -> IO (Either Text ToolOutcome)
redWithDiag _ =
    pure . Right . ToolOk $
        object
            [ "cellId" .= (1 :: Int)
            , "ok" .= False
            , "guidance"
                .= [ object
                        [ "category" .= ("type-mismatch" :: Text)
                        , "message" .= ("PIN-DIAGNOSTIC fix the type" :: Text)
                        ]
                   ]
            ]

scriptedDriver ::
    Double -> (ToolCall -> IO (Either Text ToolOutcome)) -> [Turn] -> IO Driver
scriptedDriver dt disp script = do
    cursor <- newIORef (0 :: Int)
    clock <- newIORef (0 :: Double)
    let nextTurn _msgs = do
            i <- readIORef cursor
            modifyIORef' cursor (+ 1)
            pure (Right (script !! i))
        now = do
            t <- readIORef clock
            modifyIORef' clock (+ dt)
            pure t
    pure
        Driver
            { drvChat = nextTurn
            , drvDispatch = disp
            , drvNow = now
            , drvVerify = pure (CheckPassed, Nothing)
            }

spec :: Spec
spec = describe "B3 repair budget and deadline" $ do
    it "stops after D repair rounds when a cell never compiles" $ do
        driver <-
            scriptedDriver
                0
                redWithDiag
                (concat (replicate 50 [callTurn "insert_cell", doneTurn]))
        let budget = defaultBudget{ebMaxRepairs = 2, ebDeadlineSecs = 1 / 0}
        run <- runEpisodeWith budget driver (taskPrompt dummyTask) 1000
        arStopped run `shouldBe` "repair_budget"
        ("PIN-DIAGNOSTIC" `T.isInfixOf` arFinal run) `shouldBe` True

    it "stops at the wall-clock deadline even with repairs to spare" $ do
        driver <-
            scriptedDriver
                10
                redWithDiag
                (concat (replicate 50 [callTurn "insert_cell", doneTurn]))
        let budget = defaultBudget{ebMaxRepairs = 1000, ebDeadlineSecs = 25}
        run <- runEpisodeWith budget driver (taskPrompt dummyTask) 1000
        arStopped run `shouldBe` "deadline"
        ("PIN-DIAGNOSTIC" `T.isInfixOf` arFinal run) `shouldBe` True

    it "still finishes normally when the cell goes green within budget" $ do
        let redThenGreen (ToolCall name _)
                | name == "insert_cell" = ok False
                | otherwise = ok True
            ok h =
                pure . Right . ToolOk $
                    object ["cellId" .= (1 :: Int), "ok" .= h]
        driver <-
            scriptedDriver
                0
                redThenGreen
                [callTurn "insert_cell", doneTurn, callTurn "replace_cell_source", doneTurn]
        let budget = defaultBudget{ebMaxRepairs = 3, ebDeadlineSecs = 1 / 0}
        run <- runEpisodeWith budget driver (taskPrompt dummyTask) 1000
        arStopped run `shouldBe` "done"
