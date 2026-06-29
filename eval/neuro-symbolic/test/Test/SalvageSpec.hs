{-# LANGUAGE OverloadedStrings #-}

module Test.SalvageSpec (spec) where

import Data.Aeson (object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text, isInfixOf)
import Sabela.AI.Types (ToolOutcome (..))
import Test.Hspec

import Eval.Agent (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (..),
    defaultBudget,
    runEpisodeTraced,
    runEpisodeWith,
 )
import Eval.Ollama (ToolCall (..), Turn (..))
import Eval.Salvage (salvageCell)
import Eval.Task (Grader (..), Task (..))

spec :: Spec
spec = describe "Rank 3 code-fence salvage" $ do
    describe "salvageCell (pure)" $ do
        it "extracts a fenced haskell block the model wrote as text" $
            salvageCell "Here is the fix:\n```haskell\ntotal = foldr (+) 0\n```\nDone."
                `shouldBe` Just "total = foldr (+) 0"
        it "extracts an untagged fenced block" $
            salvageCell "```\nx = 1\n```" `shouldBe` Just "x = 1"
        it "keeps a multi-line body" $
            salvageCell "```haskell\ndf <- D.readCsv \"r.csv\"\nrevenueTotal = sum xs\n```"
                `shouldBe` Just "df <- D.readCsv \"r.csv\"\nrevenueTotal = sum xs"
        it "returns Nothing when there is no fence" $
            salvageCell "I would define total as a fold." `shouldBe` Nothing
        it "returns Nothing for an unclosed fence" $
            salvageCell "```haskell\ntotal = foldr (+) 0" `shouldBe` Nothing
        it "returns Nothing for an empty block" $
            salvageCell "```haskell\n\n```" `shouldBe` Nothing
        it "ignores a fence tagged as another language" $
            salvageCell "```python\nprint(1)\n```" `shouldBe` Nothing

        it "drops a stray bare tool-call line inside the fence" $
            salvageCell "```haskell\ninsert_cell()\nrevenueTotal = 600\n```"
                `shouldBe` Just "revenueTotal = 600"

        it "drops a spaced bare tool-call line (scratchpad ())" $
            salvageCell "```haskell\nscratchpad ()\nx = 1\n```"
                `shouldBe` Just "x = 1"

        it "leaves real Haskell that merely mentions a tool name untouched" $
            salvageCell "```haskell\nresult = read_cell 1 ++ go (x)\n```"
                `shouldBe` Just "result = read_cell 1 ++ go (x)"

    describe "runEpisodeWith (salvage harvests an echoed cell)" $ do
        it "harvests a block the model echoed without inserting, then stops done" $ do
            driver <- scriptedDriver alwaysHealthy [echoTurn, doneTurn]
            run <- runEpisodeWith openBudget driver dummyTask 10
            arStopped run `shouldBe` "done"
            arToolCalls run `shouldBe` 1
            arTurns run `shouldBe` 2

        it "does not salvage once a cell is already owned (no duplicate insert)" $ do
            driver <- scriptedDriver alwaysHealthy [callTurn "insert_cell", echoTurn]
            run <- runEpisodeWith openBudget driver dummyTask 10
            arStopped run `shouldBe` "done"
            arToolCalls run `shouldBe` 1

    describe "runEpisodeTraced (debug trace sink)" $
        it "streams the system prompt and the task through the sink" $ do
            sink <- newIORef []
            driver <- scriptedDriver alwaysHealthy [doneTurn]
            _ <-
                runEpisodeTraced
                    (\t -> modifyIORef' sink (++ [t]))
                    GrammarOn
                    openBudget
                    driver
                    dummyTask
                    3
            out <- mconcat <$> readIORef sink
            ("## 1. system" `isInfixOf` out) `shouldBe` True
            ("do the thing" `isInfixOf` out) `shouldBe` True

openBudget :: EpisodeBudget
openBudget = defaultBudget{ebMaxRepairs = maxBound, ebDeadlineSecs = 1 / 0}

dummyTask :: Task
dummyTask = Task "t" "do the thing" (ByValue "True")

echoTurn :: Turn
echoTurn =
    Turn
        (object ["role" .= ("assistant" :: Text)])
        "```haskell\nrevenueTotal = 600\n```"
        []

callTurn :: Text -> Turn
callTurn name =
    Turn
        (object ["role" .= ("assistant" :: Text)])
        "```haskell\nrevenueTotal = 600\n```"
        [ToolCall name (object [])]

doneTurn :: Turn
doneTurn = Turn (object ["role" .= ("assistant" :: Text)]) "done" []

scriptedDriver ::
    (ToolCall -> IO (Either Text ToolOutcome)) -> [Turn] -> IO Driver
scriptedDriver disp script = do
    cursor <- newIORef (0 :: Int)
    let nextTurn _msgs = do
            i <- readIORef cursor
            modifyIORef' cursor (+ 1)
            pure (Right (script !! i))
    pure
        Driver
            { drvChat = nextTurn
            , drvDispatch = disp
            , drvNow = pure 0
            , drvVerify = pure True
            }

alwaysHealthy :: ToolCall -> IO (Either Text ToolOutcome)
alwaysHealthy _ = pure (Right (ToolOk (object ["cellId" .= (1 :: Int), "ok" .= True])))
