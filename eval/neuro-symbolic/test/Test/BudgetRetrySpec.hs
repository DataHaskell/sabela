{-# LANGUAGE OverloadedStrings #-}

{- | N4 (read-discovery budget nudge) and N10 (bounded chat-error retry) in the
agent episode loop. A scripted 'Driver' plays canned turns / chat errors so the
loop's discipline is exercised without a live model or server.
-}
module Test.BudgetRetrySpec (spec) where

import Data.Aeson (object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text, isInfixOf)
import qualified Data.Text as T
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Check (CheckResult (..))
import Test.Hspec

import Eval.Agent (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (..),
    defaultBudget,
    runEpisodeWith',
 )
import Eval.Ollama (ToolCall (..), Turn (..))
import Eval.Task (Grader (..), Task (..))

spec :: Spec
spec = describe "Agent loop discipline (N4 budget, N10 retry)" $ do
    describe "N4: held-fact act nudge (R5.6)" $ do
        it
            "injects a forcing 'act now' message once a call-ready fact is \
            \held and reads keep coming"
            $ do
                driver <-
                    scriptedDriver
                        discoverAnswers
                        ( map
                            Right
                            [discoverTurn "bars", discoverTurn "granite bars", doneTurn]
                        )
                run <- runEpisodeWith' GrammarOff openBudget driver (taskPrompt dummyTask) 20
                transcriptText run `shouldSatisfy` isInfixOf "act now"

        it "never nudges on a fact-free ledger, however long the spiral" $ do
            driver <-
                scriptedDriver
                    alwaysHealthy
                    (map Right (replicate 8 (callTurn "read_cell") ++ [doneTurn]))
            run <- runEpisodeWith' GrammarOff openBudget driver (taskPrompt dummyTask) 20
            transcriptText run `shouldSatisfy` (not . isInfixOf "act now")

        it "does not nudge when the model acts (insert_cell resets the budget)" $ do
            driver <-
                scriptedDriver
                    alwaysHealthy
                    (map Right (replicate 8 (callTurn "insert_cell") ++ [doneTurn]))
            run <- runEpisodeWith' GrammarOff openBudget driver (taskPrompt dummyTask) 20
            transcriptText run `shouldSatisfy` (not . isInfixOf "act now")

    describe "N10: bounded chat-error retry" $ do
        it "recovers when a chat error clears within the retry budget" $ do
            driver <-
                scriptedDriver alwaysHealthy [Left "boom", Left "boom", Right doneTurn]
            run <- runEpisodeWith' GrammarOff openBudget driver (taskPrompt dummyTask) 5
            arStopped run `shouldBe` "done"

        it "gives up with an error after exhausting retries" $ do
            driver <- scriptedDriver alwaysHealthy [Left "boom", Left "boom", Left "boom"]
            run <- runEpisodeWith' GrammarOff openBudget driver (taskPrompt dummyTask) 5
            arStopped run `shouldBe` "error"

    describe "no-progress verify guard (the stuck verify spin)"
        $ it
            "stops as 'stuck' when the model declares done, the check never passes, and nothing changes"
        $ do
            let stuckDriver =
                    Driver
                        { drvChat = \_ -> pure (Right doneTurn)
                        , drvDispatch = alwaysHealthy
                        , drvNow = pure 0
                        , drvVerify = pure (CheckFailed, Nothing)
                        }
            run <-
                runEpisodeWith' GrammarOff openBudget stuckDriver (taskPrompt dummyTask) 50
            arStopped run `shouldBe` "stuck"
            arTurns run `shouldSatisfy` (< 10)

transcriptText :: AgentRun -> Text
transcriptText = T.pack . show . arTranscript

openBudget :: EpisodeBudget
openBudget = defaultBudget{ebMaxRepairs = maxBound, ebDeadlineSecs = 1 / 0}

dummyTask :: Task
dummyTask = Task "t" "do the thing" (ByValue "True")

callTurn :: Text -> Turn
callTurn name =
    Turn (object ["role" .= ("assistant" :: Text)]) "" [ToolCall name (object [])]

discoverTurn :: Text -> Turn
discoverTurn q =
    Turn
        (object ["role" .= ("assistant" :: Text)])
        ""
        [ToolCall "discover" (object ["query" .= q])]

{- | Answer discover with a found, exact, typed hit (a call-ready fact);
everything else with the healthy write ack.
-}
discoverAnswers :: ToolCall -> IO (Either Text ToolOutcome)
discoverAnswers (ToolCall "discover" _) =
    pure . Right . ToolOk $
        object
            [ "query" .= ("bars" :: Text)
            , "state" .= ("found" :: Text)
            , "hits"
                .= [ object
                        [ "name" .= ("bars" :: Text)
                        , "type" .= ("[(Text, Double)] -> Plot -> Text" :: Text)
                        , "module" .= ("Granite.Svg" :: Text)
                        , "package" .= ("granite" :: Text)
                        , "version" .= ("0.7.4.0" :: Text)
                        , "install" .= ("installed" :: Text)
                        , "matchKind" .= ("exact" :: Text)
                        , "origin" .= ("session" :: Text)
                        ]
                   ]
            ]
discoverAnswers tc = alwaysHealthy tc

doneTurn :: Turn
doneTurn = Turn (object ["role" .= ("assistant" :: Text)]) "done" []

scriptedDriver ::
    (ToolCall -> IO (Either Text ToolOutcome)) -> [Either Text Turn] -> IO Driver
scriptedDriver disp script = do
    cursor <- newIORef (0 :: Int)
    let nextTurn _msgs = do
            i <- readIORef cursor
            modifyIORef' cursor (+ 1)
            pure (script !! i)
    pure
        Driver
            { drvChat = nextTurn
            , drvDispatch = disp
            , drvNow = pure 0
            , drvVerify = pure (CheckPassed, Nothing)
            }

alwaysHealthy :: ToolCall -> IO (Either Text ToolOutcome)
alwaysHealthy _ = pure (Right (ToolOk (object ["cellId" .= (1 :: Int), "ok" .= True])))
