{-# LANGUAGE OverloadedStrings #-}

module Test.BudgetRetrySpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value, object, (.=))
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
    describe "N4: the loop steers no one (R5.6 withdrawn)" $
        forM_ spirals $ \(name, disp, script) ->
            it (name <> ": no act-now steering is injected") $ do
                driver <- scriptedDriver disp (map Right script)
                run <- runEpisodeWith' GrammarOff openBudget driver (taskPrompt dummyTask) 20
                transcriptText run `shouldSatisfy` (not . isInfixOf "act now")

    describe "N10: bounded chat-error retry" $ do
        it "recovers when a chat error clears within the retry budget" $ do
            driver <-
                scriptedDriver
                    alwaysHealthy
                    [ Left "boom"
                    , Left "boom"
                    , Right (callTurn "insert_cell")
                    , Right doneTurn
                    ]
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
                        , drvVerify = const (pure (CheckFailed, Nothing))
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
    Turn
        (object ["role" .= ("assistant" :: Text)])
        ""
        [ToolCall name (writeArgs name)]

discoverTurn :: Text -> Turn
discoverTurn q =
    Turn
        (object ["role" .= ("assistant" :: Text)])
        ""
        [ToolCall "discover" (object ["query" .= q])]

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

{- | A write carries the source the loop reads back off it. A committed cell
with no source is not an artifact, so an episode scripted without one can
never reach a stop.
-}
writeArgs :: Text -> Value
writeArgs "insert_cell" = object ["source" .= scriptedSource]
writeArgs "replace_cell_source" =
    object ["cell_id" .= (1 :: Int), "new_source" .= scriptedSource]
writeArgs _ = object []

scriptedSource :: Text
scriptedSource = "x = 1"

{- | Read-only spirals of every shape the withdrawn nudge keyed on: a held
searchable fact, a fact-free ledger, and a spiral broken by a write.
-}
spirals ::
    [(String, ToolCall -> IO (Either Text ToolOutcome), [Turn])]
spirals =
    [
        ( "a held call-ready fact"
        , discoverAnswers
        , [discoverTurn "bars", discoverTurn "granite bars", doneTurn]
        )
    ,
        ( "a fact-free ledger"
        , alwaysHealthy
        , replicate 8 (callTurn "read_cell") ++ [doneTurn]
        )
    ,
        ( "a spiral broken by a write"
        , alwaysHealthy
        , replicate 8 (callTurn "insert_cell") ++ [doneTurn]
        )
    ]

doneTurn :: Turn
doneTurn = Turn (object ["role" .= ("assistant" :: Text)]) "done" []

{- | A driver reading a fixed script. Past its end it repeats the last turn:
the loop decides when to stop, and a fixture that crashed on one extra prompt
would report an exception where the stop tag is the thing under test.
-}
scriptedDriver ::
    (ToolCall -> IO (Either Text ToolOutcome)) -> [Either Text Turn] -> IO Driver
scriptedDriver disp script = do
    cursor <- newIORef (0 :: Int)
    let nextTurn _msgs = do
            i <- readIORef cursor
            modifyIORef' cursor (+ 1)
            pure (script !! min i (length script - 1))
    pure
        Driver
            { drvChat = nextTurn
            , drvDispatch = disp
            , drvNow = pure 0
            , drvVerify = const (pure (CheckPassed, Nothing))
            }

alwaysHealthy :: ToolCall -> IO (Either Text ToolOutcome)
alwaysHealthy _ = pure (Right (ToolOk (object ["cellId" .= (1 :: Int), "ok" .= True])))
