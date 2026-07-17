{-# LANGUAGE OverloadedStrings #-}

module Test.ScaffoldSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Types (ToolOutcome (..))
import Test.Hspec

import Eval.Agent (
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (..),
    defaultBudget,
    runEpisodeSeeded,
    runEpisodeWith,
 )
import Eval.Ollama (ToolCall (..), Turn (..))
import Eval.Scaffold (scaffoldCall, scaffoldText)
import Eval.Task (Grader (..), Task (..))

dfTask :: Task
dfTask =
    Task
        "df"
        "Using the dataframe library, load sales.csv and define total :: Double."
        (ByValue "True")

pureTask :: Task
pureTask = Task "p" "Define double :: Int -> Int." (ByValue "True")

plotTask :: Task
plotTask = Task "q" "Plot these quarterly sales with the granite library." ByRender

spec :: Spec
spec = describe "Rank 1 type scaffold" $ do
    describe "scaffoldCall (fires only for a dataframe-over-CSV task)" $ do
        it "fires for a dataframe task naming a CSV" $
            (tcName <$> scaffoldCall (taskPrompt dfTask)) `shouldBe` Just "insert_cell"
        it "does not fire for a pure task" $
            (tcName <$> scaffoldCall (taskPrompt pureTask)) `shouldBe` Nothing
        it "does not fire for a plot-only task (no dataframe, no CSV)" $
            (tcName <$> scaffoldCall (taskPrompt plotTask)) `shouldBe` Nothing

    describe "scaffoldText (the baked-in import and typed load)" $ do
        let s = scaffoldText "sales.csv"
        it "bakes in the DataFrame import" $
            ("import qualified DataFrame as D" `T.isInfixOf` s) `shouldBe` True
        it "loads the named CSV with readCsv" $
            ("D.readCsv \"sales.csv\"" `T.isInfixOf` s) `shouldBe` True
        it "declares the dataframe dependency" $
            ("build-depends: dataframe" `T.isInfixOf` s) `shouldBe` True

    describe "runEpisodeWith (scaffold is inserted before the model acts)" $
        it "dispatches the scaffold insert first for a dataframe task" $ do
            calls <- newIORef ([] :: [ToolCall])
            let disp c = do
                    modifyIORef' calls (++ [c])
                    pure (Right (ToolOk (object ["cellId" .= (1 :: Int), "ok" .= True])))
            driver <- scriptedDriver disp [doneTurn]
            _ <- runEpisodeWith openBudget driver (taskPrompt dfTask) 10
            dispatched <- readIORef calls
            take 1 (map tcName dispatched) `shouldBe` ["insert_cell"]

    describe "runEpisodeSeeded (continuation carries the transcript)" $
        it "continues from the prior transcript + one new user turn, no re-scaffold" $ do
            calls <- newIORef ([] :: [ToolCall])
            seen <- newIORef ([] :: [[Value]])
            let disp c =
                    modifyIORef' calls (++ [c])
                        >> pure (Right (ToolOk (object [])))
            driver0 <- scriptedDriver disp [doneTurn]
            let driver =
                    driver0
                        { drvChat = \msgs ->
                            modifyIORef' seen (++ [msgs]) >> drvChat driver0 msgs
                        }
                prior =
                    [ object ["role" .= ("system" :: Text), "content" .= ("S" :: Text)]
                    , object ["role" .= ("user" :: Text), "content" .= ("earlier" :: Text)]
                    , object ["role" .= ("assistant" :: Text), "content" .= ("reply" :: Text)]
                    ]
            _ <-
                runEpisodeSeeded
                    prior
                    (const (pure ()))
                    GrammarOff
                    openBudget
                    driver
                    (taskPrompt dfTask)
                    10
            firstMsgs <- head <$> readIORef seen
            -- prior transcript carried verbatim, exactly one new user turn appended,
            -- no re-injected system prompt.
            take (length prior) firstMsgs `shouldBe` prior
            length firstMsgs `shouldBe` length prior + 1
            -- a dataframe task would normally scaffold-insert first; on a
            -- continuation the scaffold is skipped.
            readIORef calls >>= (`shouldBe` [])

openBudget :: EpisodeBudget
openBudget = defaultBudget{ebMaxRepairs = maxBound, ebDeadlineSecs = 1 / 0}

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
