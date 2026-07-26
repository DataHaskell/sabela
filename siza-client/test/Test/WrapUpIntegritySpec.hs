{-# LANGUAGE OverloadedStrings #-}

module Test.WrapUpIntegritySpec (wrapUpIntegritySpec) where

import Data.Aeson (Value (..), object, (.=))
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
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
import Siza.Agent.Owned (bestFailing, recordOwned)
import Test.DiscoverFixtures (textField)

redOutcome :: Int -> Text -> Either Text ToolOutcome
redOutcome cid msg =
    Right . ToolOk $
        object
            [ "cellId" .= cid
            , "execution"
                .= object
                    [ "ok" .= False
                    , "outcome"
                        .= object
                            [ "message"
                                .= ("cell " <> T.pack (show cid) <> ", line 1: " <> msg)
                            , "tag" .= ("Raised" :: Text)
                            ]
                    ]
            ]

deletedOutcome :: Int -> Either Text ToolOutcome
deletedOutcome cid = Right . ToolOk $ object ["cellId" .= cid, "deleted" .= True]

insertCall :: Int -> ToolCall
insertCall cid =
    ToolCall "insert_cell" (object ["source" .= ("cell" <> T.pack (show cid))])

deleteCall :: Int -> ToolCall
deleteCall cid = ToolCall "delete_cell" (object ["cell_id" .= cid])

giveUpCitesLiveCellSpec :: Spec
giveUpCitesLiveCellSpec =
    describe "give-up cites the newest LIVE owned red cell (live_test2 #3)" $
        it "cites cell 10, never deleted cell 8" $ do
            let owned0 = Map.empty
                owned1 =
                    recordOwned
                        (insertCall 8, redOutcome 8 "Variable not in scope: insert_cell")
                        owned0
                owned2 = recordOwned (deleteCall 8, deletedOutcome 8) owned1
                owned3 =
                    recordOwned
                        (insertCall 10, redOutcome 10 "Ambiguous occurrence `col`")
                        owned2
            Map.member 8 owned3 `shouldBe` False
            bestFailing owned3 `shouldSatisfy` T.isInfixOf "Ambiguous occurrence"
            bestFailing owned3
                `shouldSatisfy` (not . T.isInfixOf "Variable not in scope: insert_cell")

assistant :: Text -> Value
assistant t = object ["role" .= ("assistant" :: Text), "content" .= t]

emptyStopTurn :: Turn
emptyStopTurn = Turn (assistant "") "" []

adviceOnlyTurn :: Turn
adviceOnlyTurn =
    Turn
        (assistant advice)
        advice
        []
  where
    advice = "The DataFrame library is available; use readCsv to load the CSV."

driverWith ::
    ([Value] -> IO (Either Text Turn)) ->
    (CheckResult, Maybe Text) ->
    Driver
driverWith chat verdict =
    Driver
        { drvChat = chat
        , drvDispatch = const (pure (Right (ToolOk (object []))))
        , drvNow = pure 0
        , drvVerify = pure verdict
        }

scriptChat :: (Int -> Turn) -> IO ([Value] -> IO (Either Text Turn))
scriptChat f = do
    ref <- newIORef (0 :: Int)
    pure $ \_ -> do
        n <- readIORef ref
        writeIORef ref (n + 1)
        pure (Right (f (n + 1)))

doneRequiresArtifactSpec :: Spec
doneRequiresArtifactSpec =
    describe
        "false-success: a tautological check with no artifact is never done (live_test2 #1)"
        $ do
            it "no owned cell was ever created: the episode never says done" $ do
                chat <- scriptChat (const emptyStopTurn)
                let driver = driverWith chat (CheckPassed, Nothing)
                run <-
                    runEpisodeSeeded
                        []
                        (const (pure ()))
                        GrammarOff
                        (EpisodeBudget{ebMaxRepairs = 4, ebDeadlineSecs = 600})
                        driver
                        "mean median_house_value by ocean_proximity"
                        10
                arStopped run `shouldNotBe` "done"
            it "advice-only final message on a tautology-passed check is not done" $ do
                chat <- scriptChat $ \n -> if n == 1 then adviceOnlyTurn else emptyStopTurn
                let driver = driverWith chat (CheckPassed, Nothing)
                run <-
                    runEpisodeSeeded
                        []
                        (const (pure ()))
                        GrammarOff
                        (EpisodeBudget{ebMaxRepairs = 4, ebDeadlineSecs = 600})
                        driver
                        "mean median_house_value by ocean_proximity"
                        10
                arStopped run `shouldNotBe` "done"
                let contents = map (textField "content") (arTranscript run)
                any (T.isInfixOf "written no cell yet") contents `shouldBe` True

wrapUpIntegritySpec :: Spec
wrapUpIntegritySpec = describe "C3: wrap-up integrity" $ do
    giveUpCitesLiveCellSpec
    doneRequiresArtifactSpec
