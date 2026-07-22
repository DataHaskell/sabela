{-# LANGUAGE OverloadedStrings #-}

{- | The deliverable-green done-signal (R5.5/R5.7/R9.8) through the REAL
'runEpisodeSeeded': fires exactly once, only after the verify channel confirms
the deliverable, never before the write; no search/read advice after it.
-}
module Test.DoneSignalSpec (doneSignalSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
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
import Siza.Agent.Messages (doneSignal)
import Test.DiscoverFixtures (textField)

cleanWrite :: Value
cleanWrite =
    object ["cellId" .= (1 :: Int), "execution" .= object ["ok" .= True]]

foundEnvelope :: Value
foundEnvelope =
    object
        [ "query" .= ("bars" :: Text)
        , "state" .= ("found" :: Text)
        , "hits"
            .= [ object
                    [ "name" .= ("bars" :: Text)
                    , "type" .= ("[(Text, Double)] -> Plot -> Text" :: Text)
                    , "module" .= ("Granite.Svg" :: Text)
                    , "package" .= ("granite" :: Text)
                    , "install" .= ("installed" :: Text)
                    , "matchKind" .= ("exact" :: Text)
                    , "origin" .= ("session" :: Text)
                    ]
               ]
        ]

dispatchScript :: ToolCall -> IO (Either Text ToolOutcome)
dispatchScript tc = case tcName tc of
    "insert_cell" -> pure (Right (ToolOk cleanWrite))
    "discover" -> pure (Right (ToolOk foundEnvelope))
    "list_cells" -> pure (Right (ToolOk (object ["cells" .= ([] :: [Value])])))
    _ -> pure (Right (ToolOk (object ["result" .= ("x :: Int" :: Text)])))

assistant :: Text -> Value
assistant t = object ["role" .= ("assistant" :: Text), "content" .= t]

-- | A chat driver that plays the given call batches, then stops calling.
scriptedChat :: [[ToolCall]] -> IO ([Value] -> IO (Either Text Turn))
scriptedChat batches = do
    step <- newIORef (0 :: Int)
    pure $ \_ -> do
        i <- readIORef step
        modifyIORef' step (+ 1)
        pure . Right $
            if i < length batches
                then Turn (assistant "acting") "" (batches !! i)
                else Turn (assistant "summary: done") "summary: done" []

runScript ::
    [[ToolCall]] -> (CheckResult, Maybe Text) -> IO AgentRun
runScript batches verdict = do
    chat <- scriptedChat batches
    let driver =
            Driver
                { drvChat = chat
                , drvDispatch = dispatchScript
                , drvNow = pure 0
                , drvVerify = pure verdict
                }
    runEpisodeSeeded
        []
        (const (pure ()))
        GrammarOff
        (EpisodeBudget{ebMaxRepairs = 4, ebDeadlineSecs = 600})
        driver
        "define jsonSum :: Int summing the numbers"
        12

write :: ToolCall
write = ToolCall "insert_cell" (object ["source" .= ("jsonSum = 60" :: Text)])

readOnly :: ToolCall
readOnly = ToolCall "list_bindings" (object [])

search :: ToolCall
search = ToolCall "discover" (object ["query" .= ("bars" :: Text)])

signalMsgs :: AgentRun -> [Int]
signalMsgs run =
    [ i
    | (i, m) <- zip [0 ..] (arTranscript run)
    , doneSignal `T.isInfixOf` textField "content" m
    ]

doneSignalSpec :: Spec
doneSignalSpec = describe "deliverable-green done-signal (R5.5/R5.7/R9.8)" $ do
    it "happy path: never fires before/without need, episode on the floor" $ do
        run <- runScript [[write]] (CheckPassed, Nothing)
        arStopped run `shouldBe` "done"
        arTurns run `shouldSatisfy` (<= 5)
        signalMsgs run `shouldBe` []

    it "fires exactly once when tools keep coming after a green deliverable" $ do
        run <- runScript [[write], [readOnly], [readOnly]] (CheckPassed, Nothing)
        arStopped run `shouldBe` "done"
        length (signalMsgs run) `shouldBe` 1

    it "fires only AFTER the write's outcome is in the transcript" $ do
        run <- runScript [[write], [readOnly], [readOnly]] (CheckPassed, Nothing)
        let writeIdx =
                [ i
                | (i, m) <- zip [0 ..] (arTranscript run)
                , textField "tool_name" m == "insert_cell"
                ]
        case (signalMsgs run, writeIdx) of
            (s : _, w : _) -> s `shouldSatisfy` (> w)
            _ -> expectationFailure "expected a signal and a write outcome"

    it "never fires when the check does not confirm the deliverable" $ do
        run <- runScript [[write], [readOnly], [readOnly]] (CheckFailed, Nothing)
        signalMsgs run `shouldBe` []

    it "never fires on a read-only spiral with no deliverable write" $ do
        run <-
            runScript [[readOnly], [readOnly], [readOnly]] (CheckPassed, Nothing)
        signalMsgs run `shouldBe` []

    it "R5.7: no channel emits search/read advice after the signal" $ do
        run <-
            runScript
                [[write], [search], [readOnly], [readOnly], [readOnly]]
                (CheckPassed, Nothing)
        case signalMsgs run of
            (s : _) -> do
                let after = drop (s + 1) (arTranscript run)
                forM_ after $ \m -> do
                    let c = textField "content" m
                    c `shouldSatisfy` (not . T.isInfixOf "act now")
                    c `shouldSatisfy` (not . T.isInfixOf "Facts already established")
                    c `shouldSatisfy` (not . T.isInfixOf "Look them up with discover")
            [] -> expectationFailure "expected the signal to fire"
