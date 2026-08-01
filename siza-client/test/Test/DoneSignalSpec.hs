{-# LANGUAGE OverloadedStrings #-}

module Test.DoneSignalSpec (doneSignalSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.AI.Verdict (VerdictClass (..), parseVerdict)
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..))
import Siza.Agent.Check (CheckResult (..))
import Siza.Agent.Loop (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (..),
    runEpisodeSeeded,
 )
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
                , drvVerify = const (pure verdict)
                }
    runEpisodeSeeded
        []
        (const (pure ()))
        GrammarOff
        (EpisodeBudget{ebMaxRepairs = 4, ebDeadlineSecs = 600})
        driver
        "define jsonSum :: Int summing the numbers"
        12

passed :: (CheckResult, Maybe Text)
passed = (CheckPassed, Just "jsonSum == 60")

noCheck :: (CheckResult, Maybe Text)
noCheck = (CheckNotApplicable, Just "the proposed check was discarded")

write :: ToolCall
write = ToolCall "insert_cell" (object ["source" .= ("jsonSum = 60" :: Text)])

readOnly :: ToolCall
readOnly = ToolCall "list_bindings" (object [])

search :: ToolCall
search = ToolCall "discover" (object ["query" .= ("bars" :: Text)])

-- | Transcript positions carrying an ok verdict from the harness's own gate.
signalMsgs :: AgentRun -> [Int]
signalMsgs run =
    [ i
    | (i, m) <- zip [0 ..] (arTranscript run)
    , parseVerdict (textField "content" m) == Just VerdictOk
    ]

couldNotRunMsgs :: AgentRun -> [Int]
couldNotRunMsgs run =
    [ i
    | (i, m) <- zip [0 ..] (arTranscript run)
    , parseVerdict (textField "content" m) == Just VerdictCouldNotRun
    ]

doneSignalSpec :: Spec
doneSignalSpec = describe "deliverable-green done-signal (R5.5/R5.7/R9.8)" $ do
    it "happy path: fires beside the landing write, episode on the floor" $ do
        run <- runScript [[write]] passed
        arStopped run `shouldBe` "done"
        arTurns run `shouldSatisfy` (<= 5)
        length (signalMsgs run) `shouldBe` 1

    it "fires exactly once when tools keep coming after a green deliverable" $ do
        run <- runScript [[write], [readOnly], [readOnly]] passed
        arStopped run `shouldBe` "done"
        length (signalMsgs run) `shouldBe` 1

    it "never claims a passing check when none applied" $ do
        run <- runScript [[write], [readOnly], [readOnly]] noCheck
        arStopped run `shouldBe` "done_unverified"
        signalMsgs run `shouldBe` []

    it "reserves the done tag for a check that actually passed" $ do
        green <- runScript [[write]] passed
        unchecked <- runScript [[write]] noCheck
        arStopped green `shouldBe` "done"
        arStopped unchecked `shouldSatisfy` (/= "done")

    it "discloses the no-check state as could-not-run, never as ok" $ do
        run <- runScript [[write], [readOnly], [readOnly]] noCheck
        arStopped run `shouldNotBe` "stuck"
        couldNotRunMsgs run `shouldNotBe` []
        arTranscript run
            `shouldSatisfy` any
                ( T.isInfixOf "No covering check reached a verdict this turn"
                    . textField "content"
                )

    it "fires only AFTER the write's outcome is in the transcript" $ do
        run <- runScript [[write], [readOnly], [readOnly]] passed
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
            runScript [[readOnly], [readOnly], [readOnly]] passed
        signalMsgs run `shouldBe` []

    it "R5.7: no channel emits search/read advice after the signal" $ do
        run <-
            runScript
                [[write], [search], [readOnly], [readOnly], [readOnly]]
                passed
        case signalMsgs run of
            (s : _) -> do
                let after = drop (s + 1) (arTranscript run)
                forM_ after $ \m -> do
                    let c = textField "content" m
                    c `shouldSatisfy` (not . T.isInfixOf "act now")
                    c `shouldSatisfy` (not . T.isInfixOf "Facts already established")
                    c `shouldSatisfy` (not . T.isInfixOf "Look them up with discover")
            [] -> expectationFailure "expected the signal to fire"
