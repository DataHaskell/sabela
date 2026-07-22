{-# LANGUAGE OverloadedStrings #-}

{- | The scaffold as a disclosed, outcome-gated action in the shared loop that
@siza chat@ runs (Chat.hs wires 'runEpisodeSeeded' directly): every scaffold
write is disclosed in the transcript and to the trace sink, and the orienting
note appears only after the scaffold cell verifiably ran clean (R7.4).
-}
module Test.ScaffoldChatSpec (scaffoldChatSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
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
    defaultBudget,
    runEpisodeSeeded,
 )
import Siza.Agent.NoteLedger (assertedLive)
import Siza.Agent.Scaffold (scaffoldNoteFor)

-- | A two-cell data-file request (the revenuePipeline shape).
prompt :: Text
prompt =
    "A CSV `revenue.csv` with columns `month` and `revenue` is in the working \
    \directory. First load it and show the DataFrame in one cell. Then in a \
    \second cell, plot revenue by month and show the chart."

scaffoldChatSpec :: Spec
scaffoldChatSpec = describe "scaffold in the shared chat loop (R7.4, M16)" $ do
    it "discloses the scaffold write in the transcript and the trace sink" $ do
        (msgs, emitted) <- runWith okInsert
        let scaffolds = filter (isTool "scaffold") msgs
        length scaffolds `shouldBe` 1
        contentOf (head scaffolds) `shouldSatisfy` T.isInfixOf "`revenue.csv`"
        contentOf (head scaffolds) `shouldSatisfy` T.isInfixOf "\"ok\":true"
        emitted `shouldSatisfy` T.isInfixOf "scaffold"

    it
        "injects the truthful note only after the verified scaffold, and \
        \after its disclosure"
        $ do
            (msgs, _) <- runWith okInsert
            let notes = [m | m <- laterUserMsgs msgs, isNote m]
            map contentOf notes
                `shouldBe` [scaffoldNoteFor prompt "revenue.csv"]
            let idxOf p = length (takeWhile (not . p) msgs)
            idxOf (isTool "scaffold") < idxOf isNote `shouldBe` True

    it "emits no state-asserting note when the scaffold cell ran red (R7.4)" $ do
        (msgs, _) <- runWith redInsert
        concatMap (assertedLive . contentOf) (laterUserMsgs msgs)
            `shouldBe` []
        filter (isTool "scaffold") msgs `shouldSatisfy` (not . null)

    it "emits no state-asserting note on a transport-failed scaffold (R7.4)" $ do
        (msgs, _) <- runWith failInsert
        concatMap (assertedLive . contentOf) (laterUserMsgs msgs)
            `shouldBe` []
  where
    isNote m = not (null (assertedLive (contentOf m)))

okInsert :: ToolCall -> IO (Either Text ToolOutcome)
okInsert c
    | tcName c == "insert_cell" =
        pure (Right (ToolOk (object ["cellId" .= (1 :: Int), "ok" .= True])))
    | otherwise = pure (Right (ToolOk (object [])))

redInsert :: ToolCall -> IO (Either Text ToolOutcome)
redInsert c
    | tcName c == "insert_cell" =
        pure (Right (ToolOk (object ["cellId" .= (1 :: Int), "ok" .= False])))
    | otherwise = pure (Right (ToolOk (object [])))

failInsert :: ToolCall -> IO (Either Text ToolOutcome)
failInsert c
    | tcName c == "insert_cell" = pure (Left "transport down")
    | otherwise = pure (Right (ToolOk (object [])))

-- | Run one scripted episode; the model immediately stops with no calls.
runWith ::
    (ToolCall -> IO (Either Text ToolOutcome)) -> IO ([Value], Text)
runWith disp = do
    emitted <- newIORef ("" :: Text)
    let doneTurn =
            Turn (object ["role" .= ("assistant" :: Text)]) "done" []
        driver =
            Driver
                { drvChat = const (pure (Right doneTurn))
                , drvDispatch = disp
                , drvNow = pure 0
                , drvVerify = pure (CheckPassed, Nothing)
                }
        emit t = modifyIORef' emitted (<> t)
        budget = defaultBudget{ebMaxRepairs = maxBound, ebDeadlineSecs = 1 / 0}
    run <- runEpisodeSeeded [] emit GrammarOff budget driver prompt 10
    out <- readIORef emitted
    pure (arTranscript run, out)

-- | Messages with role user, excluding the first (the task prompt itself).
laterUserMsgs :: [Value] -> [Value]
laterUserMsgs msgs = drop 1 [m | m <- msgs, roleOf m == "user"]

isTool :: Text -> Value -> Bool
isTool name m = roleOf m == "tool" && strAt "tool_name" m == name

contentOf :: Value -> Text
contentOf = strAt "content"

roleOf :: Value -> Text
roleOf = strAt "role"

strAt :: Text -> Value -> Text
strAt k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
strAt _ _ = ""
