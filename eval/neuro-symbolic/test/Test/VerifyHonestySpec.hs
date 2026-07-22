{-# LANGUAGE OverloadedStrings #-}

{- | R5-T5 verify honesty at the loop seam: uncheckable injects "not yet
confirmed — here is what to run" (never a failure claim); a denial needs
'CheckFailed'; a satisfied contract emits the one done-signal R5-T4 consumes.
-}
module Test.VerifyHonestySpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Check (CheckResult (..))
import Siza.Agent.Messages (doneSignal, unconfirmedMessage)
import Test.Hspec

import Eval.Agent (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    defaultBudget,
    runEpisodeWith,
 )
import Eval.Ollama (ToolCall (..), Turn (..))

spec :: Spec
spec = describe "verify-channel honesty (R5-T5, three-valued)" $ do
    describe "unconfirmedMessage (never a failure claim)" $ do
        it "carries the what-to-run guidance verbatim" $
            unconfirmedMessage 1 [] (Just "print the best expression")
                `shouldSatisfy` T.isInfixOf "print the best expression"
        it "never contains a failure claim, over the diagnosis grid" $
            mapM_
                ( \(owned, missing, guide) ->
                    T.toLower (unconfirmedMessage owned missing guide)
                        `shouldSatisfy` (not . T.isInfixOf "fail")
                )
                [ (o, m, g)
                | o <- [0, 1, 2]
                , m <- [[], ["evalExpr"]]
                , g <- [Nothing, Just "run the check cell"]
                ]
        it "says not yet confirmed and diagnoses a missing cell" $ do
            let msg = unconfirmedMessage 0 [] Nothing
            msg `shouldSatisfy` T.isInfixOf "not yet confirmed"
            msg `shouldSatisfy` T.isInfixOf "insert_cell"

    describe "Stop under an uncheckable verdict" $ do
        it "injects not-yet-confirmed guidance, never a denial" $ do
            driver <-
                scriptedDriver
                    healthyDispatch
                    [callTurn "insert_cell", doneTurn, doneTurn, doneTurn, doneTurn]
                    (pure (CheckUncheckable, Just "print the best expression"))
            run <- runEpisodeWith openBudget driver "task" 10
            let verifies = channelContents "verify" (arTranscript run)
            verifies `shouldSatisfy` (not . null)
            -- The first injection carries the full guidance; R5-T4 dedups the
            -- replays into back-references, which must still never claim failure.
            head verifies `shouldSatisfy` T.isInfixOf "not yet confirmed"
            head verifies `shouldSatisfy` T.isInfixOf "print the best expression"
            mapM_
                ( \c ->
                    T.toLower c `shouldSatisfy` (not . T.isInfixOf "fail")
                )
                verifies
        it "a failed verdict still injects the evidence-backed denial" $ do
            driver <-
                scriptedDriver
                    healthyDispatch
                    [callTurn "insert_cell", doneTurn, doneTurn, doneTurn, doneTurn]
                    (pure (CheckFailed, Just "This required example fails: `x == 1`."))
            run <- runEpisodeWith openBudget driver "task" 10
            let verifies = channelContents "verify" (arTranscript run)
            verifies `shouldSatisfy` (not . null)
            head verifies `shouldSatisfy` T.isInfixOf "x == 1"

    describe "done-signal (the stop signal R5-T4 consumes)" $ do
        it "emits ONE signal when the contract is satisfied and the model keeps going" $ do
            driver <-
                scriptedDriver
                    healthyDispatch
                    [ callTurn "insert_cell"
                    , callTurn "read_cell"
                    , callTurn "read_cell"
                    , doneTurn
                    ]
                    (pure (CheckPassed, Nothing))
            run <- runEpisodeWith openBudget driver "task" 10
            arStopped run `shouldBe` "done"
            let signals =
                    [ c
                    | c <- channelContents "verify" (arTranscript run)
                    , doneSignal `T.isInfixOf` c
                    ]
            length signals `shouldBe` 1
        it "emits NOTHING mid-loop when the contract is not satisfied" $ do
            driver <-
                scriptedDriver
                    healthyDispatch
                    [ callTurn "insert_cell"
                    , callTurn "read_cell"
                    , callTurn "read_cell"
                    , doneTurn
                    , doneTurn
                    , doneTurn
                    ]
                    (pure (CheckFailed, Nothing))
            run <- runEpisodeWith openBudget driver "task" 10
            -- No done-signal, and no denial injected before the model stopped.
            let msgs = arTranscript run
                firstStopIx = length (takeWhile (not . isDoneTurnMsg) msgs)
                before = take firstStopIx msgs
            channelContents "verify" before `shouldBe` []
            [c | c <- channelContents "verify" msgs, doneSignal `T.isInfixOf` c]
                `shouldBe` []
        it "does not add chatter on the happy path (write then stop)" $ do
            driver <-
                scriptedDriver
                    healthyDispatch
                    [callTurn "insert_cell", doneTurn]
                    (pure (CheckPassed, Nothing))
            run <- runEpisodeWith openBudget driver "task" 10
            arStopped run `shouldBe` "done"
            channelContents "verify" (arTranscript run) `shouldBe` []

openBudget :: EpisodeBudget
openBudget = defaultBudget{ebMaxRepairs = maxBound, ebDeadlineSecs = 1 / 0}

callTurn :: Text -> Turn
callTurn name =
    Turn
        (object ["role" .= ("assistant" :: Text)])
        ""
        [ToolCall name (object [])]

doneTurn :: Turn
doneTurn =
    Turn
        (object ["role" .= ("assistant" :: Text), "content" .= ("ok" :: Text)])
        "ok"
        []

isDoneTurnMsg :: Value -> Bool
isDoneTurnMsg (Object o) =
    KM.lookup "role" o == Just (String "assistant")
        && KM.lookup "content" o == Just (String "ok")
isDoneTurnMsg _ = False

-- | Contents of tool-channel messages with the given tool_name.
channelContents :: Text -> [Value] -> [Text]
channelContents name msgs =
    [ c
    | Object o <- msgs
    , KM.lookup "role" o == Just (String "tool")
    , KM.lookup "tool_name" o == Just (String name)
    , Just (String c) <- [KM.lookup (K.fromText "content") o]
    ]

healthyDispatch :: ToolCall -> IO (Either Text ToolOutcome)
healthyDispatch (ToolCall name _)
    | name `elem` ["insert_cell", "replace_cell_source"] =
        pure (Right (ToolOk (object ["cellId" .= (1 :: Int), "ok" .= True])))
    | name == "list_cells" =
        pure (Right (ToolOk (object ["cells" .= ([] :: [Value])])))
    | otherwise = pure (Right (ToolOk (object [])))

scriptedDriver ::
    (ToolCall -> IO (Either Text ToolOutcome)) ->
    [Turn] ->
    IO (CheckResult, Maybe Text) ->
    IO Driver
scriptedDriver disp script verify = do
    cursor <- newIORef (0 :: Int)
    let nextTurn _msgs = do
            i <- readIORef cursor
            modifyIORef' cursor (+ 1)
            pure (Right (script !! min i (length script - 1)))
    pure
        Driver
            { drvChat = nextTurn
            , drvDispatch = disp
            , drvNow = pure 0
            , drvVerify = verify
            }
