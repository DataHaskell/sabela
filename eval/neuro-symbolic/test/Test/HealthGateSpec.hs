{-# LANGUAGE OverloadedStrings #-}

module Test.HealthGateSpec (spec) where

import Data.Aeson (object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Check (CheckResult (..))
import Test.Hspec

import Eval.Agent (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    StopDecision (..),
    defaultBudget,
    ownedCellOutcome,
    runEpisodeWith,
    stopDecision,
 )
import Eval.Messages (reenterMessage)
import Eval.Ollama (ToolCall (..), Turn (..))
import Eval.Owned (OwnedCell (..), noProgressStep, redSignature)
import Eval.Task (Grader (..), Task (..))

insertOf :: ToolCall
insertOf = ToolCall "insert_cell" (object ["source" .= ("x = 1" :: Text)])

replaceOf :: ToolCall
replaceOf = ToolCall "replace_cell_source" (object [])

okOutcome :: Int -> ToolOutcome
okOutcome cid = ToolOk (object ["cellId" .= cid, "ok" .= True])

redOutcome :: Int -> ToolOutcome
redOutcome cid =
    ToolOk $
        object
            [ "cellId" .= cid
            , "ok" .= False
            , "guidance"
                .= [ object
                        ["category" .= ("type-mismatch" :: Text), "message" .= ("fix the type" :: Text)]
                   ]
            ]

spec :: Spec
spec = describe "B2 health gate" $ do
    describe "ownedCellOutcome" $ do
        it "records an inserted cell and its health" $
            ownedCellOutcome insertOf (Right (okOutcome 3)) `shouldBe` Just (3, True)

        it "records a replaced cell as unhealthy when ok is false" $
            ownedCellOutcome replaceOf (Right (redOutcome 5)) `shouldBe` Just (5, False)

        it "ignores a non-owning tool (read_cell)" $
            ownedCellOutcome (ToolCall "read_cell" (object [])) (Right (okOutcome 1))
                `shouldBe` Nothing

        it "ignores a transport error with no cell id" $
            ownedCellOutcome insertOf (Left "boom") `shouldBe` Nothing

        it "ignores an owning call whose outcome carries no cellId" $
            ownedCellOutcome insertOf (Right (ToolOk (object ["ok" .= True])))
                `shouldBe` Nothing

    describe "stopDecision" $ do
        it "stops when nothing is owned" $
            stopDecision Map.empty `shouldBe` Stop

        it "stops when every owned cell is healthy" $
            stopDecision (Map.fromList [(1, True), (2, True)]) `shouldBe` Stop

        it "re-enters when an owned cell is unhealthy" $
            stopDecision (Map.fromList [(1, True), (2, False)]) `shouldBe` Reenter [2]

    describe "redSignature (reenter no-progress detector)" $ do
        let owned =
                Map.fromList
                    [ (2, OwnedCell False "ambiguous take" "src")
                    , (1, OwnedCell False "not in scope foo" "src")
                    ]
        it "is stable and sorted by cell id regardless of the red list order" $
            redSignature [2, 1] owned
                `shouldBe` [(1, "not in scope foo"), (2, "ambiguous take")]
        it "is unchanged when the same cells stay red with the same errors" $
            redSignature [1, 2] owned `shouldBe` redSignature [2, 1] owned
        it "changes when a cell's diagnostic changes (real progress)" $
            ( redSignature [1] owned
                == redSignature [1] (Map.insert 1 (OwnedCell False "different" "src") owned)
            )
                `shouldBe` False
        it "ignores cells absent from the red list" $
            map fst (redSignature [1] owned) `shouldBe` [1]

    describe "noProgressStep (oscillation-aware reenter guard)" $ do
        let owned =
                Map.fromList
                    [ (1, OwnedCell False "not in scope foo" "src")
                    , (2, OwnedCell False "ambiguous take" "src")
                    ]
            sigA = redSignature [1] owned
            sigB = redSignature [2] owned
        it "a brand-new signature is progress" $
            snd (noProgressStep Set.empty sigA) `shouldBe` False
        it "an immediately-repeated signature is no progress" $
            snd (noProgressStep (fst (noProgressStep Set.empty sigA)) sigA)
                `shouldBe` True
        it "catches an A/B/A oscillation a compare-to-previous check misses" $
            let s1 = fst (noProgressStep Set.empty sigA)
                s2 = fst (noProgressStep s1 sigB)
             in snd (noProgressStep s2 sigA) `shouldBe` True

    describe "reenterMessage" $ do
        it "names the unhealthy cell ids and pushes for a fix" $ do
            let msg = reenterMessage [(2, ""), (4, "")]
            ("2" `T.isInfixOf` msg) `shouldBe` True
            ("4" `T.isInfixOf` msg) `shouldBe` True
            (T.length msg > 0) `shouldBe` True

    describe "runEpisodeWith (re-entry on an unhealthy owned cell)" $ do
        it "re-enters when the model says done with a red cell, stops once it is green" $ do
            driver <-
                scriptedDriver
                    redInsertHealthyReplace
                    [callTurn "insert_cell", doneTurn, callTurn "replace_cell_source", doneTurn]
            run <- runEpisodeWith openBudget driver (taskPrompt dummyTask) 10
            arStopped run `shouldBe` "done"
            arToolCalls run `shouldBe` 2
            arTurns run `shouldBe` 4

        it "stops immediately when the model writes a healthy cell then says done" $ do
            driver <- scriptedDriver alwaysHealthy [callTurn "insert_cell", doneTurn]
            run <- runEpisodeWith openBudget driver (taskPrompt dummyTask) 10
            arStopped run `shouldBe` "done"
            arToolCalls run `shouldBe` 1
            arTurns run `shouldBe` 2

        it "does not loop forever: hits max_turns if the model never fixes the cell" $ do
            driver <-
                scriptedDriver
                    redInsertHealthyReplace
                    (concat (replicate 20 [callTurn "insert_cell", doneTurn]))
            run <- runEpisodeWith openBudget driver (taskPrompt dummyTask) 4
            arStopped run `shouldBe` "max_turns"

    describe "Stop gates on the covering test, not owned-cell health" $ do
        it "finishes when the deliverable passes its covering test" $ do
            driver <- scriptedDriverV alwaysHealthy [callTurn "insert_cell", doneTurn] True
            run <- runEpisodeWith openBudget driver (taskPrompt dummyTask) 10
            arStopped run `shouldBe` "done"

        it "re-enters a healthy-but-unverified deliverable, bounded by the budget" $ do
            -- owned cells compile (alwaysHealthy) yet drvVerify = False, so Stop
            -- must not finish; it re-enters until the repair budget is spent.
            driver <- scriptedDriverV alwaysHealthy (replicate 20 doneTurn) False
            run <- runEpisodeWith tightBudget driver (taskPrompt dummyTask) 50
            arStopped run `shouldBe` "repair_budget"

openBudget :: EpisodeBudget
openBudget = defaultBudget{ebMaxRepairs = maxBound, ebDeadlineSecs = 1 / 0}

tightBudget :: EpisodeBudget
tightBudget = defaultBudget{ebMaxRepairs = 2, ebDeadlineSecs = 1 / 0}

dummyTask :: Task
dummyTask = Task "t" "do the thing" (ByValue "True")

callTurn :: Text -> Turn
callTurn name =
    Turn
        (object ["role" .= ("assistant" :: Text)])
        "```haskell\nx = 1\n```"
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
            , drvVerify = pure (CheckPassed, Nothing)
            }

scriptedDriverV ::
    (ToolCall -> IO (Either Text ToolOutcome)) -> [Turn] -> Bool -> IO Driver
scriptedDriverV disp script verdict = do
    d <- scriptedDriver disp script
    pure
        d
            { drvVerify =
                pure (if verdict then CheckPassed else CheckFailed, Nothing)
            }

redInsertHealthyReplace :: ToolCall -> IO (Either Text ToolOutcome)
redInsertHealthyReplace (ToolCall name _)
    | name == "insert_cell" = ok 1 False
    | name == "replace_cell_source" = ok 1 True
    | otherwise = pure (Right (ToolOk (object [])))

alwaysHealthy :: ToolCall -> IO (Either Text ToolOutcome)
alwaysHealthy _ = ok 1 True

ok :: Int -> Bool -> IO (Either Text ToolOutcome)
ok cid healthy = pure (Right (ToolOk (object ["cellId" .= cid, "ok" .= healthy])))
