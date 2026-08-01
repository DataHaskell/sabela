{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.WrapUpLoopSpec (wrapUpLoopSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
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
import Siza.Agent.Loop.WrapUp (wrapUpMarker)
import Test.DiscoverFixtures (argText, installNamesFile, runCat, textField)
import Test.WrapUpSpec (searchAdvicePhrases)

assistant :: Text -> Value
assistant t = object ["role" .= ("assistant" :: Text), "content" .= t]

-- | A turn that ends the model's work by saying so, with no tool calls.
summaryStopTurn :: Turn
summaryStopTurn = Turn (assistant "no further progress") "no further progress" []

-- | A turn that says nothing at all: not a summary, and not a completion.
emptyStopTurn :: Turn
emptyStopTurn = Turn (assistant "") "" []

writeTurn :: Text -> Turn
writeTurn src =
    Turn (assistant "") "" [ToolCall "insert_cell" (object ["source" .= src])]

okDispatch :: ToolCall -> IO (Either Text ToolOutcome)
okDispatch tc = case tcName tc of
    "insert_cell" ->
        pure . Right . ToolOk $
            object ["cellId" .= (1 :: Int), "execution" .= object ["ok" .= True]]
    _ -> pure (Right (ToolOk (object [])))

discoverDispatch :: ToolCall -> IO (Either Text ToolOutcome)
discoverDispatch tc = case tcName tc of
    "discover" -> Right . ToolOk <$> runCat (argText "query" (tcArgs tc))
    _ -> okDispatch tc

driverWith ::
    ([Value] -> IO (Either Text Turn)) ->
    (ToolCall -> IO (Either Text ToolOutcome)) ->
    (CheckResult, Maybe Text) ->
    Driver
driverWith chat dispatch verdict =
    Driver
        { drvChat = chat
        , drvDispatch = dispatch
        , drvNow = pure 0
        , drvVerify = const (pure verdict)
        }

runShaped :: Driver -> EpisodeBudget -> Int -> IO AgentRun
runShaped driver budget =
    runEpisodeSeeded [] (const (pure ())) GrammarOff budget driver "sum the numbers"

defBudget :: EpisodeBudget
defBudget = EpisodeBudget{ebMaxRepairs = 4, ebDeadlineSecs = 600}

markerCount :: AgentRun -> Int
markerCount run =
    length
        [ m
        | m <- arTranscript run
        , wrapUpMarker `T.isInfixOf` textField "content" m
        ]

contents :: AgentRun -> [Text]
contents run = map (textField "content") (arTranscript run)

scriptChat :: IORef Int -> (Int -> Turn) -> [Value] -> IO (Either Text Turn)
scriptChat ref f _ = do
    n <- readIORef ref
    writeIORef ref (n + 1)
    pure (Right (f (n + 1)))

wrapUpLoopSpec :: Spec
wrapUpLoopSpec = describe "budget-exhaustion wrap-up in the real loop (R6-T3)" $ do
    loopStopGridSpec
    happyFloorSpec

loopStopGridSpec :: Spec
loopStopGridSpec = describe "the real loop: no stop reason yields an empty final" $ do
    it "max_turns: wrap-up fires once, final non-empty, post-wrap-up silence (R5.7)" $ do
        installNamesFile
        calls <- newIORef (0 :: Int)
        let chat = scriptChat calls $ \n ->
                Turn
                    (assistant "")
                    ""
                    [ ToolCall
                        "discover"
                        (object ["query" .= ("mysteryname" <> T.pack (show n))])
                    ]
            driver = driverWith chat discoverDispatch (CheckUncheckable, Nothing)
        run <- runShaped driver defBudget 4
        arStopped run `shouldBe` "max_turns"
        T.strip (arFinal run) `shouldSatisfy` (not . T.null)
        markerCount run `shouldBe` 1
        let after =
                drop 1 $
                    dropWhile
                        (not . T.isInfixOf wrapUpMarker)
                        (contents run)
        forM_ after $ \c ->
            forM_ searchAdvicePhrases $ \p ->
                (c, p, p `T.isInfixOf` T.toLower c) `shouldSatisfy` \(_, _, b) -> not b
    it "max_turns performs no hidden repair calls after exhaustion" $ do
        chatCalls <- newIORef (0 :: Int)
        dispatched <- newIORef ([] :: [Text])
        let chat = scriptChat chatCalls (const (writeTurn "total = missingName"))
            dispatch tc = do
                modifyIORef' dispatched (++ [tcName tc])
                pure . Right . ToolOk $
                    object
                        [ "cellId" .= (1 :: Int)
                        , "execution" .= object ["ok" .= False, "outputs" .= ([] :: [Value])]
                        ]
            driver = driverWith chat dispatch (CheckUncheckable, Nothing)
        run <- runShaped driver defBudget 1
        arStopped run `shouldBe` "max_turns"
        seen <- readIORef dispatched
        seen `shouldBe` ["list_cells", "insert_cell"]
        arToolCalls run `shouldBe` 1
    it "repair_budget: wrap-up fires once and the final is non-empty" $ do
        calls <- newIORef (0 :: Int)
        let chat = scriptChat calls (const summaryStopTurn)
            driver = driverWith chat okDispatch (CheckFailed, Just "expected 3, got 1")
        run <-
            runShaped
                driver
                EpisodeBudget{ebMaxRepairs = 2, ebDeadlineSecs = 600}
                20
        arStopped run `shouldBe` "repair_budget"
        T.strip (arFinal run) `shouldSatisfy` (not . T.null)
        markerCount run `shouldBe` 1
    it "deadline: wrap-up fires once and the final is non-empty" $ do
        calls <- newIORef (0 :: Int)
        clock <- newIORef (0 :: Double)
        let chat = scriptChat calls $ \_ ->
                Turn (assistant "") "" [ToolCall "list_bindings" (object [])]
            now = do
                t <- readIORef clock
                writeIORef clock (t + 450)
                pure t
            driver =
                (driverWith chat okDispatch (CheckUncheckable, Nothing))
                    { drvNow = now
                    }
        run <-
            runShaped
                driver
                EpisodeBudget{ebMaxRepairs = 4, ebDeadlineSecs = 1000}
                50
        arStopped run `shouldBe` "deadline"
        T.strip (arFinal run) `shouldSatisfy` (not . T.null)
        markerCount run `shouldBe` 1
    it "an empty model summary is no_reply, and the final is still non-empty" $ do
        calls <- newIORef (0 :: Int)
        let chat = scriptChat calls $ \n ->
                if n == 1 then writeTurn "total = 6" else emptyStopTurn
            driver = driverWith chat okDispatch (CheckPassed, Nothing)
        run <- runShaped driver defBudget 20
        arStopped run `shouldBe` "no_reply"
        T.strip (arFinal run) `shouldSatisfy` (not . T.null)
    it "stuck: the final stays non-empty" $ do
        calls <- newIORef (0 :: Int)
        let chat = scriptChat calls (const (Turn (assistant "did it") "did it" []))
            driver = driverWith chat okDispatch (CheckFailed, Just "expected 3, got 1")
        run <-
            runShaped
                driver
                EpisodeBudget{ebMaxRepairs = 9, ebDeadlineSecs = 600}
                20
        arStopped run `shouldBe` "stuck"
        T.strip (arFinal run) `shouldSatisfy` (not . T.null)
    it "error: the final stays non-empty" $ do
        let driver =
                driverWith
                    (const (pure (Left "boom")))
                    okDispatch
                    (CheckUncheckable, Nothing)
        run <- runShaped driver defBudget 20
        arStopped run `shouldBe` "error"
        T.strip (arFinal run) `shouldSatisfy` (not . T.null)

happyFloorSpec :: Spec
happyFloorSpec = describe "R9.8: happy-path floors add zero wrap-up bytes" $ do
    it "the jsonSum floor shape: write, succeed, summarise, stop — untouched" $ do
        calls <- newIORef (0 :: Int)
        let summary = "jsonSum = 60"
            chat = scriptChat calls $ \n ->
                if n == 1
                    then writeTurn "jsonSum = sum [10, 20, 30]"
                    else Turn (assistant summary) summary []
            driver = driverWith chat okDispatch (CheckPassed, Nothing)
        run <- runShaped driver defBudget 25
        arStopped run `shouldBe` "done"
        arFinal run `shouldBe` summary
        markerCount run `shouldBe` 0
        forM_ (contents run) $ \c -> do
            c `shouldSatisfy` (not . T.isInfixOf "Remaining turn budget")
            c `shouldSatisfy` (not . T.isInfixOf "Stopped (")
    it "the symbolicRegression floor shape: two writes then done — untouched" $ do
        calls <- newIORef (0 :: Int)
        let summary = "Best expression: (x * x), SSE: 0.0"
            chat = scriptChat calls $ \case
                1 -> writeTurn "candidates = [(1,1),(2,4),(3,9),(4,16)]"
                2 -> writeTurn "best = fit candidates"
                _ -> Turn (assistant summary) summary []
            driver = driverWith chat okDispatch (CheckPassed, Nothing)
        run <- runShaped driver defBudget 26
        arStopped run `shouldBe` "done"
        arFinal run `shouldBe` summary
        markerCount run `shouldBe` 0
        forM_ (contents run) $ \c ->
            c `shouldSatisfy` (not . T.isInfixOf wrapUpMarker)
