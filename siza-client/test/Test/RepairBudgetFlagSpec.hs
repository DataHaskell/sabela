{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | W6C: the repair budget is a configured number, not a literal. Pins the
flag, its environment fallback, and the loop and wrap-up reading the
configured value rather than the 8 the CLI used to hard-code.
-}
module Test.RepairBudgetFlagSpec (repairBudgetFlagSpec) where

import Data.Aeson (Value, object, (.=))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Options.Applicative as O
import Test.Hspec
import Test.QuickCheck

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
import Siza.Agent.Loop.WrapUp (budgetView, wrapUpDue)
import Siza.Cli.Chat (
    ChatOpts (..),
    chatBudget,
    chatOptsParser,
    defaultMaxRepairs,
    minMaxRepairs,
    resolveMaxRepairs,
 )

repairBudgetFlagSpec :: Spec
repairBudgetFlagSpec = describe "W6C: the repair budget is configured, not literal" $ do
    resolveSpec
    floorSpec
    parserSpec
    loopSpec
    dueSpec

baseOpts :: ChatOpts
baseOpts =
    ChatOpts
        { coModel = "gpt-oss:20b"
        , coUrl = Nothing
        , coTimeout = 1800
        , coMaxTurns = 40
        , coMaxRepairs = Nothing
        , coVerbose = False
        }

budgets :: Gen Int
budgets = choose (1, 12)

resolveSpec :: Spec
resolveSpec = describe "resolveMaxRepairs: flag > environment > default" $ do
    it "keeps today's behaviour when neither is given" $ do
        defaultMaxRepairs `shouldBe` 8
        resolveMaxRepairs Nothing Nothing `shouldBe` 8
    it "returns the flag for any usable value, whatever the environment says" $
        property $ \n (env :: Maybe Int) ->
            resolveMaxRepairs (Just n) (show <$> env) === max minMaxRepairs n
    it "reads the environment when the flag is absent" $
        property $ \n ->
            resolveMaxRepairs Nothing (Just (show (n :: Int)))
                === max minMaxRepairs n
    it "falls back to the default on an unreadable environment value" $
        property $ \(s :: String) ->
            null (reads s :: [(Int, String)]) ==>
                resolveMaxRepairs Nothing (Just s) === defaultMaxRepairs
    it "carries the resolved value into the episode budget" $
        property $
            forAll budgets $ \n ->
                ebMaxRepairs (chatBudget n baseOpts) === n
    it "leaves the wall-clock budget the flag's own" $
        property $
            forAll budgets $ \n ->
                ebDeadlineSecs (chatBudget n baseOpts{coTimeout = 90000})
                    === 90000

{- | A budget of zero stops the episode before its first turn, so the final
line reads like a model failure the model was never asked to avoid.
-}
floorSpec :: Spec
floorSpec = describe "the resolved budget is one the loop can spend" $ do
    it "never resolves below the floor, for any flag and any environment" $
        property $ \given (env :: Maybe Int) ->
            resolveMaxRepairs given (show <$> env) >= minMaxRepairs
    it "floors an unusable flag rather than admitting it" $
        property $
            forAll unusable $ \n ->
                resolveMaxRepairs (Just n) Nothing === minMaxRepairs
    it "floors an unusable environment value too" $
        property $
            forAll unusable $ \n ->
                resolveMaxRepairs Nothing (Just (show n)) === minMaxRepairs
    it "states the floor in --help" $
        helpText `shouldSatisfy` isInfixOf ("minimum " ++ show minMaxRepairs)
    it "leaves the loop at least one turn to spend" $ do
        run <- runRepairing (resolveMaxRepairs (Just 0) Nothing)
        arTurns run `shouldSatisfy` (> 0)

-- | Budgets the loop cannot spend a round of.
unusable :: Gen Int
unusable = choose (-50, 0)

helpText :: String
helpText = case O.execParserPure O.defaultPrefs info ["--help"] of
    O.Failure f -> fst (O.renderFailure f "siza chat")
    _ -> ""
  where
    info = O.info (chatOptsParser O.<**> O.helper) mempty

parserSpec :: Spec
parserSpec = describe "the chat subcommand exposes --max-repairs" $ do
    it "parses any value the flag is given" $
        property $
            forAll (choose (1, 500)) $ \n ->
                (coMaxRepairs <$> parseChat ["--max-repairs", show n])
                    === Just (Just n)
    it "is absent by default, and then resolves to 8" $ do
        let parsed = parseChat []
        (coMaxRepairs <$> parsed) `shouldBe` Just Nothing
        fmap (\o -> resolveMaxRepairs (coMaxRepairs o) Nothing) parsed
            `shouldBe` Just 8

parseChat :: [String] -> Maybe ChatOpts
parseChat args =
    case O.execParserPure O.defaultPrefs (O.info chatOptsParser mempty) args of
        O.Success o -> Just o
        _ -> Nothing

loopSpec :: Spec
loopSpec =
    describe "the loop stops at the configured budget" $ do
        it "spends exactly the configured repair rounds, for any budget" $
            property $
                forAll budgets $ \n -> ioProperty $ do
                    run <- runRepairing n
                    pure $
                        (arStopped run, arTurns run) === ("repair_budget", 2 * n)
        it "a raised budget outlives the 8 the CLI used to hard-code" $ do
            run <- runRepairing 13
            arStopped run `shouldBe` "repair_budget"
            arTurns run `shouldSatisfy` (> 2 * 8)

{- | An episode that spends one repair round every other turn: the model
writes a failing cell, then replies with prose, which re-enters repair.
-}
runRepairing :: Int -> IO AgentRun
runRepairing maxRepairs = do
    turns <- newIORef (0 :: Int)
    payloads <- newIORef (0 :: Int)
    let chat _ = do
            n <- bump turns
            pure . Right $
                if odd n
                    then writeTurn ("total" <> tShow n <> " = missingName")
                    else proseTurn
        dispatch tc = do
            k <- bump payloads
            pure . Right . ToolOk $ case tcName tc of
                "insert_cell" -> redCell k
                _ -> object []
        driver =
            Driver
                { drvChat = chat
                , drvDispatch = dispatch
                , drvNow = pure 0
                , drvVerify = const (pure (CheckFailed, Just "not yet"))
                }
    runEpisodeSeeded
        []
        (const (pure ()))
        GrammarOff
        EpisodeBudget{ebMaxRepairs = maxRepairs, ebDeadlineSecs = 1 / 0}
        driver
        "sum the numbers"
        (4 * maxRepairs + 8)

{- | Each write reports a diagnostic no earlier write reported, so the loop
never reads the round as no-progress and the repair budget is what ends it.
-}
redCell :: Int -> Value
redCell k =
    object
        [ "cellId" .= (1 :: Int)
        , "execution"
            .= object
                [ "ok" .= False
                , "error" .= ("undefined name at step " <> tShow k)
                ]
        ]

bump :: IORef Int -> IO Int
bump ref = atomicModifyIORef' ref (\n -> (n + 1, n + 1))

writeTurn :: Text -> Turn
writeTurn src =
    Turn
        (object ["role" .= ("assistant" :: Text), "content" .= ("" :: Text)])
        ""
        [ToolCall "insert_cell" (object ["source" .= src])]

proseTurn :: Turn
proseTurn =
    Turn
        (object ["role" .= ("assistant" :: Text), "content" .= msg])
        msg
        []
  where
    msg = "still working on it" :: Text

dueSpec :: Spec
dueSpec = describe "wrapUpDue's repair clause is relative to the budget" $ do
    it "fires on the last round of any budget, and not before" $
        property $
            forAll budgets $ \cap ->
                forAll (choose (0, cap)) $ \spent ->
                    wrapUpDue (view cap spent)
                        === (spent > 0 && cap - spent <= 1)
    it "a budget past 8 is not wrapped up at the seventh repair" $
        wrapUpDue (view 30 7) `shouldBe` False
  where
    view cap spent = budgetView 500 0 cap spent 0 (1 / 0)

tShow :: Int -> Text
tShow = T.pack . show
