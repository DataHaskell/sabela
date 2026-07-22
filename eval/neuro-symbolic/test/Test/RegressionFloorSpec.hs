{-# LANGUAGE OverloadedStrings #-}

{- | R9 regression-floor pins: the behaviours the 2026-07-18 triage found
working are pinned BEFORE anything changes, so improvements are provable and
regressions are caught in the round they appear.
-}
module Test.RegressionFloorSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.SelfHeal (selfHealNote, sourceDelta)
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
import Eval.Tools (renderOutcome)
import Eval.TranscriptLint (lintMessages)

spec :: Spec
spec = describe "R9 regression floor" $ do
    describe "R9.8 dateDays happy-path shape" $
        it "write, succeed, summarise, stop - 0 discover calls, no chatter" $ do
            calls <- newIORef ([] :: [Text])
            turns <- newIORef (0 :: Int)
            let disp (ToolCall name _) = do
                    modifyIORef' calls (<> [name])
                    pure $ case name of
                        "list_cells" ->
                            Right (ToolOk (object ["cells" .= ([] :: [Value])]))
                        _ ->
                            Right
                                (ToolOk (object ["cellId" .= (1 :: Int), "ok" .= True]))
                chat _ = do
                    n <- readIORef turns
                    modifyIORef' turns (+ 1)
                    let src = "dateDays :: Integer\ndateDays = 100" :: Text
                        insertArgs = object ["source" .= src]
                        raw =
                            object
                                [ "role" .= ("assistant" :: Text)
                                , "tool_calls"
                                    .= [ object
                                            [ "function"
                                                .= object
                                                    [ "name" .= ("insert_cell" :: Text)
                                                    , "arguments" .= insertArgs
                                                    ]
                                            ]
                                       ]
                                ]
                    pure . Right $
                        if n == 0
                            then Turn raw "" [ToolCall "insert_cell" insertArgs]
                            else
                                Turn
                                    ( object
                                        [ "role" .= ("assistant" :: Text)
                                        , "content" .= ("dateDays = 100." :: Text)
                                        ]
                                    )
                                    "dateDays = 100."
                                    []
                driver =
                    Driver
                        { drvChat = chat
                        , drvDispatch = disp
                        , drvNow = pure 0
                        , drvVerify = pure (CheckPassed, Nothing)
                        }
            run <-
                runEpisodeWith'
                    GrammarOn
                    defaultBudget{ebMaxRepairs = 100, ebDeadlineSecs = 1 / 0}
                    driver
                    "Define dateDays :: Integer as 100."
                    10
            arStopped run `shouldBe` "done"
            arTurns run `shouldBe` 2
            arToolCalls run `shouldBe` 1
            issued <- readIORef calls
            -- The model made no discover-class call; the episode is one write.
            filter (`elem` ["discover", "find_function"]) issued `shouldBe` []
            -- No added chatter: system, user, assistant, result, assistant.
            length (arTranscript run) `shouldBe` 5
            -- The happy path is lint-clean under the R8.4 rules.
            lintMessages (arTranscript run) `shouldBe` []

    describe "R9.3 await-idle settled answer" $
        it "stays terse and truthful on an idle kernel" $ do
            let out = renderOutcome (Right (ToolOk (object ["state" .= ("idle" :: Text)])))
            out `shouldSatisfy` T.isInfixOf "idle"
            T.length out `shouldSatisfy` (<= 40)
            out `shouldSatisfy` (not . T.isInfixOf "\n")

    describe "R9.6/R7.1/R7.2 self-heal added/removed report" $ do
        it "emits NO note when healing changed nothing" $
            selfHealNote "x = 1" "x = 1" `shouldBe` Nothing
        it "reports the true diff for a silent dep add" $ do
            let before = "import Data.Time\nd = 1" :: Text
                after = "-- cabal: build-depends: time\nimport Data.Time\nd = 1"
            sourceDelta before after
                `shouldBe` ([], ["-- cabal: build-depends: time"])
            case selfHealNote before after of
                Nothing -> expectationFailure "expected a heal note"
                Just note -> do
                    let s = T.pack (show note)
                    s `shouldSatisfy` T.isInfixOf "build-depends: time"
        it "note exists iff the source changed (general invariant)" $ do
            let srcs =
                    [ ""
                    , "x = 1"
                    , "x = 2"
                    , "-- cabal: build-depends: time\nx = 1"
                    ] ::
                        [Text]
            mapM_
                ( \(b, a) ->
                    ((b, a), selfHealNote b a == Nothing) `shouldBe` ((b, a), b == a)
                )
                [(b, a) | b <- srcs, a <- srcs]
