{-# LANGUAGE OverloadedStrings #-}

{- | The two-turn scripted episode the end-to-end spec runs on both arms, and
the episode metadata written alongside it.
-}
module Test.EpisodeSpec.Fixture (
    sampleMeta,
    fixtureEpisode,
    fixtureMeta,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Check (CheckResult (..))

import Eval.Agent (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (..),
    defaultBudget,
    runEpisodeWith',
 )
import Eval.Episode (EpisodeMeta (..))
import Eval.Ollama (ToolCall (..), Turn (..))
import Eval.TranscriptLint (lintLine, lintMessages)

sampleMeta :: EpisodeMeta
sampleMeta =
    EpisodeMeta
        { emTask = "dateDays"
        , emArm = "off"
        , emLevers = [("SABELA_HOOGLE_RESOLVE", "0"), ("grammar", "on")]
        , emSeed = 1
        , emSeedsTried = [1, 1000004]
        , emModel = "gpt-oss:20b"
        , emStopped = "done"
        , emFinal = "Defined dateDays = 100."
        , emLint = "ok"
        , emRunId = "run-20260719-120000"
        , emCommit = "22557b9deadbeef"
        , emBuildTime = "2026-07-19T10:00:00Z"
        , emRunTime = "2026-07-19T12:00:00Z"
        , emEndpoint = "http://localhost:3300"
        , emRelinkProbe = "ok: binary 2026-07-19T10:00:00Z >= newest source"
        }

fixtureEpisode :: GrammarMode -> IO AgentRun
fixtureEpisode mode = do
    turns <- newIORef (0 :: Int)
    let installSrc =
            "-- cabal: build-depends: granite\nimport Granite.Svg\nbars' = ()" :: Text
        disp (ToolCall name a) = pure $ case name of
            "list_cells"
                | fullTrue a ->
                    Right
                        ( ToolOk
                            (object ["cells" .= [object ["source" .= installSrc]]])
                        )
                | otherwise ->
                    Right (ToolOk (object ["cells" .= [object ["id" .= (0 :: Int)]]]))
            "find_function" ->
                Right (ToolOk (object ["result" .= ("bars :: Plot -> Text" :: Text)]))
            _ -> Right (ToolOk (object ["cellId" .= (1 :: Int), "ok" .= True]))
        chat _ = do
            n <- readIORef turns
            modifyIORef' turns (+ 1)
            let insertArgs = object ["source" .= ("done = ()" :: Text)]
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
                                , "content" .= ("done" :: Text)
                                ]
                            )
                            "done"
                            []
        driver =
            Driver
                { drvChat = chat
                , drvDispatch = disp
                , drvNow = pure 0
                , drvVerify = const (pure (CheckPassed, Nothing))
                }
    runEpisodeWith'
        mode
        defaultBudget{ebMaxRepairs = 100, ebDeadlineSecs = 1 / 0}
        driver
        "define done"
        10

fixtureMeta :: Text -> AgentRun -> EpisodeMeta
fixtureMeta arm run =
    EpisodeMeta
        { emTask = "fixture"
        , emArm = arm
        , emLevers = [("grammar", arm)]
        , emSeed = 1
        , emSeedsTried = [1]
        , emModel = "test-model"
        , emStopped = arStopped run
        , emFinal = arFinal run
        , emLint = lintLine (lintMessages (arTranscript run))
        , emRunId = "run-20260719-120000"
        , emCommit = "22557b9deadbeef"
        , emBuildTime = "2026-07-19T10:00:00Z"
        , emRunTime = "2026-07-19T12:00:00Z"
        , emEndpoint = "http://localhost:3300"
        , emRelinkProbe = "ok: binary 2026-07-19T10:00:00Z >= newest source"
        }

fullTrue :: Value -> Bool
fullTrue (Object o) = KM.lookup "full" o == Just (Bool True)
fullTrue _ = False
