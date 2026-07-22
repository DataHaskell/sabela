{-# LANGUAGE OverloadedStrings #-}

module Test.EpisodeSpec (spec, sampleMeta) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Check (CheckResult (..))
import System.Directory (
    createDirectoryIfMissing,
    getTemporaryDirectory,
    removePathForcibly,
 )
import System.FilePath ((</>))
import Test.Hspec

import Eval.Agent (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (..),
    defaultBudget,
    runEpisodeWith',
 )
import Eval.Episode (
    EpisodeMeta (..),
    naNote,
    parseEpisodeMeta,
    readNaFlags,
    readSaturatedFlags,
    readVoidFlags,
    renderEpisodeMeta,
    rerollSeed,
    retryFreshSeed,
    saturatedNote,
    saveEpisodeIn,
    transcriptBody,
    voidNote,
    voidPair,
 )
import Eval.GateMetrics (renderGateMetrics)
import Eval.GateResult (GateResult (..), SearchMode (..))
import Eval.Ollama (ToolCall (..), Turn (..))
import Eval.ReportGuard (guardReport)
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

spec :: Spec
spec = describe "Eval.Episode (R8.1-R8.3 measurement plumbing)" $ do
    describe "episode-config header" $ do
        it "round-trips through render/parse" $
            parseEpisodeMeta (renderEpisodeMeta sampleMeta)
                `shouldBe` Just sampleMeta
        it "round-trips empty levers and a multi-line final as one line" $ do
            let m =
                    sampleMeta
                        { emLevers = []
                        , emFinal = "chat error after retries: boom"
                        }
            parseEpisodeMeta (renderEpisodeMeta m) `shouldBe` Just m
        it "is absent from a headerless transcript" $
            parseEpisodeMeta "# Session: dateDays\n" `shouldBe` Nothing
        it "strips only the header from the body" $ do
            let body = "# Session: dateDays\n\nbody line\n"
            transcriptBody (renderEpisodeMeta sampleMeta <> body)
                `shouldBe` body

    describe "voidPair (R8.2 general invariant)" $ do
        it "equal bodies are void, unequal are not, whatever the headers say" $ do
            let bodies = ["", "# a\nx\n", "# a\ny\n", "# b\nlong transcript\n"]
                metas = [sampleMeta, sampleMeta{emArm = "on", emSeed = 2}]
            forM_ [(b1, b2, m1, m2) | b1 <- bodies, b2 <- bodies, m1 <- metas, m2 <- metas] $
                \(b1, b2, m1, m2) ->
                    voidPair
                        (renderEpisodeMeta m1 <> b1)
                        (renderEpisodeMeta m2 <> b2)
                        `shouldBe` (b1 == b2)

    describe "rerollSeed / retryFreshSeed (R6.11)" $ do
        it "attempt 0 is the base seed; retries get fresh distinct seeds" $ do
            rerollSeed 7 0 `shouldBe` 7
            length (nub [rerollSeed 7 n | n <- [0 .. 5]]) `shouldBe` 6
        it "re-invokes with a fresh seed and records every seed tried" $ do
            seen <- newIORef ([] :: [Int])
            let attempt s = do
                    modifyIORef' seen (<> [s])
                    n <- length <$> readIORef seen
                    pure (if n < 2 then ("fail" :: Text) else "good")
            (r, tried) <- retryFreshSeed 2 41 (== "good") attempt
            r `shouldBe` "good"
            used <- readIORef seen
            tried `shouldBe` used
            length used `shouldBe` 2
            take 1 used `shouldBe` [41]
            length (nub used) `shouldBe` 2
        it "stops at the retry cap and returns the failing run" $ do
            (r, tried) <- retryFreshSeed 2 5 (== ("good" :: Text)) (const (pure "fail"))
            r `shouldBe` "fail"
            length tried `shouldBe` 3

    describe "guardReport (couples R8.4 to the gate numbers)" $ do
        let okFile = ("a-s1-off.md", renderEpisodeMeta sampleMeta <> "body")
            report = "Overall: A 3/6 B 4/6"
        it "prints the report when every transcript is sound" $
            guardReport [okFile] report `shouldBe` report
        it "withholds the numbers when a transcript lacks arm config" $ do
            let out = guardReport [okFile, ("b.md", "# Session: b\n")] report
            out `shouldSatisfy` T.isInfixOf "WITHHELD"
            out `shouldSatisfy` (not . T.isInfixOf "3/6")
            out `shouldSatisfy` T.isInfixOf "missing episode-config"
        it "withholds the numbers when a transcript failed the lint" $ do
            let bad =
                    ( "c-s1-on.md"
                    , renderEpisodeMeta sampleMeta{emLint = "FAIL raw-exception"}
                    )
                out = guardReport [okFile, bad] report
            out `shouldSatisfy` T.isInfixOf "WITHHELD"
            out `shouldSatisfy` T.isInfixOf "raw-exception"

    describe "two-arm fixture run (R8.1/R8.2 end to end)" $
        it "arms differ, both carry full config, identical pairs get VOID" $ do
            tmp <- getTemporaryDirectory
            let dir = tmp </> "siza-eval-episode-spec"
            removePathForcibly dir
            createDirectoryIfMissing True dir
            offRun <- fixtureEpisode GrammarOff
            onRun <- fixtureEpisode GrammarOn
            mv1 <- saveEpisodeIn dir (fixtureMeta "off" offRun) (arTranscript offRun)
            mv2 <- saveEpisodeIn dir (fixtureMeta "on" onRun) (arTranscript onRun)
            (mv1, mv2) `shouldBe` (Nothing, Nothing)
            offTxt <- readFileT (dir </> "fixture-s1-off.md")
            onTxt <- readFileT (dir </> "fixture-s1-on.md")
            offTxt `shouldNotBe` onTxt
            voidPair offTxt onTxt `shouldBe` False
            forM_ [(offTxt, "off"), (onTxt, "on")] $ \(txt, arm) -> do
                let m = parseEpisodeMeta txt
                m `shouldSatisfy` isJust
                fmap emArm m `shouldBe` Just arm
                fmap emTask m `shouldBe` Just "fixture"
                fmap emSeed m `shouldBe` Just 1
                fmap emModel m `shouldBe` Just "test-model"
                fmap emStopped m `shouldBe` Just "done"
                fmap emLevers m `shouldBe` Just [("grammar", arm)]
            -- Byte-identical SEARCH-FREE arms (GrammarOff makes no discover
            -- call): not-applicable, never VOID (section 13 applicability).
            _ <-
                saveEpisodeIn
                    dir
                    (fixtureMeta "off" offRun){emTask = "nadup"}
                    (arTranscript offRun)
            mvNa <-
                saveEpisodeIn
                    dir
                    (fixtureMeta "on" offRun){emTask = "nadup"}
                    (arTranscript offRun)
            mvNa `shouldSatisfy` isJust
            nas <- readNaFlags dir
            nas `shouldBe` [("nadup", 1)]
            naNote nas `shouldSatisfy` T.isInfixOf "nadup s1"
            readVoidFlags dir >>= \vs -> ("nadup", 1) `notElem` vs `shouldBe` True
            -- Byte-identical SEARCH-USING arms whose discover surface ANSWERED
            -- (GrammarOn proactively discovers): lever-saturated, never VOID.
            _ <-
                saveEpisodeIn
                    dir
                    (fixtureMeta "off" onRun){emTask = "vdup"}
                    (arTranscript onRun)
            mvV <-
                saveEpisodeIn
                    dir
                    (fixtureMeta "on" onRun){emTask = "vdup"}
                    (arTranscript onRun)
            mvV `shouldSatisfy` isJust
            sats <- readSaturatedFlags dir
            ("vdup", 1) `elem` sats `shouldBe` True
            saturatedNote sats `shouldSatisfy` T.isInfixOf "vdup s1"
            readVoidFlags dir >>= \vs -> ("vdup", 1) `notElem` vs `shouldBe` True
            -- Byte-identical arms whose discover CALL got no answer: the lever
            -- surface never fired — dead, flagged VOID.
            let deadMsgs =
                    [ object
                        [ "role" .= ("assistant" :: Text)
                        , "tool_calls"
                            .= [ object
                                    [ "function"
                                        .= object
                                            [ "name" .= ("find_function" :: Text)
                                            , "arguments"
                                                .= object ["query" .= ("col" :: Text)]
                                            ]
                                    ]
                               ]
                        ]
                    ]
            _ <-
                saveEpisodeIn dir (fixtureMeta "off" offRun){emTask = "deadlever"} deadMsgs
            mvD <-
                saveEpisodeIn dir (fixtureMeta "on" offRun){emTask = "deadlever"} deadMsgs
            mvD `shouldSatisfy` isJust
            voids <- readVoidFlags dir
            ("deadlever", 1) `elem` voids `shouldBe` True
            voidNote voids `shouldSatisfy` T.isInfixOf "deadlever s1"
            -- The lever-effect tripwire compares BODIES: the identical "vdup"
            -- pair reads inert even though its arm headers differ.
            metrics <- renderGateMetrics dir (gateRows "vdup")
            metrics `shouldSatisfy` T.isInfixOf "0/1"
            metrics `shouldSatisfy` T.isInfixOf "LEVER INERT"
            metricsLive <- renderGateMetrics dir (gateRows "fixture")
            metricsLive `shouldSatisfy` T.isInfixOf "1/1"
  where
    readFileT p = T.pack <$> readFile p
    gateRows task =
        [ GateResult task 1 SearchOff True 2 1 "done" 0
        , GateResult task 1 SearchOn True 2 1 "done" 0
        ]

{- | The arm-lever fixture: a notebook whose cell 0 declares deps, so the
GrammarOn proactive discover produces messages GrammarOff never emits.
-}
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
                , drvVerify = pure (CheckPassed, Nothing)
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

-- | Does a list_cells argument object carry @full: true@ (the M1 fix)?
fullTrue :: Value -> Bool
fullTrue (Object o) = KM.lookup "full" o == Just (Bool True)
fullTrue _ = False
