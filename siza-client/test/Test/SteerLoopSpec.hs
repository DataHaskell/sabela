{-# LANGUAGE OverloadedStrings #-}

{- | R6-T3 steering through the REAL loop: the budget-pressure floor is wired
(a mid-budget fresh miss hears held facts), and the literal-minded caller
converges miss -> steer -> construct -> write within the R5.9 pin.
-}
module Test.SteerLoopSpec (steerLoopSpec) where

import Control.Monad (unless)
import Data.Aeson (object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..))
import Siza.Agent.Check (CheckResult (..))
import Siza.Agent.DiscoverTool (runDiscoverCall)
import Siza.Agent.Loop (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (..),
    runEpisodeSeeded,
 )
import Siza.Agent.Loop.WrapUp (wrapUpMarker)
import Test.CatalogueSim (simWorldCall)
import Test.DiscoverFixtures (argText, installNamesFileWith, textField)
import Test.SteerSpec (foundHidden, missEnvOf, steerTarget, world)

steerLoopSpec :: Spec
steerLoopSpec = describe "construct steering in the real loop (R6-T3)" $ do
    pressureWiringSpec
    convergenceSpec

-- The loop wires the miss-ladder budget pressure (R5.6) ----------------------

pressureWiringSpec :: Spec
pressureWiringSpec = describe "the miss ladder's budget-pressure wiring" $
    it "the loop wires the floor: a mid-budget fresh miss hears the facts" $ do
        calls <- newIORef (0 :: Int)
        let chat _ = do
                n <- readIORef calls
                writeIORef calls (n + 1)
                let q =
                        if n == 0
                            then "cumulus"
                            else "moonbeam" <> T.pack (show n)
                pure . Right $
                    Turn
                        (object ["role" .= ("assistant" :: Text), "content" .= ("" :: Text)])
                        ""
                        [ToolCall "discover" (object ["query" .= q])]
            dispatch tc = case tcName tc of
                "discover"
                    | argText "query" (tcArgs tc) == "cumulus" ->
                        pure (Right (ToolOk foundHidden))
                    | otherwise ->
                        pure
                            ( Right
                                (ToolOk (missEnvOf (argText "query" (tcArgs tc))))
                            )
                _ -> pure (Right (ToolOk (object [])))
            driver =
                Driver
                    { drvChat = chat
                    , drvDispatch = dispatch
                    , drvNow = pure 0
                    , drvVerify = pure (CheckUncheckable, Nothing)
                    }
        run <-
            runEpisodeSeeded
                []
                (const (pure ()))
                GrammarOff
                EpisodeBudget{ebMaxRepairs = 4, ebDeadlineSecs = 600}
                driver
                "find the plotting api"
                8
        arStopped run `shouldBe` "max_turns"
        let discoverOuts =
                [ textField "content" m
                | m <- arTranscript run
                , textField "role" m == "tool"
                , textField "tool_name" m == "discover"
                ]
        length discoverOuts `shouldSatisfy` (>= 6)
        -- Early miss (fresh cluster, above mid-budget): no facts yet.
        (discoverOuts !! 1)
            `shouldSatisfy` (not . T.isInfixOf "build-depends: cumulus")
        -- Mid-budget miss (turn 4 of 8): FIRST miss of a fresh cluster
        -- already carries the held cabal fact.
        (discoverOuts !! 4) `shouldSatisfy` T.isInfixOf "cumulus"
        (discoverOuts !! 4) `shouldSatisfy` T.isInfixOf "Already held"

-- The literal-minded scripted caller (R5.9 pin) ------------------------------

convergenceSpec :: Spec
convergenceSpec = describe "R5.9: guidance converges within the pinned budget" $
    it "miss -> steer -> construct -> write, within 4 discover calls" $ do
        installNamesFileWith ["plume", "framing", "styling"]
        written <- newIORef False
        preWrite <- newIORef (0 :: Int)
        huntN <- newIORef (0 :: Int)
        let huntQueries = ["newPlot", "`newPlot`"]
            assistant t =
                object ["role" .= ("assistant" :: Text), "content" .= (t :: Text)]
            deliverable =
                Turn
                    (assistant "")
                    ""
                    [ ToolCall
                        "insert_cell"
                        ( object
                            [ "source"
                                .= ( "chart = bars [(\"a\", 1.0)] defaultPlot" ::
                                        Text
                                   )
                            ]
                        )
                    ]
            lastTool msgs = case [ textField "content" m
                                 | m <- msgs
                                 , textField "role" m == "tool"
                                 ] of
                [] -> ""
                cs -> last cs
            actOrdered msgs =
                any
                    ( \m ->
                        let c = textField "content" m
                         in "act now" `T.isInfixOf` c
                                || wrapUpMarker `T.isInfixOf` c
                    )
                    (drop 2 msgs)
            chat msgs = do
                w <- readIORef written
                let lt = lastTool msgs
                if w
                    then
                        pure . Right $
                            Turn (assistant "bar chart drawn") "bar chart drawn" []
                    else
                        if actOrdered msgs
                            then pure (Right deliverable)
                            else case steerTarget lt of
                                Just ty ->
                                    pure . Right $
                                        Turn
                                            (assistant "")
                                            ""
                                            [ ToolCall
                                                "discover"
                                                ( object
                                                    [ "query" .= ty
                                                    , "mode"
                                                        .= ("construct" :: Text)
                                                    ]
                                                )
                                            ]
                                Nothing
                                    | "defaultPlot" `T.isInfixOf` lt ->
                                        pure (Right deliverable)
                                    | otherwise -> do
                                        n <- readIORef huntN
                                        modifyIORef' huntN (+ 1)
                                        let q =
                                                huntQueries
                                                    !! min
                                                        n
                                                        (length huntQueries - 1)
                                        pure . Right $
                                            Turn
                                                (assistant "")
                                                ""
                                                [ ToolCall
                                                    "discover"
                                                    (object ["query" .= q])
                                                ]
            dispatch tc = case tcName tc of
                "discover" -> do
                    w <- readIORef written
                    unless w (modifyIORef' preWrite (+ 1))
                    out <-
                        runDiscoverCall
                            True
                            (simWorldCall world)
                            (argText "query" (tcArgs tc))
                            (tcArgs tc)
                    pure (Right out)
                "insert_cell" -> do
                    writeIORef written True
                    pure . Right . ToolOk $
                        object
                            [ "cellId" .= (1 :: Int)
                            , "execution" .= object ["ok" .= True]
                            ]
                "list_cells" -> simWorldCall world ListCells (tcArgs tc)
                _ -> pure (Right (ToolOk (object [])))
            driver =
                Driver
                    { drvChat = chat
                    , drvDispatch = dispatch
                    , drvNow = pure 0
                    , drvVerify = pure (CheckPassed, Nothing)
                    }
        run <-
            runEpisodeSeeded
                []
                (const (pure ()))
                GrammarOff
                EpisodeBudget{ebMaxRepairs = 4, ebDeadlineSecs = 600}
                driver
                "draw a bar chart of revenue by month"
                12
        arStopped run `shouldBe` "done"
        readIORef written `shouldReturn` True
        -- The R5.9 pin: at most 4 discover-class calls before the first write.
        readIORef preWrite `shouldReturn` 3
        -- The steer engaged and its named call succeeded.
        let bodies = map (textField "content") (arTranscript run)
        bodies `shouldSatisfy` any (T.isInfixOf "For a value of type Plot")
        bodies `shouldSatisfy` any (T.isInfixOf "defaultPlot")
