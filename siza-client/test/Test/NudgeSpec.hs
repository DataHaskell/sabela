{-# LANGUAGE OverloadedStrings #-}

{- | The held-fact act nudge (R5.6/R5.7/R5.9): fires iff a call-ready fact
(name + complete signature) is held AND no write landed within the next
k=2 calls, always leaving at least 'nudgeFloor' turns of budget; the
scaffold echo carries ONLY ledger-held names ('assertedLive' is the
validator); it never fires on a fact-free ledger. Plus the literal-minded
scripted caller converging to a write inside the pinned call budget.
-}
module Test.NudgeSpec (nudgeSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..))
import Siza.Agent.Check (CheckResult (..))
import Siza.Agent.Discover.Advice (harvestFacts)
import Siza.Agent.Discover.History (
    callReadyFacts,
    emptyLedger,
    ledgerRecord,
 )
import Siza.Agent.Loop (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (..),
    runEpisodeSeeded,
 )
import Siza.Agent.Loop.Support (
    forceActMsgWith,
    nudgeFloor,
    nudgeK,
    updateNudge,
 )
import Siza.Agent.NoteLedger (assertedLive)
import Test.DiscoverFixtures (textField)

readCall :: ToolCall
readCall = ToolCall "list_bindings" (object [])

writeCall :: ToolCall
writeCall = ToolCall "insert_cell" (object ["source" .= ("x = 1" :: Text)])

nudgeMsg :: Value
nudgeMsg = forceActMsgWith ["fact"] "Remaining turn budget: 5."

-- | A found discover envelope holding one exact, typed hit.
foundEnvelope :: Text -> Text -> Value
foundEnvelope name ty =
    object
        [ "query" .= name
        , "state" .= ("found" :: Text)
        , "hits"
            .= [ object
                    [ "name" .= name
                    , "type" .= ty
                    , "module" .= ("Granite.Svg" :: Text)
                    , "package" .= ("granite" :: Text)
                    , "version" .= ("0.7.4.0" :: Text)
                    , "install" .= ("installed" :: Text)
                    , "matchKind" .= ("exact" :: Text)
                    , "origin" .= ("session" :: Text)
                    ]
               ]
        ]

barsSig :: Text
barsSig = "[(Text, Double)] -> Plot -> Text"

nudgeSpec :: Spec
nudgeSpec = describe "held-fact act nudge (R5.6/R5.7/R5.9)" $ do
    describe "trigger grid: fires iff call-ready fact + k reads + floor" $ do
        it "fires after k consecutive reads with a call-ready fact held" $ do
            ref <- newIORef 0
            out <- updateNudge (pure nudgeMsg) ref nudgeK True 9 (replicate nudgeK readCall)
            length out `shouldBe` 1
        it "accumulates across batches" $ do
            ref <- newIORef 0
            first <- updateNudge (pure nudgeMsg) ref nudgeK True 9 [readCall]
            first `shouldBe` []
            out <- updateNudge (pure nudgeMsg) ref nudgeK True 9 [readCall]
            length out `shouldBe` 1
        it "NEVER fires on a fact-free ledger, however long the spiral" $ do
            ref <- newIORef 0
            forM_ [1 .. 6 :: Int] $ \_ -> do
                out <- updateNudge (pure nudgeMsg) ref nudgeK False 9 (replicate 3 readCall)
                out `shouldBe` []
        it "a write in the batch resets the counter and suppresses it" $ do
            ref <- newIORef 0
            out <- updateNudge (pure nudgeMsg) ref nudgeK True 9 [readCall, writeCall]
            out `shouldBe` []
            readIORef ref `shouldReturn` 0
        it "holds its fire below the remaining-budget floor" $ do
            ref <- newIORef 0
            out <-
                updateNudge
                    (pure nudgeMsg)
                    ref
                    nudgeK
                    True
                    (nudgeFloor - 1)
                    (replicate nudgeK readCall)
            out `shouldBe` []
        it "resets after firing (no every-turn nag)" $ do
            ref <- newIORef 0
            _ <- updateNudge (pure nudgeMsg) ref nudgeK True 9 (replicate nudgeK readCall)
            out <- updateNudge (pure nudgeMsg) ref nudgeK True 9 [readCall]
            out `shouldBe` []

    describe "the ledger holds a call-ready fact after a typed exact hit" $ do
        it "harvestFacts yields a signature fact from an exact typed hit" $ do
            let facts = harvestFacts (foundEnvelope "bars" barsSig)
            facts `shouldSatisfy` any (T.isInfixOf ("`bars` :: " <> barsSig))
        it "callReadyFacts recognises the signature fact" $ do
            let (led, _) =
                    ledgerRecord "bars" (foundEnvelope "bars" barsSig) emptyLedger
            callReadyFacts led `shouldSatisfy` (not . null)
            callReadyFacts led `shouldSatisfy` all (T.isInfixOf " :: ")
        it "a signature-free ledger is not call-ready" $
            callReadyFacts emptyLedger `shouldBe` []

    describe "scaffold echo: only ledger-held names (assertedLive validator)" $ do
        it "every name the nudge asserts is ledger-held"
            $ forM_
                [ ["`bars` :: " <> barsSig <> " — found in Granite.Svg (granite)"]
                , ["granite (hidden): -- cabal: build-depends: granite"]
                , []
                ]
            $ \facts -> do
                let msg = forceActMsgWith facts "Remaining turn budget: 5."
                    content = textField "content" msg
                    heldNames =
                        [ n
                        | f <- facts
                        , n <- T.splitOn "`" f
                        , not (T.null n)
                        ]
                forM_ (assertedLive content) $ \n ->
                    (n, n `elem` heldNames) `shouldBe` (n, True)
        it "a fact-free nudge asserts nothing" $ do
            let content = textField "content" (forceActMsgWith [] "Remaining: 5.")
            assertedLive content `shouldBe` []

    describe "R5.9 scripted-caller convergence" $
        it "the literal-minded caller writes within the pinned budget" $ do
            written <- newIORef False
            queryN <- newIORef (0 :: Int)
            let dispatch tc = case tcName tc of
                    "insert_cell" -> do
                        writeIORef written True
                        pure . Right . ToolOk $
                            object
                                [ "cellId" .= (1 :: Int)
                                , "execution" .= object ["ok" .= True]
                                ]
                    "list_cells" ->
                        pure (Right (ToolOk (object ["cells" .= ([] :: [Value])])))
                    "discover" ->
                        pure (Right (ToolOk (foundEnvelope "bars" barsSig)))
                    _ -> pure (Right (ToolOk (object [])))
                chat msgs
                    | any (nudgeIn . textField "content") msgs = do
                        done <- readIORef written
                        if done
                            then pure (Right (Turn (assistant "done") "done" []))
                            else
                                pure . Right $
                                    Turn
                                        (assistant "acting")
                                        ""
                                        [ ToolCall
                                            "insert_cell"
                                            ( object
                                                [ "source"
                                                    .= ( "displaySvg (T.unpack (bars xs plot))" ::
                                                            Text
                                                       )
                                                ]
                                            )
                                        ]
                    | otherwise = do
                        n <- readIORef queryN
                        modifyIORef' queryN (+ 1)
                        -- The caller wraps its args ({input:{...}}): the seam
                        -- must normalise BEFORE the ledger guard, or no fact
                        -- is ever harvested and this test cannot converge.
                        pure . Right $
                            Turn
                                (assistant "searching")
                                ""
                                [ ToolCall
                                    "discover"
                                    ( object
                                        [ "input"
                                            .= object
                                                [ "query"
                                                    .= ("bars v" <> T.pack (show n))
                                                ]
                                        ]
                                    )
                                ]
                driver =
                    Driver
                        { drvChat = chat
                        , drvDispatch = dispatch
                        , drvNow = pure 0
                        , drvVerify = pure (CheckPassed, Nothing)
                        }
                budget = EpisodeBudget{ebMaxRepairs = 4, ebDeadlineSecs = 600}
            run <-
                runEpisodeSeeded
                    []
                    (const (pure ()))
                    GrammarOff
                    budget
                    driver
                    "plot a bar chart"
                    pinnedBudget
            arStopped run `shouldBe` "done"
            arToolCalls run `shouldSatisfy` (<= pinnedBudget)
            let nudges =
                    [ m
                    | m <- arTranscript run
                    , nudgeIn (textField "content" m)
                    ]
            length nudges `shouldSatisfy` (>= 1)
            forM_ (take 1 nudges) $ \m -> do
                textField "content" m `shouldSatisfy` T.isInfixOf "`bars`"
                remainingOf (textField "content" m)
                    `shouldSatisfy` (>= nudgeFloor)
  where
    assistant t = object ["role" .= ("assistant" :: Text), "content" .= (t :: Text)]
    nudgeIn c = "act now" `T.isInfixOf` c
    -- Parse "Remaining turn budget: N." out of the nudge content.
    remainingOf c = case T.breakOn marker c of
        (_, rest)
            | not (T.null rest) ->
                either (const 0) fst (TR.decimal (T.drop (T.length marker) rest))
        _ -> 0
    marker = "Remaining turn budget: " :: Text

-- | The pinned call budget the scripted caller must converge within (R5.9).
pinnedBudget :: Int
pinnedBudget = 10
