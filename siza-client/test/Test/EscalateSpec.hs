{-# LANGUAGE OverloadedStrings #-}

{- | R8-T3: escalation in kind (search-api.md 8.1/8.3) — a refusing scripted
caller through the REAL 'runEpisodeSeeded' sees facts, then the typed-hole
candidate, then the candidate-carrying close; R5.7 sweep and R9.8 grid too.
-}
module Test.EscalateSpec (escalateSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..))
import Siza.Agent.Check (CheckResult (..))
import Siza.Agent.EmitLedger (eligibleBlocks)
import Siza.Agent.Loop (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (..),
    runEpisodeSeeded,
 )
import Siza.Agent.Loop.Support (nudgeFloor, nudgeK)
import Siza.Agent.Loop.WrapUp (escalationK, missRungFloor)
import Test.DiscoverFixtures (textField)

barsSig :: Text
barsSig = "[(Text, Double)] -> Plot -> Text"

-- | The found discover envelope the refusing caller keeps re-earning.
foundEnvelope :: Value
foundEnvelope =
    object
        [ "query" .= ("bars" :: Text)
        , "state" .= ("found" :: Text)
        , "hits"
            .= [ object
                    [ "name" .= ("bars" :: Text)
                    , "type" .= barsSig
                    , "module" .= ("Granite.Svg" :: Text)
                    , "package" .= ("granite" :: Text)
                    , "version" .= ("0.7.4.0" :: Text)
                    , "install" .= ("hidden" :: Text)
                    , "matchKind" .= ("exact" :: Text)
                    , "origin" .= ("session" :: Text)
                    , "cabal" .= ("-- cabal: build-depends: granite" :: Text)
                    ]
               ]
        ]

-- | The typed-hole candidate marker (nothing else emits a hole binder).
candidateMark :: Text
candidateMark = "(_ :: "

-- | The harness-injected user-role messages (where nudges live).
userMsgs :: AgentRun -> [Value]
userMsgs run =
    [m | m <- arTranscript run, textField "role" m == "user"]

isNudge1 :: Text -> Bool
isNudge1 c = "act now" `T.isInfixOf` c

isNudge2 :: Text -> Bool
isNudge2 c =
    candidateMark `T.isInfixOf` c
        && "insert_cell" `T.isInfixOf` c
        && not (isNudge1 c)

isNudge :: Text -> Bool
isNudge c = isNudge1 c || isNudge2 c

searchMorePhrases :: [Text]
searchMorePhrases =
    ["try another query", "rephrase", "retry discover", "search again"]

-- | Run the refusing caller: every turn is one more discover of "bars".
runRefusing :: Int -> IO AgentRun
runRefusing =
    runEpisodeSeeded
        []
        (const (pure ()))
        GrammarOff
        (EpisodeBudget{ebMaxRepairs = 50, ebDeadlineSecs = 600})
        driver
        "add a bar chart cell"
  where
    driver =
        Driver
            { drvChat = chat
            , drvDispatch = dispatch
            , drvNow = pure 0
            , drvVerify = pure (CheckFailed, Nothing)
            }
    dispatch tc = case tcName tc of
        "discover" -> pure (Right (ToolOk foundEnvelope))
        "list_cells" ->
            pure (Right (ToolOk (object ["cells" .= ([] :: [Value])])))
        _ -> pure (Right (ToolOk (object [])))
    chat _ =
        pure . Right $
            Turn
                (object ["role" .= ("assistant" :: Text), "content" .= ("searching" :: Text)])
                ""
                [ToolCall "discover" (object ["query" .= ("bars" :: Text)])]

{- | The model's own draft: a red insert on turn 0, then discover-only turns —
the topMonth turn-13 shape (it HAD a correct draft, then spun on discover).
-}
draftSource :: Text
draftSource = "import Granite.Svg\nbarChart myData (barPlot 400 300)"

{- | Run the drafting caller: one red draft insert, then every later turn is a
bare discover — so the act nudge fires with a held draft to seed from (R3.8).
-}
runDrafting :: Int -> IO AgentRun
runDrafting maxT = do
    turnRef <- newIORef (0 :: Int)
    runEpisodeSeeded
        []
        (const (pure ()))
        GrammarOff
        (EpisodeBudget{ebMaxRepairs = 50, ebDeadlineSecs = 600})
        (driver turnRef)
        "add a bar chart cell"
        maxT
  where
    driver turnRef =
        Driver
            { drvChat = chat turnRef
            , drvDispatch = dispatch
            , drvNow = pure 0
            , drvVerify = pure (CheckFailed, Nothing)
            }
    dispatch tc = case tcName tc of
        "discover" -> pure (Right (ToolOk foundEnvelope))
        "insert_cell" ->
            pure . Right . ToolOk $
                object
                    [ "cellId" .= (5 :: Int)
                    , "execution" .= object ["ok" .= False]
                    ]
        "list_cells" ->
            pure (Right (ToolOk (object ["cells" .= ([] :: [Value])])))
        _ -> pure (Right (ToolOk (object [])))
    chat turnRef _ = do
        n <- readIORef turnRef
        modifyIORef' turnRef (+ 1)
        pure . Right $
            if n == 0
                then
                    Turn
                        (object ["role" .= ("assistant" :: Text), "content" .= ("drafting" :: Text)])
                        ""
                        [ToolCall "insert_cell" (object ["source" .= draftSource])]
                else
                    Turn
                        (object ["role" .= ("assistant" :: Text), "content" .= ("searching" :: Text)])
                        ""
                        [ToolCall "discover" (object ["query" .= ("bars" :: Text)])]

escalateSpec :: Spec
escalateSpec = describe "escalation in kind (R8-T3)" $ do
    describe "draft-seeded escalation fires live through runEpisodeSeeded (R3.8)" $ do
        -- A rung-2 write nudge that is NOT the fact echo: it re-hands a cell to
        -- insert. With a held draft this must be the draft, never a synthesis.
        let isWriteNudge c =
                "insert_cell" `T.isInfixOf` c
                    && "verbatim" `T.isInfixOf` c
                    && not (isNudge1 c)
        it "the rung-2 nudge carries the model's own draft, not a synthesis" $ do
            run <- runDrafting 14
            let n2 =
                    [ c
                    | m <- userMsgs run
                    , let c = textField "content" m
                    , isWriteNudge c
                    ]
            n2 `shouldSatisfy` (not . null)
            head n2 `shouldSatisfy` T.isInfixOf "barChart myData (barPlot 400 300)"
        it "the drafted nudge never falls back to the ledger record stub" $ do
            run <- runDrafting 14
            let nudges =
                    [ textField "content" m
                    | m <- userMsgs run
                    , isWriteNudge (textField "content" m)
                    ]
            forM_ nudges $ \c ->
                c `shouldSatisfy` (not . T.isInfixOf "bars [(\"\", 0.0)]")
    describe "the refusing caller's ladder (R5.9 convergence shape)" $ do
        it "nudge 1 carries ranked facts; nudge 2 carries the candidate" $ do
            run <- runRefusing 12
            let contents = map (textField "content") (userMsgs run)
                n1 = [i | (i, c) <- zip [0 :: Int ..] contents, isNudge1 c]
                n2 = [i | (i, c) <- zip [0 :: Int ..] contents, isNudge2 c]
            n1 `shouldSatisfy` (not . null)
            n2 `shouldSatisfy` (not . null)
            head n1 `shouldSatisfy` (< head n2)
            -- nudge 1 already argues from the held facts
            contents !! head n1 `shouldSatisfy` T.isInfixOf "`bars`"
            -- nudge 2's candidate fills the tuple-list literal and holes the
            -- genuine Plot gap (R3.10 literal-fill)
            contents !! head n2
                `shouldSatisfy` T.isInfixOf "bars [(\"\", 0.0)] (_ :: Plot)"
            contents !! head n2 `shouldSatisfy` T.isInfixOf "import Granite.Svg"
        it "the closed cluster answers with the candidate in the envelope" $ do
            run <- runRefusing 12
            let closes =
                    [ c
                    | m <- arTranscript run
                    , let c = textField "content" m
                    , "discovery closed" `T.isInfixOf` c
                    ]
            closes `shouldSatisfy` (not . null)
            closes `shouldSatisfy` any (T.isInfixOf candidateMark)
        it "no byte-identical nudge is ever emitted twice" $ do
            run <- runRefusing 12
            let nudges =
                    [ c
                    | m <- userMsgs run
                    , let c = textField "content" m
                    , isNudge c
                    ]
            length nudges `shouldSatisfy` (>= 2)
            nub nudges `shouldBe` nudges
        -- R3.8/R5.5 general invariant: the repeated candidate STUB (the block
        -- the live nudge re-emitted ~6x) transmits verbatim at most once — the
        -- emit ledger back-references every later copy. The stub must ride on
        -- its own block for this to hold, which is exactly the wiring fix.
        it "the candidate stub is injected verbatim at most once" $ do
            run <- runRefusing 12
            let stub = "bars [(\"\", 0.0)] (_ :: Plot)"
                verbatim =
                    length
                        [ ()
                        | m <- userMsgs run
                        , stub `T.isInfixOf` textField "content" m
                        ]
                backRefs =
                    [ ()
                    | m <- userMsgs run
                    , "as established turn" `T.isInfixOf` textField "content" m
                    ]
            verbatim `shouldSatisfy` (<= 1)
            backRefs `shouldSatisfy` (not . null)
        -- The dedupable blocks of every injected nudge are byte-distinct.
        it "no dedupable block is injected byte-identically twice" $ do
            run <- runRefusing 12
            let blocks =
                    concat
                        [ eligibleBlocks (textField "content" m)
                        | m <- userMsgs run
                        ]
            blocks `shouldSatisfy` (not . null)
            nub blocks `shouldBe` blocks
        it "no channel emits search-more phrasing after the first nudge (R5.7)" $ do
            run <- runRefusing 12
            let contents = map (textField "content") (arTranscript run)
                after =
                    drop
                        (head [i | (i, c) <- zip [0 ..] contents, isNudge1 c])
                        contents
            forM_ after $ \c ->
                forM_ searchMorePhrases $ \p ->
                    T.toLower c `shouldSatisfy` (not . T.isInfixOf p)

    describe "budget proportionality: the gate needs pressure (R9.8)" $ do
        it "no rung-3 hard gate while the budget is above the floor" $
            forM_ [(20, r) | r <- [nudgeFloor + 1 .. 20]] $ \(total, r) ->
                missRungFloor total r `shouldSatisfy` (< 3)
        it "the gate binds only at or below the floor" $
            forM_ [0 .. nudgeFloor] $ \r ->
                missRungFloor 20 r `shouldBe` 3
        it "escalationK relaxes to nudgeK while over half the budget remains" $ do
            forM_ [11 .. 20] $ \r -> escalationK 20 r `shouldBe` nudgeK
            forM_ [0 .. 10] $ \r -> escalationK 20 r `shouldBe` 1
