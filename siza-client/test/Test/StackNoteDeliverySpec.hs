{-# LANGUAGE OverloadedStrings #-}

{- | C2-10f: the shared stack computes notes and only the MCP server rendered
them, so anything emitted as a note was invisible to the chat loop. Note
EQUALITY is pinned in Test.StackParitySpec; this pins note DELIVERY, on the
channel each surface delivers it by.
-}
module Test.StackNoteDeliverySpec (stackNoteDeliverySpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..))
import Siza.Agent.Check (CheckResult (..))
import Siza.Agent.GrammarCards (GrammarMode (..))
import Siza.Agent.Loop (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    episodeStack,
    runEpisodeSeeded,
 )
import Siza.Agent.Owned (OwnedCell (..), hasArtifact)
import Siza.Agent.Stack (Dispatch, StackSession, newStackSession, ownedCells)
import Siza.Agent.Stack.Call (
    CallResult (..),
    StackNote (..),
    noteToolName,
    runToolCall,
 )
import Siza.Mcp (McpEnv, mcpEnvOver, mcpSession, mcpStackFor, toolsCall)
import Test.StackFixtures (recordingNotebook)
import Test.TruthGen (genIdent)

stackNoteDeliverySpec :: Spec
stackNoteDeliverySpec = describe "a note the shared stack computes reaches the model" $ do
    it "gives the chat loop a note channel of its own (C2-10f)" $
        property $
            forAll genNoteGrid $ \calls -> ioProperty $ do
                notes <- notesFor chatSession episodeStack calls
                bodies <- chatNoteMessages calls
                pure $
                    counterexample "the grid produced no note" (notes =/= [])
                        .&&. conjoin
                            [ counterexample (T.unpack (snText n) <> show bodies) $
                                any (snText n `T.isInfixOf`) bodies
                            | n <- notes
                            ]

    it "gives the MCP result a note block beside the outcome (C2-10f)" $
        property $
            forAll genNoteGrid $ \calls -> ioProperty $ do
                notes <- notesFor mcpSession mcpStackFor calls
                blocks <- mcpNoteBlocks calls
                pure $
                    counterexample "the grid produced no note" (notes =/= [])
                        .&&. conjoin
                            [ counterexample (T.unpack (snText n)) $
                                any (snText n `T.isInfixOf`) blocks
                            | n <- notes
                            ]

    describe "settled deferred state stays off the model-result channel" $ do
        it "reconciles a clean cell with one hidden snapshot call" $ do
            (result, tape, owned) <- deferredAwait False
            map tcName tape
                `shouldBe` ["insert_cell", "run_pending", "await_idle", "list_cells"]
            crHiddenCalls result `shouldBe` 1
            map (tcName . fst) (crStateSteps result) `shouldBe` ["list_cells"]
            hasArtifact owned `shouldBe` True

        it "reads a failed cell once and discloses only its compact diagnosis" $ do
            (result, tape, owned) <- deferredAwait True
            map tcName tape
                `shouldBe` ["insert_cell", "run_pending", "await_idle", "list_cells", "read_cell"]
            crHiddenCalls result `shouldBe` 2
            map (tcName . fst) (crStateSteps result)
                `shouldBe` ["list_cells", "read_cell"]
            ocDiagnostic <$> Map.lookup 7 owned
                `shouldBe` Just "Variable not in scope: total"
            map snText (crNotes result)
                `shouldSatisfy` any (T.isInfixOf "Variable not in scope: total")

        it "counts the hidden snapshot in the episode's actual dispatch total" $ do
            turnRef <- newIORef (0 :: Int)
            let calls =
                    [ ToolCall "insert_cell" (object ["source" .= ("answer = 42" :: Text)])
                    , ToolCall "run_pending" (object [])
                    , ToolCall "await_idle" (object [])
                    ]
                chat _ = do
                    n <- readIORef turnRef
                    writeIORef turnRef (n + 1)
                    pure . Right $
                        Turn
                            (object ["role" .= ("assistant" :: Text), "content" .= ("" :: Text)])
                            ""
                            (take 1 (drop n calls))
                driver =
                    Driver
                        { drvChat = chat
                        , drvDispatch = pure . deferredReply False
                        , drvNow = pure 0
                        , drvVerify = const (pure (CheckPassed, Nothing))
                        }
            run <-
                runEpisodeSeeded
                    []
                    (const (pure ()))
                    GrammarOn
                    EpisodeBudget{ebMaxRepairs = 4, ebDeadlineSecs = 600}
                    driver
                    "write a deferred cell"
                    4
            arToolCalls run `shouldBe` 4

chatSession :: IO StackSession
chatSession = newStackSession GrammarOn ""

deferredAwait :: Bool -> IO (CallResult, [ToolCall], Map.Map Int OwnedCell)
deferredAwait fails = do
    tapeRef <- newIORef []
    ss <- newStackSession GrammarOn ""
    let fake call = do
            modifyIORef' tapeRef (<> [call])
            pure (deferredReply fails call)
        run = runToolCall ss (episodeStack ss fake)
    _ <- run (ToolCall "insert_cell" (object ["source" .= ("answer = 42" :: Text)]))
    _ <- run (ToolCall "run_pending" (object []))
    result <- run (ToolCall "await_idle" (object []))
    (,,) result <$> readIORef tapeRef <*> ownedCells ss

deferredReply :: Bool -> ToolCall -> Either Text ToolOutcome
deferredReply _ (ToolCall "insert_cell" _) =
    Right . ToolOk $
        object
            [ "cellId" .= (7 :: Int)
            , "hash" .= ("h7" :: Text)
            , "status" .= ("completed" :: Text)
            , "execution" .= Null
            ]
deferredReply _ (ToolCall "run_pending" _) =
    Right (ToolOk (object ["pending" .= ([7] :: [Int])]))
deferredReply _ (ToolCall "await_idle" _) =
    Right (ToolOk (object ["waited" .= ("settled" :: Text)]))
deferredReply fails (ToolCall "list_cells" _) =
    Right . ToolOk $
        object
            [ "cells"
                .= [ object
                        [ "id" .= (7 :: Int)
                        , "hash" .= ("h7" :: Text)
                        , "type" .= ("CodeCell" :: Text)
                        , "dirty" .= False
                        , "hasError" .= fails
                        ]
                   ]
            ]
deferredReply fails (ToolCall "read_cell" _) =
    Right . ToolOk $
        object
            [ "id" .= (7 :: Int)
            , "hash" .= ("h7" :: Text)
            , "source" .= ("answer = 42" :: Text)
            , "error"
                .= if fails then Just ("Variable not in scope: total" :: Text) else Nothing
            ]
deferredReply _ _ = Left "unexpected call"

{- | A grid whose second write is blocked by a red cell, which is what makes
the shared stack re-aim it and produce a note the caller did not ask for.
-}
genNoteGrid :: Gen [ToolCall]
genNoteGrid = do
    n <- genIdent
    plain <- genIdent
    pure
        [ ToolCall "insert_cell" (object ["source" .= (plain <> " = 1")])
        , ToolCall "insert_cell" (object ["source" .= ("blocked" <> n <> " = 2")])
        ]

-- | The notes one surface's own session computes for a grid.
notesFor ::
    IO StackSession ->
    (StackSession -> Dispatch -> Dispatch) ->
    [ToolCall] ->
    IO [StackNote]
notesFor mkSession mkStack calls = do
    (fake, _) <- recordingNotebook
    ss <- mkSession
    concatMap crNotes <$> mapM (runToolCall ss (mkStack ss fake)) calls

{- | The note messages the chat loop emits for a grid, driven through the real
episode. The note channel, not the tool result: a note the payload happens to
repeat is not a note the loop delivered.
-}
chatNoteMessages :: [ToolCall] -> IO [Text]
chatNoteMessages calls = do
    (fake, _) <- recordingNotebook
    counter <- newIORef (0 :: Int)
    let chat _ = do
            i <- readIORef counter
            writeIORef counter (i + 1)
            pure . Right $
                Turn
                    (object ["role" .= ("assistant" :: Text), "content" .= ("" :: Text)])
                    ""
                    (take 1 (drop i calls))
        driver =
            Driver
                { drvChat = chat
                , drvDispatch = fake
                , drvNow = pure 0
                , drvVerify = const (pure (CheckUncheckable, Nothing))
                }
    run <-
        runEpisodeSeeded
            []
            (const (pure ()))
            GrammarOn
            EpisodeBudget{ebMaxRepairs = 4, ebDeadlineSecs = 600}
            driver
            "write the cells"
            (length calls)
    pure
        [ c
        | Object o <- arTranscript run
        , KM.lookup "tool_name" o == Just (String noteToolName)
        , Just (String c) <- [KM.lookup "content" o]
        ]

{- | The blocks an MCP tools/call returns beside the outcome. The first block
is the tool's own result, so a note must arrive after it.
-}
mcpNoteBlocks :: [ToolCall] -> IO [Text]
mcpNoteBlocks calls = do
    (fake, _) <- recordingNotebook
    ss <- mcpSession
    let env = mcpEnvOver ss fake [] :: McpEnv
    concat <$> mapM (fmap (drop 1 . allBlocks) . one env) calls
  where
    one env call =
        toolsCall env (object ["name" .= tcName call, "arguments" .= tcArgs call])

allBlocks :: Value -> [Text]
allBlocks (Object o) = case KM.lookup (K.fromText "content") o of
    Just (Array a) ->
        [ t
        | Object b <- foldr (:) [] a
        , Just (String t) <- [KM.lookup (K.fromText "text") b]
        ]
    _ -> []
allBlocks _ = []
