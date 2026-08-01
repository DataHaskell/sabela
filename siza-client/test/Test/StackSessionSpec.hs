{-# LANGUAGE OverloadedStrings #-}

{- | The session state and pre-dispatch layers both entry points share.
Every layer here must work from a 'Dispatch' alone — no model, no transcript.
-}
module Test.StackSessionSpec (stackSessionSpec) where

import Data.Aeson (Value (..), object, (.=))
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover (GrammarMode (..))
import Siza.Agent.Futility (normaliseDiagnostic)
import Siza.Agent.Loop (episodeStack)
import Siza.Agent.Owned (OwnedCell (..))
import Siza.Agent.Stack (
    Dispatch,
    StackSession,
    newStackSession,
    ownedCells,
    ownedReds,
    recordCall,
    sessionGoal,
    sessionRejectionRepeats,
    stackDispatch,
    stackLayers,
 )
import Siza.Agent.Stack.Call (runToolCall)
import Siza.Mcp (mcpSession, mcpStackFor)
import Test.StackFixtures (Fake, hasKey, recordingNotebook)
import Test.TruthGen (genGhcDiagnostic, genSubstantiveSource)

stackSessionSpec :: Spec
stackSessionSpec = describe "the shared dispatch stack session" $ do
    it "names its layers, so a parity test cannot go vacuous" $
        stackLayers `shouldBe` ["normalize", "goal", "discover-ledger", "futility"]

    it "takes the goal off the call before it reaches the wire" $ do
        (fake, tape) <- recordingNotebook
        ss <- newStackSession GrammarOn False ""
        _ <-
            stackDispatch ss fake $
                ToolCall
                    "insert_cell"
                    (object ["source" .= ("x = 1" :: Text), "goal" .= chartGoal])
        seen <- readIORef tape
        map tcArgs seen `shouldSatisfy` not . any (hasKey "goal")
        sessionGoal ss `shouldReturn` chartGoal

    it "remembers the goal for a later call that carries none" $ do
        (fake, _) <- recordingNotebook
        ss <- newStackSession GrammarOn False ""
        _ <- stackDispatch ss fake (writeWithGoal "x = 1" chartGoal)
        _ <- stackDispatch ss fake (write "y = 2")
        sessionGoal ss `shouldReturn` chartGoal

    it "unwraps an enveloped argument object" $ do
        (fake, tape) <- recordingNotebook
        ss <- newStackSession GrammarOn False ""
        _ <-
            stackDispatch ss fake $
                ToolCall
                    "insert_cell"
                    (object ["input" .= object ["source" .= ("x = 1" :: Text)]])
        seen <- readIORef tape
        map tcArgs seen `shouldSatisfy` all (hasKey "source")

    it "keys futility past the goal, so a re-goaled retry is still a retry" $ do
        (fake, _) <- recordingNotebook
        ss <- newStackSession GrammarOn False ""
        _ <- stackDispatch ss fake (writeWithGoal "boom" "first try")
        second <- stackDispatch ss fake (writeWithGoal "boom" "second try")
        futilityNoted second `shouldBe` True

    it "records the cells it writes and forgets the ones it deletes" $ do
        (fake, _) <- recordingNotebook
        ss <- newStackSession GrammarOn False ""
        callThrough ss fake (write "x = 1")
        owned <- ownedCells ss
        Map.keys owned `shouldBe` [101]
        callThrough ss fake (ToolCall "delete_cell" (object ["cell_id" .= (101 :: Int)]))
        ownedCells ss >>= \m -> Map.keys m `shouldBe` []

    it "lists the reds it owns, and only those" $ do
        (fake, _) <- recordingNotebook
        ss <- newStackSession GrammarOn False ""
        callThrough ss fake (write "x = 1")
        callThrough ss fake (write "red = 2")
        reds <- ownedReds ss
        map fst reds `shouldBe` [102]

    it "does not own a cell a rejected write never created" $ do
        (fake, _) <- recordingNotebook
        ss <- newStackSession GrammarOn False ""
        callThrough ss fake (write "boom")
        ownedCells ss >>= \m -> Map.keys m `shouldBe` []

    rejectionLedgerSpec

{- | C1-17c: a rejection creates no cell, so every mechanism keyed on the owned
map is blind to it. What the session keeps instead is the diagnostic class.
-}
rejectionLedgerSpec :: Spec
rejectionLedgerSpec = describe "the rejection ledger a refused write leaves" $ do
    it "counts a run of N same-class rejections as N-1 repeats (C1-17c)" $
        property $
            forAll genRun $ \(srcs, body, positions) -> ioProperty $ do
                let steps = zip srcs [p <> body | p <- positions]
                repeats <- runOnBoth steps
                pure $
                    counterexample (show repeats) $
                        repeats === [repeatsOf steps, repeatsOf steps]
                            .&&. Map.elems (repeatsOf steps)
                            === [length steps - 1]

    it "counts distinct diagnostics as no repeat at all (C1-17c contrast)" $
        property $
            forAll genDistinctRun $ \steps -> ioProperty $ do
                repeats <- runOnBoth steps
                pure $
                    counterexample (show repeats) $
                        conjoin [Map.elems m === map (const 0) steps | m <- repeats]

    it "keys the run on the diagnostic, not on the source" $
        property $
            forAll genResentRun $ \(src, body, positions) -> ioProperty $ do
                let steps = [(src, p <> body) | p <- positions]
                repeats <- runOnBoth steps
                pure $
                    counterexample (show repeats) $
                        repeats === [repeatsOf steps, repeatsOf steps]
                            .&&. Map.elems (repeatsOf steps)
                            === [length steps - 1]

{- | The repeat count the metric defines, computed from the sequence itself:
one fewer than the number of calls each normalised diagnostic answered.
-}
repeatsOf :: [(Text, Text)] -> Map.Map Text Int
repeatsOf steps =
    Map.map (subtract 1) $
        Map.fromListWith (+) [(normaliseDiagnostic d, 1) | (_, d) <- steps]

{- | The same rejection sequence through the chat stack and the MCP stack, as
each surface's own entry point runs it.
-}
runOnBoth :: [(Text, Text)] -> IO [Map.Map Text Int]
runOnBoth steps = sequence [viaChat, viaMcp]
  where
    viaChat = do
        ss <- newStackSession GrammarOn False ""
        drive ss episodeStack
    viaMcp = do
        ss <- mcpSession
        drive ss mcpStackFor
    drive ss mkStack = do
        mapM_ (one ss mkStack) steps
        sessionRejectionRepeats ss
    one ss mkStack (src, diag) =
        runToolCall ss (mkStack ss (rejecting diag)) (write src)

{- | A notebook that refuses the write it is given with the diagnostic it was
built for, and answers everything else emptily.
-}
rejecting :: Text -> Dispatch
rejecting diag (ToolCall name _)
    | name `elem` ["insert_cell", "replace_cell_source"] =
        pure . Right . ToolErr $
            object
                [ "notCommitted" .= ("compile-gate" :: Text)
                , "verdict" .= ("diagnostic" :: Text)
                , "diagnostic" .= diag
                ]
rejecting _ (ToolCall "list_cells" _) =
    pure (Right (ToolOk (object ["cells" .= ([] :: [Value])])))
rejecting _ _ = pure (Right (ToolOk (object ["result" .= ("" :: Text)])))

{- | N distinct sources, one diagnostic body, and N independently drawn
interactive positions.
-}
genRun :: Gen ([Text], Text, [Text])
genRun = do
    n <- choose (2, 5)
    srcs <- distinctSources n
    body <- genGhcDiagnostic
    positions <- vectorOf n genPosition
    pure (srcs, body, positions)

-- | One source resent, with the positions varying independently.
genResentRun :: Gen (Text, Text, [Text])
genResentRun = do
    n <- choose (2, 5)
    src <- genSubstantiveSource
    body <- genGhcDiagnostic
    positions <- vectorOf n genPosition
    pure (src, body, positions)

-- | Rejections whose diagnostics are pairwise distinct after normalisation.
genDistinctRun :: Gen [(Text, Text)]
genDistinctRun = do
    n <- choose (2, 4)
    srcs <- distinctSources n
    bodies <- vectorOf n genGhcDiagnostic `suchThat` pairwiseDistinct
    positions <- vectorOf n genPosition
    pure (zip srcs (zipWith (<>) positions bodies))
  where
    pairwiseDistinct bs = length (foldr keep [] bs) == length bs
    keep x acc = if x `elem` acc then acc else x : acc

distinctSources :: Int -> Gen [Text]
distinctSources n = vectorOf n genSubstantiveSource `suchThat` allDifferent
  where
    allDifferent xs = length (foldr keep [] xs) == n
    keep x acc = if x `elem` acc then acc else x : acc

genPosition :: Gen Text
genPosition = do
    l <- choose (1, 99999 :: Int)
    c <- choose (1, 400 :: Int)
    pure ("<interactive>:" <> tshow l <> ":" <> tshow c <> ": error: ")

tshow :: (Show a) => a -> Text
tshow = T.pack . show

chartGoal :: Text
chartGoal = "show a chart"

write :: Text -> ToolCall
write src = ToolCall "insert_cell" (object ["source" .= src])

writeWithGoal :: Text -> Text -> ToolCall
writeWithGoal src goal =
    ToolCall "insert_cell" (object ["source" .= src, "goal" .= goal])

-- | Dispatch through the stack and record the result, as a caller would.
callThrough :: StackSession -> Fake -> ToolCall -> IO ()
callThrough ss fake call = do
    out <- stackDispatch ss fake call
    recordCall ss (call, out)

futilityNoted :: Either Text ToolOutcome -> Bool
futilityNoted (Right (ToolErr (Object o))) = hasKey "futility" (Object o)
futilityNoted (Left e) = "byte-identical" `T.isInfixOf` e
futilityNoted _ = False
