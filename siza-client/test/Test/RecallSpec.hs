{-# LANGUAGE OverloadedStrings #-}

{- | The retrieval a stub or a back-reference promises, driven through the
functions production drives: what the model is told to call must answer, and a
miss must read as a miss.
-}
module Test.RecallSpec (recallSpec) where

import Control.Monad (foldM, forM)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Types (toolOutcomeIsError, toolOutcomeValue)
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Chat (seedTranscript)
import Siza.Agent.Compact (isResultStub)
import Siza.Agent.EmitLedger (blockFloor, dedupInjected, newEmitLedger)
import Siza.Agent.Recall (
    answerRecall,
    recallOutcome,
    resultId,
    withRecallStore,
 )
import Siza.Agent.ToolRoute (Route (..), routeCallWith)
import Siza.Agent.Tools (dispatch, offeredArgKeys)
import Siza.Transport (newConn)
import Test.TruthGen (genIdent, genPrefixed)

recallSpec :: Spec
recallSpec = describe "reading back what the harness elided" $ do
    it "reports a miss as an error, not as a result (C2-4a)" $
        property $
            forAll ((,) <$> genStore <*> arbitrary) $ \(store, n) ->
                Map.notMember n store ==>
                    let out = recallOutcome store (indexArgs n)
                     in counterexample (show (toolOutcomeValue out)) $
                            toolOutcomeIsError out === True

    it "reports a hit as a result carrying the stored bytes" $
        property $
            forAll genStore $ \store ->
                conjoin
                    [ counterexample (show i) $
                        let out = recallOutcome store (indexArgs i)
                         in toolOutcomeIsError out === False
                                .&&. resultField (toolOutcomeValue out) === Just full
                    | (i, full) <- Map.toList store
                    ]

    it "answers through the dispatch a live tool call takes" $ do
        reset (Map.fromList [(bytesIndex, storedBytes)])
        conn <- newConn
        hit <- dispatch conn base (recallCall bytesIndex)
        miss <- dispatch conn base (recallCall (bytesIndex + 1))
        fmap toolOutcomeIsError hit `shouldBe` Right False
        fmap (resultField . toolOutcomeValue) hit `shouldBe` Right (Just storedBytes)
        fmap toolOutcomeIsError miss `shouldBe` Right True

    it "keeps a prompt's markers resolvable at a later prompt (C2-4b)" $
        property $
            forAll (resize 3 (listOf1 (resize 3 (listOf1 genPayload)))) $
                \episodes -> ioProperty $ do
                    reset Map.empty
                    seeds <- foldM prompt [] episodes
                    resolutions <-
                        forM (markerIndices seeds) $ \i ->
                            (,) i <$> answerRecall (indexArgs i)
                    let payloads = concat episodes
                    pure $
                        counterexample "no marker survived to be resolved" (not (null resolutions))
                            .&&. conjoin
                                [ counterexample (show i) $
                                    resultField (toolOutcomeValue out) `elem` map Just payloads
                                | (i, out) <- resolutions
                                ]

    it "answers every back-reference the emit ledger mints (C1-19b)" $
        property $
            forAll genRepeatedBlocks $ \blocks -> ioProperty $ do
                reset Map.empty
                outs <- runEmitLedger blocks
                let calls = concatMap retrievalCalls outs
                answers <- mapM (answerRecall . tcArgs) calls
                pure $
                    counterexample (show outs) (not (null calls))
                        .&&. conjoin
                            [ counterexample (T.unpack (tcName tc)) (dispatchable tc)
                            | tc <- calls
                            ]
                        .&&. conjoin
                            [ counterexample (show (toolOutcomeValue out)) $
                                toolOutcomeIsError out === False
                                    .&&. counterexample
                                        "resolved to bytes no block produced"
                                        (resultField (toolOutcomeValue out) `elem` map Just blocks)
                            | out <- answers
                            ]

    it "never hands a colliding block the other block's bytes (C2-4b)" $
        withMaxSuccess 10 $
            forAll genIdent $ \seed -> ioProperty $ do
                let (a, b) = collidingPair seed
                reset Map.empty
                outs <- runEmitLedger [a, a, b, b]
                answers <-
                    forM (concatMap retrievalCalls outs) $ \tc ->
                        (,) tc <$> answerRecall (tcArgs tc)
                pure $
                    counterexample (show (resultId a, resultId b)) $
                        (resultId a === resultId b)
                            .&&. counterexample "no marker for one of the two" (length answers >= 2)
                            .&&. conjoin
                                [ counterexample (show (tcArgs tc)) $
                                    resultField (toolOutcomeValue out) `elem` [Just a, Just b]
                                | (tc, out) <- answers
                                ]
                            .&&. counterexample
                                "both markers resolve to the same block"
                                (length (distinct [resultField (toolOutcomeValue o) | (_, o) <- answers]) === 2)

{- | A name the router places. What the harness tells the model to call must
be a call the router will take.
-}
dispatchable :: ToolCall -> Bool
dispatchable tc = case routeCallWith offeredArgKeys tc of
    RouteUnknown _ -> False
    RouteBadArgs _ -> False
    _ -> True

base :: Text
base = "http://127.0.0.1:1/api"

bytesIndex :: Int
bytesIndex = 7714

storedBytes :: Text
storedBytes = "the bytes an index names"

recallCall :: Int -> ToolCall
recallCall n = ToolCall "recall_result" (indexArgs n)

-- | Put the shared store in a known state, as a conversation's first prompt does.
reset :: Map.Map Int Text -> IO ()
reset s = withRecallStore (const ((), s))

-- | One prompt: the transcript so far plus this prompt's results, compacted
-- the way the chat loop compacts it.
prompt :: [Value] -> [Text] -> IO [Value]
prompt prev payloads =
    seedTranscript (prev ++ [toolMsgV "discover" p | p <- payloads])

markerIndices :: [Value] -> [Int]
markerIndices msgs =
    [ i
    | m <- msgs
    , Just c <- [contentOf m]
    , isResultStub c
    , Just i <- [markerIndex c]
    ]

-- | The emissions the ledger actually injects, through its IO entry point.
runEmitLedger :: [Text] -> IO [Text]
runEmitLedger blocks = do
    ref <- newEmitLedger
    outs <- forM (zip [1 ..] blocks) $ \(turn, b) ->
        dedupInjected ref turn [toolMsgV "discover" b]
    pure [c | msgs <- outs, m <- msgs, Just c <- [contentOf m]]

{- | Two distinct blocks whose content index collides, found by walking a
generated family until one repeats. The store must keep both.
-}
collidingPair :: Text -> (Text, Text)
collidingPair seed = go Map.empty [0 :: Int ..]
  where
    go _ [] = (block 0, block 1)
    go seen (i : is) = case Map.lookup (resultId (block i)) seen of
        Just held | held /= block i -> (held, block i)
        _ -> go (Map.insert (resultId (block i)) (block i) seen) is
    block i =
        "block "
            <> seed
            <> " "
            <> T.pack (show i)
            <> " "
            <> T.take blockFloor (T.replicate blockFloor (seed <> " "))

{- | The retrieval call a marker names: the word before its index argument,
with that index.
-}
retrievalCalls :: Text -> [ToolCall]
retrievalCalls t =
    [ ToolCall (lastWord before) (indexArgs idx)
    | (before, m) <- T.breakOnAll indexKey t
    , Just idx <- [readDigits (T.stripStart (T.drop (T.length indexKey) m))]
    ]
  where
    indexKey = "{\"index\":"
    lastWord = T.takeWhileEnd (/= ' ') . T.strip
    readDigits d = case T.takeWhile isDigit d of
        ds | not (T.null ds) -> Just (read (T.unpack ds) :: Int)
        _ -> Nothing

markerIndex :: Text -> Maybe Int
markerIndex t = case T.breakOn "\"index\":" t of
    (_, rest)
        | T.null rest -> Nothing
        | otherwise ->
            case T.takeWhile isDigit (T.stripStart (T.drop 8 rest)) of
                digits | not (T.null digits) -> Just (read (T.unpack digits))
                _ -> Nothing

distinct :: (Eq a) => [a] -> [a]
distinct = foldr (\x xs -> x : filter (/= x) xs) []

resultField :: Value -> Maybe Text
resultField (Object o) = case KM.lookup (K.fromText "result") o of
    Just (String s) -> Just s
    _ -> Nothing
resultField _ = Nothing

contentOf :: Value -> Maybe Text
contentOf (Object o) = case KM.lookup (K.fromText "content") o of
    Just (String s) -> Just s
    _ -> Nothing
contentOf _ = Nothing

toolMsgV :: Text -> Text -> Value
toolMsgV name content =
    object ["role" .= ("tool" :: Text), "tool_name" .= name, "content" .= content]

indexArgs :: Int -> Value
indexArgs n = object ["index" .= n]

genStore :: Gen (Map.Map Int Text)
genStore = do
    vals <- listOf1 genPayload
    pure (Map.fromList [(resultId v, v) | v <- vals])

-- | A result long enough to be worth eliding, over arbitrary names.
genPayload :: Gen Text
genPayload = do
    k <- genPrefixed "k"
    body <- genPrefixed "b"
    pure ("{\"" <> k <> "\":\"" <> T.take 300 (T.replicate 300 (body <> " ")) <> "\"}")

{- | Blocks a turn repeats, which is what makes the ledger elide rather than
delta.
-}
genRepeatedBlocks :: Gen [Text]
genRepeatedBlocks = do
    n <- genIdent
    body <- T.take (blockFloor * 2) . T.replicate (blockFloor * 2) <$> genIdent
    let block = "fact about " <> n <> ": " <> body
    k <- choose (2, 4)
    pure (replicate k block)
