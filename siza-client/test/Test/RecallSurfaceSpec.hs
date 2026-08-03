{-# LANGUAGE OverloadedStrings #-}

{- | What the recall tool's own schema claims about where an index comes from,
and how long an index stays answerable. Both are claims about the harness, so
both are checked against what the harness mints rather than against prose.
-}
module Test.RecallSurfaceSpec (recallSurfaceSpec) where

import Control.Monad (forM)
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
import Siza.Agent.Chat (seedTranscript)
import Siza.Agent.EmitLedger (blockFloor, dedupInjected, newEmitLedger)
import Siza.Agent.Recall (
    answerRecall,
    readRecall,
    recallHintShape,
    recallResult,
    recallToolDef,
    resetRecallStore,
 )
import Test.TruthGen (genIdent, genPrefixed)

recallSurfaceSpec :: Spec
recallSurfaceSpec = describe "what the recall tool says about itself" $ do
    it "quotes the marker shape in every description it publishes" $
        [ d | d <- schemaDescriptions recallToolDef, not (recallHintShape `T.isInfixOf` d)
        ]
            `shouldBe` []

    it "names that same shape when the argument is missing" $
        recallResult (Map.fromList [(1, "x")]) (object [])
            `shouldSatisfy` either (T.isInfixOf recallHintShape) (const False)

    it "quotes a shape BOTH elision paths mint (C2-4a)" $
        property $
            forAll genBlock $ \block -> ioProperty $ do
                resetRecallStore
                stubs <- markersOf <$> compactedContents block
                refs <- markersOf <$> ledgerContents block
                pure $
                    counterexample "compaction minted no marker" (not (null stubs))
                        .&&. counterexample "the emit ledger minted no marker" (not (null refs))
                        .&&. conjoin
                            [ counterexample (T.unpack m) (recallHintShape `T.isInfixOf` shapeOf m)
                            | m <- stubs ++ refs
                            ]

    it "answers nothing an earlier episode elided" $
        property $
            forAll genBlock $ \block -> ioProperty $ do
                resetRecallStore
                idx <- indicesOf . markersOf <$> ledgerContents block
                before <- forM idx (answerRecall . indexArgs)
                resetRecallStore
                after <- forM idx (answerRecall . indexArgs)
                pure $
                    counterexample "the ledger minted no index" (not (null idx))
                        .&&. conjoin [toolOutcomeIsError o === False | o <- before]
                        .&&. conjoin
                            [ counterexample (show (toolOutcomeValue o)) $
                                toolOutcomeIsError o === True
                            | o <- after
                            ]

    it "leaves the store empty, and says so, after a reset" $ do
        _ <- ledgerContents "a block worth eliding, repeated twice over"
        resetRecallStore
        store <- readRecall
        Map.size store `shouldBe` 0
        recallResult store (indexArgs 1)
            `shouldBe` Left "No result #1: nothing has been elided yet."

-- | Every description the tool's JSON schema publishes to the model.
schemaDescriptions :: Value -> [Text]
schemaDescriptions = go
  where
    go (Object o) =
        [d | Just (String d) <- [KM.lookup (K.fromText "description") o]]
            ++ concatMap go (KM.elems o)
    go (Array a) = concatMap go a
    go _ = []

-- | The contents compaction produces for a transcript carrying one big result.
compactedContents :: Text -> IO [Text]
compactedContents block =
    map contentOf <$> seedTranscript [toolMsgV "discover" block]

{- | The contents the emit ledger injects for a block a later turn repeats,
which is what makes it back-reference rather than delta.
-}
ledgerContents :: Text -> IO [Text]
ledgerContents block = do
    ref <- newEmitLedger
    outs <- forM [1 :: Int, 2] $ \turn ->
        dedupInjected ref turn [toolMsgV "discover" block]
    pure (map contentOf (concat outs))

-- | The emitted contents that name a retrieval index.
markersOf :: [Text] -> [Text]
markersOf ts = [t | t <- ts, indexKey `T.isInfixOf` t]

indicesOf :: [Text] -> [Int]
indicesOf ts = [i | t <- ts, Just i <- [readIndexAfter t]]

{- | A marker with its own index replaced by @N@, so it can be compared with
the shape the schema quotes.
-}
shapeOf :: Text -> Text
shapeOf t = case T.breakOn indexKey t of
    (pre, rest)
        | T.null rest -> t
        | otherwise ->
            let body = T.drop (T.length indexKey) rest
                (sp, digits) = T.span (== ' ') body
             in pre <> indexKey <> sp <> "N" <> T.dropWhile isDigit digits

readIndexAfter :: Text -> Maybe Int
readIndexAfter t = case T.breakOn indexKey t of
    (_, rest)
        | T.null rest -> Nothing
        | otherwise ->
            case T.takeWhile isDigit (T.stripStart (T.drop (T.length indexKey) rest)) of
                ds | not (T.null ds) -> Just (read (T.unpack ds))
                _ -> Nothing

indexKey :: Text
indexKey = "{\"index\":"

contentOf :: Value -> Text
contentOf (Object o) = case KM.lookup (K.fromText "content") o of
    Just (String s) -> s
    _ -> ""
contentOf _ = ""

toolMsgV :: Text -> Text -> Value
toolMsgV name content =
    object ["role" .= ("tool" :: Text), "tool_name" .= name, "content" .= content]

indexArgs :: Int -> Value
indexArgs n = object ["index" .= n]

{- | A block past both elision floors, over arbitrary names, so both the
compaction stub and the ledger back-reference apply to it.
-}
genBlock :: Gen Text
genBlock = do
    k <- genPrefixed "k"
    body <- genIdent
    pure
        ( "{\""
            <> k
            <> "\":\""
            <> T.take (blockFloor * 3) (T.replicate (blockFloor * 3) (body <> " "))
            <> "\"}"
        )
