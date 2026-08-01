{-# LANGUAGE OverloadedStrings #-}

module Test.CompactSpec (compactSpec) where

import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isDigit)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Verdict (verdictClasses, verdictMarker)
import Siza.Agent.Compact (
    compactSeed,
    compactWith,
    isResultStub,
    resultStubFor,
 )
import Siza.Agent.EmitLedger (dedupText, emptyEmitLedger)
import Siza.Agent.Recall (recallResult)
import Test.TruthGen (genGhcDiagnostic, genIdent, genPrefixed, genSource)

big :: Text
big = T.replicate 60 "discover answer line\n"

assistantWith :: Text -> Value
assistantWith t =
    object
        ["role" .= ("assistant" :: Text), "content" .= ("ok" :: Text), "thinking" .= t]

toolMsgV :: Text -> Text -> Value
toolMsgV name content =
    object ["role" .= ("tool" :: Text), "tool_name" .= name, "content" .= content]

userMsgV :: Text -> Value
userMsgV t = object ["role" .= ("user" :: Text), "content" .= t]
field :: Text -> Value -> Maybe Text
field k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> Just s
    _ -> Nothing
field _ _ = Nothing

compactSpec :: Spec
compactSpec = describe "seed compaction" $ do
    it "keeps the user prompt verbatim, so `it` still refers" $ do
        let (seed, _) = compactSeed [userMsgV "plot a sine wave", userMsgV "animate it"]
        map (field "content") seed
            `shouldBe` [Just "plot a sine wave", Just "animate it"]

    it "drops the model's own thinking" $ do
        let (seed, _) = compactSeed [assistantWith "long private reasoning"]
        field "thinking" (head seed) `shouldBe` Nothing

    it "keeps the assistant's prose, which the next prompt may refer to" $ do
        let (seed, _) = compactSeed [assistantWith "reasoning"]
        field "content" (head seed) `shouldBe` Just "ok"

    it "elides a large tool result to a reference" $ do
        let (seed, _) = compactSeed [toolMsgV "discover" big]
        field "content" (head seed)
            `shouldSatisfy` maybe False ("[result #" `T.isPrefixOf`)

    it "names the tool and the index in the stub" $ do
        let (seed, _) = compactSeed [toolMsgV "discover" big]
            c = fromMaybe "" (field "content" (head seed))
        c `shouldSatisfy` T.isInfixOf "from discover"
        c `shouldSatisfy` T.isInfixOf "recall_result"

    it "keeps a short result verbatim: a stub would cost more than it saves" $ do
        let (seed, _) = compactSeed [toolMsgV "check_type" "x :: Int"]
        field "content" (head seed) `shouldBe` Just "x :: Int"

    it "recalls an elided result in full, byte for byte" $ do
        let (seed, store) = compactSeed [toolMsgV "discover" big]
        fmap (recallResult store . indexArgs) (markerOf seed 0)
            `shouldBe` Just (Right big)

    it "numbers several elided results independently" $ do
        let (seed, store) =
                compactSeed [toolMsgV "discover" big, toolMsgV "try" (big <> "!")]
        markerOf seed 0 `shouldNotBe` markerOf seed 1
        fmap (recallResult store . indexArgs) (markerOf seed 1)
            `shouldBe` Just (Right (big <> "!"))

    it "an unknown index says so, and lists what there is" $
        recallResult (Map.fromList [(1, "a")]) (object ["index" .= (9 :: Int)])
            `shouldSatisfy` either (T.isInfixOf "No result #9") (const False)

    it "a stringly-typed index still resolves" $
        recallResult (Map.fromList [(3, "found")]) (object ["index" .= ("3" :: Text)])
            `shouldBe` Right "found"

    it "a missing index asks for one rather than answering emptily" $
        recallResult (Map.fromList [(1, "a")]) (object [])
            `shouldSatisfy` either (T.isInfixOf "needs an integer") (const False)

    it "never elides a result carrying an actionable diagnostic (C2-4d)" $
        property $
            forAll genRejection $ \payload ->
                let (seed, _) = compactSeed [toolMsgV "insert_cell" payload]
                 in field "content" (head seed) === Just payload

    it "names exactly the top-level keys of the result it elides (C2-4c)" $
        property $
            forAll ((,) <$> genPayload <*> listOf1 (genPrefixed "z")) $
                \((ks, payload), absent) ->
                    let stub = resultStubFor 1 "discover" payload
                        named k = counterexample (T.unpack k) (k `T.isInfixOf` stub)
                        unnamed k =
                            counterexample (T.unpack k) (not (k `T.isInfixOf` stub))
                     in conjoin (map named ks)
                            .&&. conjoin
                                [unnamed k | k <- absent, not (k `T.isInfixOf` payload)]

    it "tells two different results apart (C2-4c)" $
        property $
            forAll ((,) <$> genPrefixed "k" <*> arbitrary) $ \(k, m) ->
                let mk :: Int -> Text
                    mk v =
                        encodeText
                            ( object
                                [ "blob" .= T.replicate 300 "x"
                                , K.fromText k .= (T.replicate 300 "y" <> tshow v)
                                ]
                            )
                    stubOf v = field "content" (head (fst (compactSeed [msg v])))
                    msg v = toolMsgV "discover" (mk v)
                 in stubOf m =/= stubOf (m + 1)

    it "never resolves a marker to another result's bytes (C2-4b)" $
        property $
            forAll ((,) <$> genPayload <*> genPayload) $ \((_, r1), (_, r2)) ->
                r1 /= r2 ==>
                    let (seed1, _) = compactSeed [toolMsgV "discover" r1]
                        (_, store2) =
                            compactSeed (seed1 ++ [toolMsgV "check_type" r2])
                     in case markerIndex =<< field "content" (head seed1) of
                            Nothing -> counterexample "no marker index" (property False)
                            Just i -> case Map.lookup i store2 of
                                Nothing -> property True
                                Just bytes -> bytes === r1

    it "keeps every marker resolvable across successive prompts (C2-4b)" $
        property $
            forAll (resize 3 (listOf1 (resize 3 (listOf1 genPayload)))) $ \episodes ->
                let step (msgs, store) ep =
                        compactWith store (msgs ++ [toolMsgV "discover" p | (_, p) <- ep])
                    (final, storeN) = foldl step ([], Map.empty) episodes
                    payloads = [p | ep <- episodes, (_, p) <- ep]
                    resolved =
                        [ counterexample (show i) $
                            recallResult storeN (indexArgs i) `elem` map Right payloads
                        | m <- final
                        , Just c <- [field "content" m]
                        , isResultStub c
                        , Just i <- [markerIndex c]
                        ]
                 in counterexample "no marker was minted" (not (null resolved))
                        .&&. conjoin resolved

    it "keeps whatever the emit ledger keeps, class for class (C1-19c)" $
        property $
            forAll genProtected $ \payload ->
                let (seed, _) = compactSeed [toolMsgV "insert_cell" payload]
                    (_, led) = dedupText 1 payload emptyEmitLedger
                    (repeated, _) = dedupText 2 payload led
                 in counterexample "the ledger back-referenced it" (repeated === payload)
                        .&&. counterexample
                            "compaction elided it"
                            (field "content" (head seed) === Just payload)

{- | The classes neither elision path may touch: a live diagnostic under a
value of any shape, a verdict, and a cell's own failing output.
-}
genProtected :: Gen Text
genProtected =
    oneof [genRejection, genStructuredError, genVerdictEcho, genFailingOutput]

genStructuredError :: Gen Text
genStructuredError = do
    code <- genPrefixed "E"
    diag <- genGhcDiagnostic
    pure . encodeText $
        object
            [ "error" .= object ["code" .= code, "message" .= diag]
            , "padding" .= T.replicate 300 "x"
            ]

genVerdictEcho :: Gen Text
genVerdictEcho = do
    cls <- elements verdictClasses
    body <- genGhcDiagnostic
    pure . encodeText $
        object
            [ "content" .= (verdictMarker cls <> " " <> body)
            , "padding" .= T.replicate 300 "x"
            ]

genFailingOutput :: Gen Text
genFailingOutput = do
    n <- genIdent
    pure . encodeText $
        object
            [ "outputs"
                .= [ object
                        [ "oiMime" .= ("text/plain" :: Text)
                        , "oiOutput" .= ("Not in scope: " <> n)
                        ]
                   ]
            , "padding" .= T.replicate 300 "x"
            ]

{- | A tool result carrying a live diagnostic: the class no elision path may
touch, at a length that straddles the stub floor.
-}
genRejection :: Gen Text
genRejection = do
    diag <- genGhcDiagnostic
    src <- genSource
    n <- choose (0, 30)
    pad <- vectorOf n (genPrefixed "g")
    pure . encodeText $
        object
            [ "verdict" .= ("diagnostic" :: Text)
            , "notCommitted" .= ("compile-gate" :: Text)
            , "diagnostic" .= diag
            , "source" .= src
            , "guidance" .= pad
            ]

{- | An arbitrary JSON result and its top-level key names. Big enough to be
worth eliding, and carrying no actionable key.
-}
genPayload :: Gen ([Text], Text)
genPayload = do
    ks <- nub <$> ((:) <$> genPrefixed "k" <*> listOf (genPrefixed "k"))
    vals <- mapM (const genFieldValue) ks
    filler <- genPrefixed "f"
    body <- T.take 300 . T.replicate 300 <$> genPrefixed "b"
    let pairs = (K.fromText filler .= body) : zipWith (\k v -> K.fromText k .= v) ks vals
    pure (filler : ks, encodeText (object pairs))

genFieldValue :: Gen Value
genFieldValue =
    oneof
        [ String <$> genPrefixed "s"
        , Number . fromIntegral <$> (arbitrary :: Gen Int)
        , Bool <$> arbitrary
        , hits <$> (choose (1, 4) >>= \n -> vectorOf n (genPrefixed "h"))
        ]
  where
    hits items = toJSON [object ["name" .= i, "module" .= ("M." <> i)] | i <- items]

-- | The index carried by the marker in the nth compacted message.
markerOf :: [Value] -> Int -> Maybe Int
markerOf seed i = markerIndex =<< field "content" =<< listToMaybe (drop i seed)

indexArgs :: Int -> Value
indexArgs n = object ["index" .= n]

-- | The index a stub tells the model to recall by.
markerIndex :: Text -> Maybe Int
markerIndex t = case T.breakOn "\"index\":" t of
    (_, rest)
        | T.null rest -> Nothing
        | otherwise ->
            case T.takeWhile isDigit (T.stripStart (T.drop 8 rest)) of
                digits | not (T.null digits) -> Just (read (T.unpack digits))
                _ -> Nothing

encodeText :: Value -> Text
encodeText = TE.decodeUtf8 . LBS.toStrict . encode

tshow :: (Show a) => a -> Text
tshow = T.pack . show
