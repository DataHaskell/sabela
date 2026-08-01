{-# LANGUAGE OverloadedStrings #-}

module Test.FutilitySpec (futilitySpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Futility (
    futilityNote,
    guardDispatch,
    newFutilityGuard,
    normaliseDiagnostic,
    unchangedState,
 )
import Test.TruthGen (genGhcDiagnostic, genSubstantiveSource)

call :: Text -> ToolCall
call src = ToolCall "insert_cell" (object ["source" .= src])

failingWith :: Text -> ToolCall -> IO (Either Text ToolOutcome)
failingWith e _ = pure (Left e)

noteOf :: Either Text ToolOutcome -> Maybe Text
noteOf (Left e) = Just e
noteOf (Right (ToolErr (Object o))) = case KM.lookup "futility" o of
    Just (String s) -> Just s
    _ -> Nothing
noteOf _ = Nothing

hasNote :: Either Text ToolOutcome -> Bool
hasNote (Left e) = "byte-identical" `T.isInfixOf` e
hasNote (Right (ToolErr (Object o))) = KM.member "futility" o
hasNote _ = False

futilitySpec :: Spec
futilitySpec = describe "Siza.Agent.Futility (retry-futility guard)" $ do
    it "does not annotate a first failure" $ do
        g <- newFutilityGuard
        r <- guardDispatch g (failingWith "boom") (call "x = 1")
        hasNote r `shouldBe` False

    it "annotates the second byte-identical identically-failing call" $ do
        g <- newFutilityGuard
        _ <- guardDispatch g (failingWith "boom") (call "x = 1")
        r <- guardDispatch g (failingWith "boom") (call "x = 1")
        hasNote r `shouldBe` True
        case r of
            Left e -> do
                e `shouldSatisfy` T.isInfixOf "boom"
                e `shouldSatisfy` T.isInfixOf futilityNote
            _ -> expectationFailure "expected Left"

    it "directs away from payload rewriting, toward a different approach" $ do
        futilityNote `shouldSatisfy` T.isInfixOf "not the fault"
        futilityNote `shouldSatisfy` T.isInfixOf "Change approach"

    describe "truthful futility for a deterministic rejection" $ do
        let gateRejection =
                ToolErr
                    ( object
                        [ "notCommitted" .= ("compile-gate" :: Text)
                        , "verdict" .= ("diagnostic" :: Text)
                        , "diagnostic"
                            .= ("<interactive>:238:1: error: [GHC-88464]" :: Text)
                        ]
                    )
            rejecting _ = pure (Right gateRejection)
            secondNote = do
                g <- newFutilityGuard
                _ <- guardDispatch g rejecting (call "x = 1 +")
                out <- guardDispatch g rejecting (call "x = 1 +")
                pure (noteOf out)

        it "false-futility: never tells the model the payload is not the fault" $ do
            n <- secondNote
            n `shouldSatisfy` maybe False (not . T.isInfixOf "not the fault")

        it "names the source as the fault" $ do
            n <- secondNote
            n `shouldSatisfy` maybe False (T.isInfixOf "source")

        it "never steers a source error at state-inspection tools" $ do
            n <- secondNote
            n `shouldSatisfy` maybe False (not . T.isInfixOf "kernel_status")

        it "still gives the environmental note for a transport failure" $ do
            g <- newFutilityGuard
            _ <- guardDispatch g (failingWith "boom") (call "x = 1")
            out <- guardDispatch g (failingWith "boom") (call "x = 1")
            hasNote out `shouldBe` True

    it "does not annotate when the arguments differ" $ do
        g <- newFutilityGuard
        _ <- guardDispatch g (failingWith "boom") (call "x = 1")
        r <- guardDispatch g (failingWith "boom") (call "x = 2")
        hasNote r `shouldBe` False

    it "does not annotate when the failure differs" $ do
        g <- newFutilityGuard
        errs <- newIORef (["a", "b"] :: [Text])
        let d _ = do
                es <- readIORef errs
                modifyIORef' errs (drop 1)
                pure (Left (case es of e : _ -> e; [] -> "z"))
        _ <- guardDispatch g d (call "x = 1")
        r <- guardDispatch g d (call "x = 1")
        hasNote r `shouldBe` False

    it "a success clears the memory for that call" $ do
        g <- newFutilityGuard
        _ <- guardDispatch g (failingWith "boom") (call "x = 1")
        _ <-
            guardDispatch
                g
                (\_ -> pure (Right (ToolOk (object []))))
                (call "x = 1")
        r <- guardDispatch g (failingWith "boom") (call "x = 1")
        hasNote r `shouldBe` False

    it "annotates a repeated identical ToolErr via the futility field" $ do
        g <- newFutilityGuard
        let d _ = pure (Right (ToolErr (object ["error" .= ("bad" :: Text)])))
        _ <- guardDispatch g d (call "x = 1")
        r <- guardDispatch g d (call "x = 1")
        hasNote r `shouldBe` True

    unchangedDiagnosticSpec

{- | C2-9a: the episode's rejections were never byte-identical. The SOURCE
changed every time and the DIAGNOSTIC did not, which is what the guard has to
key on for a rejection loop to be visible at all.
-}
unchangedDiagnosticSpec :: Spec
unchangedDiagnosticSpec = describe "an unchanged diagnostic over changed sources" $ do
    it "erases only the interactive position, and always the same way" $
        property $
            forAll ((,,) <$> genGhcDiagnostic <*> genPosition <*> genPosition) $
                \(body, p1, p2) ->
                    normaliseDiagnostic (p1 <> body)
                        === normaliseDiagnostic (p2 <> body)
                        .&&. counterexample
                            "the body was erased too"
                            (body `T.isInfixOf` normaliseDiagnostic (p1 <> body))

    it "marks the second rejection unchanged though the source changed (C2-9a)" $
        property $
            forAll genUnchangedRun $ \(s1, s2, body, p1, p2) -> ioProperty $ do
                out <- runRejections [(s1, p1 <> body), (s2, p2 <> body)]
                pure $
                    counterexample (show out) (stateOf out === Just unchangedState)
                        .&&. counterexample
                            "claimed the payload is not the fault"
                            (noteOf out === Nothing)

    it "reports the counts and the delta it measured, and nothing else" $
        property $
            forAll genUnchangedRun $ \(s1, s2, body, p1, p2) -> ioProperty $ do
                out <- runRejections [(s1, p1 <> body), (s2, p2 <> body)]
                let d = unchangedField out
                pure $
                    counterexample (show out) $
                        intOf "priorCalls" d
                            === Just 1
                            .&&. intOf "distinctSources" d
                                === Just 2
                            .&&. boolOf "sourceChanged" d
                                === Just True
                            .&&. counterexample
                                "no delta for a changed source"
                                (KM.member (K.fromText "changedLines") (fieldsOf d))

    it "counts a run of three over three distinct sources" $
        property $
            forAll ((,,) <$> genDistinct 3 <*> genGhcDiagnostic <*> vectorOf 3 genPosition) $
                \(srcs, body, ps) -> ioProperty $ do
                    out <- runRejections (zip srcs [p <> body | p <- ps])
                    let d = unchangedField out
                    pure $
                        intOf "priorCalls" d
                            === Just 2
                            .&&. intOf "distinctSources" d
                                === Just 3

    it "marks nothing when the normalised diagnostic differs (C2-9a dual)" $
        property $
            forAll genChangedRun $ \(s1, s2, d1, d2) -> ioProperty $ do
                out <- runRejections [(s1, d1), (s2, d2)]
                pure (counterexample (show out) (stateOf out === Nothing))

    it "marks nothing on the first rejection of a class" $
        property $
            forAll ((,) <$> genSubstantiveSource <*> genGhcDiagnostic) $
                \(s, body) -> ioProperty $ do
                    out <- runRejections [(s, "<interactive>:5:9: " <> body)]
                    pure (stateOf out === Nothing)

{- | Dispatch each (source, diagnostic) pair through one guard, as a rejection
the compile gate would return, and hand back the last outcome.
-}
runRejections :: [(Text, Text)] -> IO (Either Text ToolOutcome)
runRejections steps = do
    g <- newFutilityGuard
    let one (src, diag) = guardDispatch g (rejectingWith diag) (call src)
    outs <- mapM one steps
    pure (last (Left "no step" : outs))

rejectingWith :: Text -> ToolCall -> IO (Either Text ToolOutcome)
rejectingWith diag _ =
    pure . Right . ToolErr $
        object
            [ "notCommitted" .= ("compile-gate" :: Text)
            , "verdict" .= ("diagnostic" :: Text)
            , "diagnostic" .= diag
            ]

-- | An @\<interactive\>@ position of the shape GHC prints for a session cell.
genPosition :: Gen Text
genPosition = do
    l <- choose (1, 99999 :: Int)
    c <- choose (1, 400 :: Int)
    pure ("<interactive>:" <> tshow l <> ":" <> tshow c <> ": error: ")

{- | Two different sources, one diagnostic body, and two independently drawn
positions: the shape the transcript's §96/§98/§100 run actually had.
-}
genUnchangedRun :: Gen (Text, Text, Text, Text, Text)
genUnchangedRun = do
    (s1, s2) <- genDistinctPair
    body <- genGhcDiagnostic
    p1 <- genPosition
    p2 <- genPosition
    pure (s1, s2, body, p1, p2)

-- | Two rejections whose diagnostics differ even after normalisation.
genChangedRun :: Gen (Text, Text, Text, Text)
genChangedRun = do
    (s1, s2) <- genDistinctPair
    p1 <- genPosition
    p2 <- genPosition
    b1 <- genGhcDiagnostic
    b2 <- genGhcDiagnostic `suchThat` (/= b1)
    pure (s1, s2, p1 <> b1, p2 <> b2)

genDistinctPair :: Gen (Text, Text)
genDistinctPair = do
    s1 <- genSubstantiveSource
    s2 <- genSubstantiveSource `suchThat` (/= s1)
    pure (s1, s2)

genDistinct :: Int -> Gen [Text]
genDistinct n = vectorOf n genSubstantiveSource `suchThat` allDifferent
  where
    allDifferent xs = length (foldr keep [] xs) == n
    keep x acc = if x `elem` acc then acc else x : acc

stateOf :: Either Text ToolOutcome -> Maybe Text
stateOf out = case KM.lookup (K.fromText "state") (errFields out) of
    Just (String s) -> Just s
    _ -> Nothing

unchangedField :: Either Text ToolOutcome -> Maybe Value
unchangedField out = KM.lookup (K.fromText "unchanged") (errFields out)

errFields :: Either Text ToolOutcome -> KM.KeyMap Value
errFields (Right (ToolErr (Object o))) = o
errFields _ = KM.empty

fieldsOf :: Maybe Value -> KM.KeyMap Value
fieldsOf (Just (Object o)) = o
fieldsOf _ = KM.empty

intOf :: Text -> Maybe Value -> Maybe Int
intOf k d = case KM.lookup (K.fromText k) (fieldsOf d) of
    Just (Number n) -> Just (round n)
    _ -> Nothing

boolOf :: Text -> Maybe Value -> Maybe Bool
boolOf k d = case KM.lookup (K.fromText k) (fieldsOf d) of
    Just (Bool b) -> Just b
    _ -> Nothing

tshow :: (Show a) => a -> Text
tshow = T.pack . show
