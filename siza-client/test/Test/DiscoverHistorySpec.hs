{-# LANGUAGE OverloadedStrings #-}

module Test.DiscoverHistorySpec (discoverHistorySpec) where

import Data.Aeson (Value, object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.Envelope (
    envelopeCharBudget,
    envelopeChars,
    envelopeViolations,
 )
import Siza.Agent.Discover.History (
    SearchLedger,
    emptyLedger,
    ledgerClose,
    ledgerRecord,
    ledgerShortcut,
    ledgerWorldChanged,
 )
import Siza.Agent.Discover.HistoryGuard (guardDiscover, newSearchLedger)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    NotebookEnv (..),
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Test.DiscoverFixtures (argText, stateOf, textField)
import Test.DiscoverGen (genGoalFreeRow, genPkgName)

envB :: NotebookEnv
envB =
    seededBuiltins
        ( NotebookEnv
            [("D", "DataFrame")]
            ["DataFrame"]
            [("DataFrame", 0)]
            ["col", "cols"]
            []
            []
        )

hk0 :: HackageInfo
hk0 = HackageInfo True []

foundCumulus :: Value
foundCumulus =
    discoverEnvelope
        envB
        (interpret envB "cumulus")
        8
        [okAnswer "session" [hiddenBars]]
        (HackageInfo True ["cumulus"])
  where
    hiddenBars =
        (mkHit "bars" "Cumulus.Plot" "cumulus")
            { dhType = "[(Text, Double)] -> Text"
            , dhVersion = "0.3.1"
            , dhInstall = InstHidden
            , dhCabal = Just "-- cabal: build-depends: cumulus"
            }

foundAliased :: Value
foundAliased =
    discoverEnvelope
        envB
        (interpret envB "D.gust")
        8
        [okAnswer "session" [(mkHit "gust" "DataFrame" "dataframe"){dhVersion = "2.0"}]]
        hk0

missOf :: Text -> Value
missOf q =
    discoverEnvelope
        envB
        (interpret envB q)
        8
        [okAnswer "session" [], okAnswer "hoogle" []]
        hk0

script :: [(Text, Value)] -> (SearchLedger, [Value])
script = foldl step (emptyLedger, [])
  where
    step (led, outs) (q, v) = case ledgerShortcut led q of
        Just out -> (led, outs ++ [out])
        Nothing ->
            let (led2, out) = ledgerRecord q v led
             in (led2, outs ++ [out])

hunt :: [(Text, Value)]
hunt =
    [ ("cumulus", foundCumulus)
    , ("D.gust", foundAliased)
    , ("colx", missOf "colx")
    , ("col", missOf "col")
    , ("cols", missOf "cols")
    , ("colx @Int", missOf "colx @Int")
    , ("`colx`", missOf "`colx`")
    , ("colX", missOf "colX")
    , ("colx", missOf "colx")
    ]

adviceOf :: Value -> Text
adviceOf v = textField "next" v <> " " <> textField "summary" v

suggestedNames :: Value -> [Text]
suggestedNames v = case T.breakOn "Nearest held names:" (adviceOf v) of
    (_, rest)
        | T.null rest -> []
        | otherwise ->
            map (T.strip . T.dropAround (== '.')) . T.splitOn "," $
                T.takeWhile (/= '.') (T.drop (T.length "Nearest held names:") rest)

discoverHistorySpec :: Spec
discoverHistorySpec = describe "discover history ledger (R3.8, R5.5-R5.7)" $ do
    let (led, outs) = script hunt

    describe "R5.5 advice never repeats a tried query shape" $ do
        it "the first miss still suggests nearest names" $
            suggestedNames (outs !! 2) `shouldContain` ["col"]
        it "a tried name is never suggested again" $ do
            suggestedNames (outs !! 3) `shouldSatisfy` notElem "col"
            suggestedNames (outs !! 4) `shouldSatisfy` notElem "col"
            suggestedNames (outs !! 4) `shouldSatisfy` notElem "cols"
        it "holds over the whole generated sequence" $ do
            let triedBefore i = map fst (take i hunt)
                bad =
                    [ (i, s)
                    | (i, out) <- zip [0 ..] outs
                    , s <- suggestedNames out
                    , s `elem` triedBefore i
                    ]
            bad `shouldBe` []

    describe "R5.6 escalation: the record grows by rung, it never advises" $ do
        it "the second miss in a cluster surfaces the held facts" $ do
            let advice = adviceOf (outs !! 5)
            advice `shouldSatisfy` T.isInfixOf "-- cabal: build-depends: cumulus"
            advice `shouldSatisfy` T.isInfixOf "alias D = DataFrame"
        it "the third miss names what was consulted and carries the facts" $ do
            let advice = adviceOf (outs !! 6)
            advice `shouldSatisfy` T.isInfixOf "no match in any recorded answer"
            advice `shouldSatisfy` T.isInfixOf "-- cabal: build-depends: cumulus"
        it "a fourth same-cluster query is a terse reference" $ do
            stateOf (outs !! 7) `shouldBe` "duplicate"
            envelopeChars (outs !! 7) `shouldSatisfy` (< 500)

    describe "R3.8 byte-identical repeats" $ do
        it "returns a one-line reference, never a re-transmitted wall" $ do
            stateOf (outs !! 8) `shouldBe` "duplicate"
            textField "ref" (outs !! 8) `shouldSatisfy` (not . T.null)
            envelopeChars (outs !! 8) `shouldSatisfy` (< 500)
        it "re-runs fully after the world changed (install/restart)" $ do
            let led2 = ledgerWorldChanged led
            ledgerShortcut led2 "colx" `shouldBe` Nothing

    describe "after close the record replays; no channel says search more" $ do
        let closed = ledgerClose led
            Just gated = ledgerShortcut closed "colx"
        it "an unseen scope key is never answered from the closed ledger" $
            ledgerShortcut closed "anything" `shouldBe` Nothing
        it "a discover call after close is a terse replay of the record" $ do
            stateOf gated `shouldBe` "duplicate"
            adviceOf gated `shouldSatisfy` T.isInfixOf "Already held"
        it "the gated answer carries the held facts" $
            adviceOf gated
                `shouldSatisfy` T.isInfixOf "-- cabal: build-depends: cumulus"
        it "no post-close or escalated advice instructs more searching" $ do
            let banned = ["retry", "different shape", "rephrase", "search again"]
                texts = adviceOf gated : map adviceOf (drop 6 outs)
            [ (t, b) | t <- texts, b <- banned, b `T.isInfixOf` T.toLower t
              ]
                `shouldBe` []

    dedupEscalationSpec

-- ----------------------------------------- the dedup rung escalates (D5)

consumerQ :: Text
consumerQ = "consumeIt"

missQ :: Text
missQ = "someSpelling"

{- | A found answer whose one hit consumes a type nothing held produces, which
is what leaves a goal standing behind the queries that follow.
-}
consumerAnswer :: Text -> Text -> Value
consumerAnswer goal pkg =
    object
        [ "query" .= consumerQ
        , "state" .= ("found" :: Text)
        , "total" .= (1 :: Int)
        , "hits"
            .= [ object
                    [ "name" .= consumerQ
                    , "type" .= (goal <> " -> Text")
                    , "module" .= holderModule
                    , "package" .= pkg
                    , "install" .= ("installed" :: Text)
                    , "matchKind" .= ("exact" :: Text)
                    , "origin" .= ("hoogle" :: Text)
                    , "cabal" .= ("-- cabal: build-depends: " <> pkg)
                    ]
               ]
        ]

holderModule :: Text
holderModule = "Some.Module"

producerName :: Text
producerName = "makeIt"

-- | The capability answer a type query comes back with: one real producer.
producerAnswer :: Text -> Text -> Value
producerAnswer goal pkg =
    object ["hits" .= [object ["package" .= pkg, "api" .= [row]]]]
  where
    row =
        object
            [ "name" .= producerName
            , "module" .= holderModule
            , "type" .= ("Int -> " <> goal)
            ]

missAnswer :: Text -> Value
missAnswer q =
    object
        [ "query" .= q
        , "state" .= ("not_found" :: Text)
        , "total" .= (0 :: Int)
        ]

{- | One consumer call, then the same miss asked @n@ times: the second and
later asks are duplicates, which is the rung the live rounds closed on.
-}
repeatRun :: Int -> Text -> Text -> IO ([Value], [Text])
repeatRun n goal pkg = do
    seen <- newIORef []
    ref <- newSearchLedger
    let inner tc = do
            modifyIORef' seen (++ [tc])
            pure (Right (ToolOk (answerFor tc)))
        answerFor tc
            | tcName tc == "search_capability" = producerAnswer goal pkg
            | argText "query" (tcArgs tc) == consumerQ = consumerAnswer goal pkg
            | otherwise = missAnswer (argText "query" (tcArgs tc))
        one q = guardDiscover ref inner (ToolCall "discover" (object ["query" .= q]))
    outs <- mapM one (consumerQ : replicate n missQ)
    calls <- readIORef seen
    pure
        ( [v | Right (ToolOk v) <- outs]
        , [argText "query" (tcArgs c) | c <- calls, tcName c == "search_capability"]
        )

dedupEscalationSpec :: Spec
dedupEscalationSpec = describe "a duplicate escalates too, once per cluster" $ do
    prop "a repeated question under a standing goal spends one type query" $
        forAll ((,) <$> genGoalFreeRow <*> genPkgName) $ \((goal, _, _), pkg) ->
            ioProperty $ do
                (outs, typeQs) <- repeatRun 4 goal pkg
                let said = T.concat (map (textField "next") outs)
                pure
                    . counterexample (show (typeQs, said))
                    $ conjoin
                        [ typeQs === ["+" <> pkg <> " :: " <> goal]
                        , map stateOf outs
                            === ["found", "not_found", "duplicate", "duplicate", "duplicate"]
                        , property (T.isInfixOf (producerName <> " :: Int -> " <> goal) said)
                        , concatMap envelopeViolations outs === []
                        , property (all ((<= envelopeCharBudget) . envelopeChars) outs)
                        ]
    prop "the disclosure names the harness's own reasoning, not an answer" $
        forAll ((,) <$> genGoalFreeRow <*> genPkgName) $ \((goal, _, _), pkg) ->
            ioProperty $ do
                (outs, _) <- repeatRun 3 goal pkg
                let said = T.concat (map (textField "next") outs)
                pure
                    . counterexample (T.unpack said)
                    $ conjoin
                        [ property (T.isInfixOf "the name is the wrong axis" said)
                        , property (T.isInfixOf "the type query" said)
                        ]
