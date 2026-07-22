{-# LANGUAGE OverloadedStrings #-}

{- | The history-aware feedback protocol (testing-plan R3.8, R5.5-R5.7) driven
by a scripted literal-minded caller: advice never repeats a tried query shape,
misses escalate to held facts by the second and an act-or-blocker directive by
the third, a byte-identical repeat is a one-line reference, and once the nudge
says act no channel says search more.
-}
module Test.DiscoverHistorySpec (discoverHistorySpec) where

import Data.Aeson (Value, object, (.=))
import Data.IORef (newIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.Envelope (envelopeChars)
import Siza.Agent.Discover.History (
    SearchLedger,
    emptyLedger,
    heldFacts,
    ledgerClose,
    ledgerRecord,
    ledgerShortcut,
    ledgerWorldChanged,
 )
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
import Siza.Agent.Loop.Support (forceActMsgWith, nudgeK, updateNudge)
import Test.DiscoverFixtures (stateOf, textField)

-- | Notebook with an alias import and two bindings near the hunted name.
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

-- | A found answer for a hidden package: the cabal-line fact to hold.
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

-- | A found alias-resolved answer: the alias fact to hold.
foundAliased :: Value
foundAliased =
    discoverEnvelope
        envB
        (interpret envB "D.gust")
        8
        [okAnswer "session" [(mkHit "gust" "DataFrame" "dataframe"){dhVersion = "2.0"}]]
        hk0

-- | A miss for the given raw query (empty sources; nearest names from envB).
missOf :: Text -> Value
missOf q =
    discoverEnvelope
        envB
        (interpret envB q)
        8
        [okAnswer "session" [], okAnswer "hoogle" []]
        hk0

-- | The scripted literal-minded caller: shortcut else record, in order.
script :: [(Text, Value)] -> (SearchLedger, [Value])
script = foldl step (emptyLedger, [])
  where
    step (led, outs) (q, v) = case ledgerShortcut led q of
        Just out -> (led, outs ++ [out])
        Nothing ->
            let (led2, out) = ledgerRecord q v led
             in (led2, outs ++ [out])

-- | The generated miss sequence: facts first, then the colx hunt.
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

-- | The names the "Nearest held names" sentence suggests, if any.
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

    describe "R5.6 escalation: facts by miss 2, act-or-blocker by miss 3" $ do
        it "the second miss in a cluster surfaces the held facts" $ do
            let advice = adviceOf (outs !! 5)
            advice `shouldSatisfy` T.isInfixOf "-- cabal: build-depends: cumulus"
            advice `shouldSatisfy` T.isInfixOf "alias D = DataFrame"
        it "the third miss says searching cannot help and carries the facts" $ do
            let advice = adviceOf (outs !! 6)
            advice `shouldSatisfy` T.isInfixOf "cannot help"
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

    describe "R5.7 after the nudge says act, no channel says search more" $ do
        -- Closure is scope-keyed (section 8.2): only a SEEN key shortcuts;
        -- an unseen key re-runs the backends even after close.
        let closed = ledgerClose led
            Just gated = ledgerShortcut closed "colx"
        it "an unseen scope key is never answered from the closed ledger" $
            ledgerShortcut closed "anything" `shouldBe` Nothing
        it "a discover call after close is a terse act reference" $ do
            stateOf gated `shouldBe` "duplicate"
            adviceOf gated `shouldSatisfy` T.isInfixOf "act"
        it "the gated answer carries the held facts" $
            adviceOf gated
                `shouldSatisfy` T.isInfixOf "-- cabal: build-depends: cumulus"
        it "no post-close or escalated advice instructs more searching" $ do
            let banned = ["retry", "different shape", "rephrase", "search again"]
                texts = adviceOf gated : map adviceOf (drop 6 outs)
            [ (t, b) | t <- texts, b <- banned, b `T.isInfixOf` T.toLower t
              ]
                `shouldBe` []

    describe "the held-facts budget nudge (R5.6/R5.7)" $ do
        it "forceActMsgWith carries the facts and the remaining budget" $ do
            let facts = heldFacts led
                msg = forceActMsgWith facts "Remaining budget: 3 turns."
                content = textField "content" msg
            facts `shouldSatisfy` (not . null)
            mapM_ (\f -> content `shouldSatisfy` T.isInfixOf f) facts
            content `shouldSatisfy` T.isInfixOf "Remaining budget: 3 turns."
            content `shouldSatisfy` T.isInfixOf "act"
        it "updateNudge injects the built nudge when the trigger crosses" $ do
            ref <- newIORef (0 :: Int)
            let msg = forceActMsgWith ["fact one"] "Remaining budget: 5 rounds."
                readCall = ToolCall "list_cells" (object ["full" .= True])
            out <- updateNudge (pure msg) ref nudgeK True 5 (replicate nudgeK readCall)
            out `shouldBe` [msg]
