{-# LANGUAGE OverloadedStrings #-}

{- | Pins the transcript mode of @siza retro@: what a rendered episode says
about the harness that produced it. The core is a round trip — plan an
episode with known ground truth, render it with the harness's own renderer,
read it back, and require the metrics to be what was planned.
-}
module Test.RetroTranscriptSpec (retroTranscriptSpec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Siza.Agent.Transcript (renderTranscript)
import Siza.Retro.Distribution (Spread (..), payloadSpread, quantile, spreadOf)
import Siza.Retro.Episode (EpisodeCall (..), Section (..), parseEpisode)
import Siza.Retro.Metrics (
    ElisionCounts (..),
    TranscriptMetrics (..),
    WriteCounts (..),
    metricsFromText,
 )
import Test.RetroBeat (Beat, Truth (..), beatMessages, episodeTruth)
import Test.RetroEpisodeGen (
    commit,
    deleteOf,
    genEpisode,
    genRepeatBeat,
    identifiers,
    rejectRun,
    renameIdentifiers,
    reroutedCommit,
    verdictOk,
 )

render :: [Beat] -> Text
render = renderTranscript "retro property" . beatMessages

measure :: [Beat] -> TranscriptMetrics
measure = metricsFromText . render

retroTranscriptSpec :: Spec
retroTranscriptSpec = describe "siza retro --transcript" $ do
    roundTripSpec
    repeatSpec
    invariantSpec
    falseSuccessSpec
    parserSpec
    spreadSpec

{- | The per-tool spread. A total tells a reader what an episode cost; only
the spread tells them whether one answer did it.
-}
spreadSpec :: Spec
spreadSpec = describe "payload spread per tool" $ do
    it "reports sizes that really occurred, ordered and totalling the sum" $
        forAll genEpisode $ \beats ->
            let m = measure beats
                spread = payloadSpread (tmPayloadRows m)
                sizes t = [n | (t', n) <- tmPayloadRows m, t' == t]
             in conjoin
                    [ counterexample (show (t, s)) $
                        conjoin
                            [ property (spMedian s `elem` sizes t)
                            , property (spMax s == maximum (sizes t))
                            , property (spMedian s <= spP90 s)
                            , property (spP90 s <= spMax s)
                            , spCount s === length (sizes t)
                            , spTotal s === sum (sizes t)
                            ]
                    | (t, s) <- M.toList spread
                    ]
    it "totals per tool agree with the flat per-tool counts" $
        forAll genEpisode $ \beats ->
            let m = measure beats
             in M.map spTotal (payloadSpread (tmPayloadRows m))
                    === tmPayloadPerTool m
    it "an empty sample reports no quantile rather than a stand-in size" $
        quantile 50 [] `shouldBe` Nothing
    it "an empty sample reports no spread at all" $
        spreadOf [] `shouldBe` Nothing
    it "every quantile it does report is a size the sample really held" $
        forAll (listOf1 (choose (0, 5000 :: Int))) $ \ns ->
            conjoin
                [ counterexample (show (p, ns)) (quantile p ns `elem` map Just ns)
                | p <- [0, 25, 50, 90, 100]
                ]

roundTripSpec :: Spec
roundTripSpec = describe "round trip against a planned episode" $
    it "recovers every count the plan asserts" $
        forAll genEpisode $ \beats ->
            let m = measure beats
                t = episodeTruth beats
                w = tmWrites m
                e = tmElision m
             in conjoin
                    [ counterexample "turns" (tmTurns m === trTurns t)
                    , counterexample "toolCalls" (tmToolCalls m === sum (trCalls t))
                    , counterexample "perTool" (tmPerTool m === trCalls t)
                    , counterexample "attempted" (wcAttempted w === trAttempted t)
                    , counterexample "committed" (wcCommitted w === trCommitted t)
                    , counterexample "rejected" (wcRejected w === trRejected t)
                    , counterexample "repeats" (tmRepeatedRejections m === trRepeats t)
                    , counterexample
                        "unchangedDiagnostics"
                        (tmUnchangedDiagnostics m === trUnchangedDiags t)
                    , counterexample
                        "unknownDiagnostics"
                        (tmUnknownDiagnostics m === trUnknownDiags t)
                    , counterexample
                        "duplicateResults"
                        (tmDuplicateResults m === trDuplicates t)
                    , counterexample "elided" (ecResults e === trElided t)
                    , counterexample "elidedFullChars" (ecFullChars e === trElidedFull t)
                    , counterexample
                        "payloadChars"
                        (tmPayloadChars m === sum (trPayload t))
                    , counterexample
                        "payloadPerTool"
                        (tmPayloadPerTool m === trPayload t)
                    , counterexample "thinkingChars" (tmThinkingChars m === trThinking t)
                    , counterexample "promptChars" (tmPromptChars m === trPrompt t)
                    ]

repeatSpec :: Spec
repeatSpec = describe "repeat detection on the write path" $ do
    it "scores a run of n identical submissions at n-1 repeats" $
        forAll ((,) <$> choose (2, 6) <*> elements identifiers) $ \(n, src) ->
            let m = measure [rejectRun "insert_cell" "GHC-9: stuck" (replicate n src)]
             in conjoin
                    [ counterexample "repeats" (tmRepeatedRejections m === n - 1)
                    , counterexample "unchanged" (tmUnchangedDiagnostics m === 0)
                    , counterexample "rejected" (wcRejected (tmWrites m) === n)
                    ]

    it "calls a changed source with an unchanged diagnostic something else" $
        forAll ((,) <$> choose (2, 6) <*> elements identifiers) $ \(n, src) ->
            let sources = [src <> " " <> T.pack (show i) | i <- [1 .. n]]
                m = measure [rejectRun "insert_cell" "GHC-9: stuck" sources]
             in conjoin
                    [ counterexample
                        "unchanged"
                        (tmUnchangedDiagnostics m === n - 1)
                    , counterexample "repeats" (tmRepeatedRejections m === 0)
                    ]

    it "ignores whitespace, and only whitespace, when comparing sources" $
        forAll (elements identifiers) $ \src ->
            let base = src <> " = 1"
                spaced = "  " <> base <> "  "
                broken = T.replace " " "\n  " base
                m = measure [rejectRun "insert_cell" "d" [base, spaced, broken]]
             in tmRepeatedRejections m === 2

    it "counts one more repeat for one more identical submission" $
        forAll genEpisode $ \beats ->
            forAll (elements identifiers) $ \src ->
                let run n = rejectRun "insert_cell" "GHC-1: stuck" (replicate n src)
                    twice = measure (beats <> [run 2])
                    thrice = measure (beats <> [run 3])
                 in conjoin
                        [ tmRepeatedRejections thrice === tmRepeatedRejections twice + 1
                        , tmToolCalls thrice === tmToolCalls twice + 1
                        ]

invariantSpec :: Spec
invariantSpec = describe "invariants sharper than the round trip" $ do
    it "is unchanged by renaming every identifier in the episode" $
        forAll genEpisode $ \beats ->
            metricsFromText (renameIdentifiers (render beats)) === measure beats

    it "never reports a count larger than the calls it is drawn from" $
        forAll genEpisode $ \beats ->
            let m = measure beats
                calls = tmToolCalls m
                w = tmWrites m
             in conjoin
                    [ counterexample "attempted" (property (wcAttempted w <= calls))
                    , counterexample
                        "repeats"
                        (property (tmRepeatedRejections m <= calls))
                    , counterexample
                        "unchanged"
                        (property (tmUnchangedDiagnostics m <= calls))
                    , counterexample
                        "duplicates"
                        (property (tmDuplicateResults m <= calls))
                    , counterexample
                        "writes partition"
                        ( wcCommitted w + wcRejected w + wcUnresolved w
                            === wcAttempted w
                        )
                    , counterexample "turns" (property (tmTurns m <= tmSections m))
                    ]

    it "reads a rejection run whose results were elided as unknown, not as repeats" $
        forAll genRepeatBeat $ \beat ->
            let m = measure [beat]
                w = tmWrites m
             in tmRepeatedRejections m
                    + tmUnchangedDiagnostics m
                    + tmUnknownDiagnostics m
                    >= wcRejected w - 1

falseSuccessSpec :: Spec
falseSuccessSpec = describe "falseSuccessClaims" $ do
    it "counts a done signal with no committed cell behind it" $
        tmFalseSuccess (measure [verdictOk]) `shouldBe` [1]

    it "spares one that a live committed cell stands behind" $
        forAll (choose (1, 99)) $ \cid ->
            tmFalseSuccess (measure [commit cid, verdictOk]) === []

    it "counts it again once that cell has been deleted" $
        forAll (choose (1, 99)) $ \cid ->
            tmFalseSuccess (measure [commit cid, deleteOf cid, verdictOk]) === [5]

    it "names a result that came back under a tool the model never called" $
        forAll (choose (1, 99)) $ \cid ->
            tmReroutedResults (measure [reroutedCommit cid]) === [2]

    it "counts a done signal that follows a refused write" $
        forAll (elements identifiers) $ \src ->
            tmDoneAfterRejection (measure [rejectRun "insert_cell" "d" [src], verdictOk])
                === [3]

    it "spares a done signal that follows a committed write" $
        forAll (choose (1, 99)) $ \cid ->
            tmDoneAfterRejection (measure [commit cid, verdictOk]) === []

    it "lists every done signal, false or not" $
        forAll (choose (1, 99)) $ \cid ->
            tmDoneSignals (measure [verdictOk, commit cid, verdictOk]) === [1, 4]

parserSpec :: Spec
parserSpec = describe "episode parser" $ do
    it "keeps a fenced block inside a thinking block" $ do
        let secs = parseEpisode nestedFenceEpisode
        map secRole secs `shouldBe` ["assistant"]
        map ecTool (concatMap secCalls secs) `shouldBe` ["insert_cell"]
        case secs of
            (s : _) -> secThinking s `shouldSatisfy` T.isInfixOf "import Data.Map"
            [] -> expectationFailure "no sections parsed"

    it "is unmoved by the MCP driver's trailing footer" $
        forAll genEpisode $ \beats ->
            metricsFromText (render beats <> mcpFooter) === measure beats

    it "reads a harness-injected result that answers no call" $ do
        let secs = parseEpisode injectedResultEpisode
        map secTool secs `shouldBe` [Just "verify"]

mcpFooter :: Text
mcpFooter = "\n<!-- turns: 42 elapsed: 91s -->\n"

nestedFenceEpisode :: Text
nestedFenceEpisode =
    T.unlines
        [ "# Session: nested"
        , ""
        , "## 1. assistant"
        , ""
        , "*thinking:*"
        , "```"
        , "Let us write:"
        , ""
        , "```"
        , "import Data.Map"
        , "```"
        , ""
        , "and then commit it."
        , "```"
        , ""
        , "**tool calls:**"
        , "- `insert_cell` {\"source\":\"x = 1\"}"
        , ""
        ]

injectedResultEpisode :: Text
injectedResultEpisode =
    T.unlines
        [ "# Session: injected"
        , ""
        , "## 1. tool (verify)"
        , ""
        , "```"
        , "[verdict: ok] Deliverable confirmed."
        , "```"
        , ""
        ]
