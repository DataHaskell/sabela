{-# LANGUAGE OverloadedStrings #-}

{- | G3 tasks 2 and 3: the harness asks the compiler, the model is never
asked to write a hole. A scripted dispatch stands in for the server's
typecheck-only @try@ route; the fixtures assert the conclusions reach the
fact ledger with provenance, that NO notebook mutation is ever attempted,
that a two-hole candidate resolves inside the round cap, and that an
unanswerable gap yields a plain statement rather than a dangling
recommendation.
-}
module Test.HoleProbeSpec (holeProbeSpec) where

import Data.Aeson (Value, object)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.HoleProbe (holeProbeProvenance)
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.Candidate (candidateCell, candidateGaps)
import Siza.Agent.Discover.History (emptyLedger, heldFacts)
import Siza.Agent.Discover.HistoryGuard (recordProbeFacts)
import Siza.Agent.Discover.HoleProbe (
    ProbeDispatch,
    groundedTarget,
    probeGroundedType,
    probeTargetType,
    resolveCandidate,
    synthesisRoundCap,
 )
import Siza.Agent.Discover.Ledger (SearchLedger (..))
import Test.ProbeFixtures (probeCode, probeFactFor, scriptedTryOutcome)

-- | The live_test4 target: a consumer whose two argument slots are `Point`.
lineFact :: Text
lineFact =
    "`line` :: Point -> Point -> Picture — found in Sabela.Notebook (sabela-notebook)"

-- | A two-gap consumer over two DIFFERENT types, for the K-round fixture.
segmentFact :: Text
segmentFact =
    "`segment` :: Anchor -> Extent -> Picture — found in Fixture.Draw (fixture)"

{- | A dispatch that answers probes from a table and records every call, so a
fixture can assert what the harness did — and what it never did.
-}
scriptedDispatch :: [(Text, [Text])] -> IO (IORef [ToolCall], ProbeDispatch)
scriptedDispatch table = do
    calls <- newIORef []
    let dispatch tc = do
            atomicModifyIORef' calls (\cs -> (cs ++ [tc], ()))
            pure . Right . ToolOk $ answerFor tc
        answerFor tc
            | tcName tc /= "try" = object []
            | otherwise = scriptedTryOutcome table (tcArgs tc)
    pure (calls, dispatch)

mutatingCalls :: [ToolCall] -> [Text]
mutatingCalls cs =
    [ tcName c
    | c <- cs
    , tcName c
        `elem` ["insert_cell", "replace_cell_source", "propose_edit", "run_cell"]
    ]

{- | live_test9's ungrounded target: @Frequency@ was conjured from the query
token @Sine@ (an ALUT constructor, for a plotting request). Nothing in scope
produces it, so it must never be probed at all.
-}
plotFact :: Text
plotFact =
    "`plot` :: [(Double, Double)] -> Picture \
    \— found in Sabela.Notebook (sabela-notebook)"

holeProbeSpec :: Spec
holeProbeSpec = describe "G3 harness-side hole probing and bounded synthesis" $ do
    describe "ungrounded-probe: a target nothing produces is never probed" $ do
        it "runs ZERO probes and adds zero facts for an ungrounded type" $ do
            (calls, dispatch) <- scriptedDispatch []
            facts <- probeGroundedType dispatch [plotFact] "Frequency"
            -- Assert on the PROBE COUNT, not just the ledger: a probe that
            -- runs and is discarded still fails this.
            readIORef calls `shouldReturn` []
            facts `shouldBe` []

        it "probes a target something in scope really does produce" $ do
            (calls, dispatch) <-
                scriptedDispatch [("Picture", ["`plot` produces Picture"])]
            facts <- probeGroundedType dispatch [plotFact] "Picture"
            cs <- readIORef calls
            length cs `shouldBe` 1
            facts `shouldNotBe` []

        it "grounds on the producer test, not on lexical association" $ do
            -- `plot` merely MENTIONS Double; it produces Picture.
            groundedTarget [plotFact] "Picture" `shouldBe` True
            groundedTarget [plotFact] "Frequency" `shouldBe` False
            groundedTarget [plotFact] "Double" `shouldBe` False
            groundedTarget [] "Picture" `shouldBe` False

    describe "signature-synthesis: a held signature needs no hole" $ do
        it "builds a compiling application from the confirmed signature" $ do
            -- The exact live_test9 state: `plot`'s signature confirmed at
            -- turn 11, no hole anywhere, twenty turns spent saying "write".
            candidateGaps [plotFact] `shouldBe` []
            case candidateCell [plotFact] of
                Nothing -> expectationFailure "expected a synthesised candidate"
                Just src -> do
                    src `shouldSatisfy` T.isInfixOf "plot"
                    src `shouldSatisfy` T.isInfixOf "import Sabela.Notebook"
                    src `shouldNotSatisfy` T.isInfixOf "_ ::"

    describe "the probe on the live_test4 target type" $ do
        it "folds the compiler's producers into the ledger with provenance" $ do
            (calls, dispatch) <- scriptedDispatch [("Point", ["origin", "mkPoint"])]
            facts <- probeTargetType dispatch "Point"
            facts `shouldBe` [probeFactFor "Point" ["origin", "mkPoint"]]
            facts `shouldSatisfy` all (T.isInfixOf holeProbeProvenance)
            recorded <- readIORef calls
            map tcName recorded `shouldBe` ["try"]

        it "mutates no notebook: no write tool is ever called, no cell touched" $ do
            (calls, dispatch) <- scriptedDispatch [("Point", [])]
            _ <- resolveCandidate dispatch Nothing [lineFact]
            recorded <- readIORef calls
            mutatingCalls recorded `shouldBe` []
            -- every call the harness made was the read-only trial route
            map tcName recorded `shouldSatisfy` all (== "try")

        it "lands in the ledger through the same bounded fold as any fact" $ do
            (_, dispatch) <- scriptedDispatch [("Point", ["origin"])]
            facts <- probeTargetType dispatch "Point"
            ref <- newIORef emptyLedger
            recordProbeFacts ref facts
            recordProbeFacts ref facts
            led <- readIORef ref
            heldFacts led `shouldBe` facts
            slFacts led `shouldSatisfy` all (T.isInfixOf holeProbeProvenance)

    describe "bounded synthesis" $ do
        it "resolves a two-hole candidate within the round cap" $ do
            (calls, dispatch) <-
                scriptedDispatch
                    [("Anchor", ["originAnchor"]), ("Extent", ["unitExtent"])]
            (facts, resolved) <- resolveCandidate dispatch Nothing [segmentFact]
            resolved
                `shouldBe` Just "import Fixture.Draw\nsegment originAnchor unitExtent"
            candidateGaps facts `shouldBe` []
            probeCalls <- length . filter isProbe <$> readIORef calls
            probeCalls `shouldSatisfy` (<= 2 * synthesisRoundCap)

        it "surfaces nothing while a gap is unfilled" $ do
            (_, dispatch) <- scriptedDispatch [("Anchor", ["originAnchor"])]
            (_, resolved) <- resolveCandidate dispatch Nothing [segmentFact]
            resolved `shouldBe` Nothing

        it "an unanswerable gap becomes a plain statement, not a recommendation" $ do
            (_, dispatch) <- scriptedDispatch [("Point", [])]
            (facts, resolved) <- resolveCandidate dispatch Nothing [lineFact]
            resolved `shouldBe` Nothing
            facts
                `shouldSatisfy` any
                    ( \f ->
                        "no producer of `Point` found in scope" `T.isInfixOf` f
                            && holeProbeProvenance `T.isInfixOf` f
                    )
            facts `shouldSatisfy` (not . any (T.isInfixOf "insert_cell"))
            facts `shouldSatisfy` (not . any (T.isInfixOf "_ ::"))

        it "an answered-but-empty gap is never re-probed" $ do
            (calls, dispatch) <- scriptedDispatch [("Point", [])]
            _ <- resolveCandidate dispatch Nothing [lineFact]
            recorded <- readIORef calls
            length (filter isProbe recorded) `shouldBe` 1

        it "a probed producer makes the pure generator emit a hole-free cell" $ do
            (_, dispatch) <- scriptedDispatch [("Point", ["origin"])]
            (facts, _) <- resolveCandidate dispatch Nothing [lineFact]
            case candidateCell facts of
                Nothing -> expectationFailure "expected a candidate once the gap was filled"
                Just src -> do
                    src `shouldSatisfy` (not . T.isInfixOf "_ ::")
                    src `shouldSatisfy` T.isInfixOf "line origin origin"
  where
    isProbe tc = tcName tc == "try" && isJust (probeCode (tcArgs tc))
