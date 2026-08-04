{-# LANGUAGE OverloadedStrings #-}

{- | One classifier over one shared result. `try` and the compile gate read the
same 'DisposableResult', so they must agree on whose code a failure belongs to
and on what class of verdict it is — over every stage AND every verdict, not
over one of each (LEDGER C2-2d, C2-2e).
-}
module Test.DisposableAttributionSpec (spec) where

import Data.Aeson (Value (..))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.Try.Payload (disposablePayload)
import Sabela.AI.Verdict (verdictVocabulary)
import Sabela.Session.MaterializeStage (
    Attribution (..),
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    MaterializeStage (..),
    attributionOf,
    blamedCell,
    environmentFault,
    materializeStages,
    stageFailure,
    stageNamesCell,
    stageReachedCandidate,
    stageUsedCandidateMetadata,
 )
import Test.DisposableFixtures (
    baseResult,
    field,
    gateOf,
    intField,
    messageOf,
    reasonOf,
    textField,
 )
import Test.HarnessGen (genCellSource, genDiagnostic)

genVerdict :: Gen DisposableVerdict
genVerdict =
    elements
        [ DisposableOk
        , DisposableCompileError
        , DisposableRuntimeError
        , DisposableTimedOut
        , DisposableUnavailable
        ]

genResult :: Gen (DisposableResult, MaterializeStage)
genResult = do
    stage <- elements materializeStages
    verdict <- genVerdict
    mcid <- oneof [pure Nothing, Just <$> choose (0, 40)]
    diag <- genDiagnostic
    replayed <- listOf (choose (0, 40))
    pure
        ( baseResult
            { disposableVerdict = verdict
            , disposableFailure = Just (MaterializeFailure stage mcid diag)
            , disposableReplayedCells = replayed
            }
        , stage
        )

{- | The same result re-pinned to one stage with the replay record asked for,
so a property can vary attribution without varying the diagnostic.
-}
atStage :: MaterializeStage -> [Int] -> DisposableResult -> DisposableResult
atStage stage replayed result =
    result
        { disposableFailure =
            Just (MaterializeFailure stage Nothing (messageOf result))
        , disposableReplayedCells = replayed
        }

{- | The two things a remedy can say to a caller whose candidate never ran:
that it has no lever at all, or that its own dependency metadata is one.
-}
saysUnclearable, namesCandidateMetadata :: Maybe Text -> Bool
saysUnclearable =
    maybe False ("Nothing you write in the candidate can clear it" `T.isInfixOf`)
namesCandidateMetadata =
    maybe False ("dependencies your candidate declares" `T.isInfixOf`)

spec :: Spec
spec = describe "try and the gate classify one result the same way" $ do
    it
        "prop_bothSurfacesAgreeOnTheVerdict: for every stage AND every verdict, \
        \the trial and the gate return the same verdict class"
        $ property
        $ forAll ((,) <$> genResult <*> genCellSource)
        $ \((result, _), src) ->
            let ours = textField "verdict" (disposablePayload result)
                theirs = textField "verdict" (gateOf src result)
             in conjoin
                    [ counterexample "the two surfaces disagree" (ours === theirs)
                    , counterexample
                        "the tag is not in the vocabulary"
                        (ours `elem` map Just verdictVocabulary)
                    ]

    it
        "prop_noDiagnosticVerdictBeforeTheCandidate: neither surface calls a \
        \pre-candidate failure a diagnostic about the submitted code"
        $ property
        $ forAll ((,) <$> genResult <*> genCellSource)
        $ \((result, stage), src) ->
            not (stageReachedCandidate stage) ==>
                conjoin
                    [ counterexample
                        "try"
                        (textField "verdict" (disposablePayload result) =/= Just "diagnostic")
                    , counterexample
                        "gate"
                        (textField "verdict" (gateOf src result) =/= Just "diagnostic")
                    ]

    it
        "prop_tryNeverCallsAHarnessFailureACompileError: no stage before the \
        \candidate is reported to the model as its code failing to compile"
        $ property
        $ forAll genResult
        $ \(result, stage) ->
            let v = disposablePayload result
             in not (stageReachedCandidate stage) ==>
                    ( textField "outcome" v =/= Just "compile_error"
                        .&&. field "candidateReached" v === Just (Bool False)
                    )

    it
        "prop_bothSurfacesBlameTheSameCell: the cell the trial names is the \
        \cell the gate names, derived once"
        $ property
        $ forAll ((,) <$> genResult <*> genCellSource)
        $ \((result, _), src) ->
            conjoin
                [ intField "blamedCell" (disposablePayload result) === blamedCell result
                , intField "blamedCell" (gateOf src result) === blamedCell result
                ]

    it
        "prop_derivedBlameIsWordedAsDerived: a stage that named no cell but ran \
        \after one says the cell is the last that ran, never that it failed"
        $ property
        $ forAll ((,) <$> genResult <*> listOf1 (choose (0, 40)))
        $ \((result0, stage), replayed) ->
            let reason = reasonOf (disposablePayload (atStage stage replayed result0))
             in not (stageReachedCandidate stage) ==>
                    counterexample
                        (show reason)
                        (maybe False ("the last cell that ran" `T.isInfixOf`) reason)

    it
        "prop_outcomeAgreesWithTheReasonBesideIt: only a failure the reason \
        \attributes to a cell is reported as that cell's replay being blocked"
        $ property
        $ forAll genResult
        $ \(result, stage) ->
            let v = disposablePayload result
                blockedOnACell = textField "outcome" v == Just "replay_blocked"
                saysHarness =
                    maybe False ("harness's own setup code" `T.isInfixOf`) (reasonOf v)
             in not (stageReachedCandidate stage) ==>
                    counterexample (show (textField "reason" v)) (blockedOnACell =/= saysHarness)

    it
        "prop_neitherSurfaceDeniesWhatTheCandidateCanBreak: a stage whose input \
        \included the candidate's own dependency metadata is never called \
        \unclearable"
        $ property
        $ forAll ((,) <$> genResult <*> genCellSource)
        $ \((result, stage), src) ->
            stageUsedCandidateMetadata stage ==>
                conjoin
                    [ counterexample "try denies" $
                        not (saysUnclearable (reasonOf (disposablePayload result)))
                    , counterexample "gate denies" $
                        not (saysUnclearable (reasonOf (gateOf src result)))
                    ]

    it
        "prop_aHarnessBlockNamesTheMetadataTheCandidateContributed: when the \
        \harness itself is blamed at such a stage, both surfaces name the \
        \candidate's own dependencies as a cause it owns"
        $ property
        $ forAll ((,) <$> genResult <*> genCellSource)
        $ \((result0, stage), src) ->
            let result = atStage stage [] result0
             in ( stageUsedCandidateMetadata stage
                    && attributionOf result == AttributedHarness
                    && not (environmentFault (messageOf result))
                )
                    ==> conjoin
                        [ counterexample "try hides the cause" $
                            namesCandidateMetadata (reasonOf (disposablePayload result))
                        , counterexample "gate hides the cause" $
                            namesCandidateMetadata (reasonOf (gateOf src result))
                        ]

    it
        "the law above binds every stage that ran inside the session built \
        \from that metadata, so it is not two stages wide"
        $ filter (not . stageUsedCandidateMetadata) materializeStages
            `shouldBe` [StagePlan, StageSnapshot]

    it
        "prop_onlyAStageThatRanACellCanBlameOne: a cell id offered by a stage \
        \that ran no single cell's source is not recorded as blame"
        $ property
        $ forAll
            ((,,) <$> elements materializeStages <*> choose (0, 40) <*> genDiagnostic)
        $ \(stage, cid, diag) ->
            failureCellId (stageFailure stage (Just cid) diag)
                === (if stageNamesCell stage then Just cid else Nothing)

    it
        "prop_attributedCellIsOnlyEverANamedCell: the attribution that reads \
        \'the stage itself named this cell' arises from no other stage"
        $ property
        $ forAll
            ((,,) <$> elements materializeStages <*> choose (0, 40) <*> genDiagnostic)
        $ \(stage, cid, diag) ->
            let result =
                    baseResult
                        { disposableFailure = Just (stageFailure stage (Just cid) diag)
                        , disposableReplayedCells = [cid]
                        }
             in case attributionOf result of
                    AttributedCell _ -> counterexample "named a cell" (stageNamesCell stage)
                    _ -> counterexample "named no cell" (not (stageNamesCell stage))

    it
        "the post-replay prelude failure names the last cell that ran, as the \
        \harness's own code, on the trial surface too"
        $ do
            let result =
                    baseResult
                        { disposableFailure =
                            Just (MaterializeFailure StagePrelude Nothing "ambiguous occurrence")
                        , disposableReplayedCells = [3, 7, 10]
                        }
                v = disposablePayload result
            textField "verdict" v `shouldBe` Just "could-not-run"
            textField "outcome" v `shouldBe` Just "harness_blocked"
            intField "blamedCell" v `shouldBe` Just 10
            textField "reason" v
                `shouldSatisfy` maybe False ("notebook cell 10" `T.isInfixOf`)
            textField "reason" v
                `shouldSatisfy` maybe False ("the last cell that ran" `T.isInfixOf`)
            textField "reason" v
                `shouldSatisfy` maybe False ("prelude" `T.isInfixOf`)

    it "a harness failure with nothing replayed blames the harness, not a cell" $ do
        let result =
                baseResult
                    { disposableFailure =
                        Just (MaterializeFailure StageSession Nothing "spawn failed")
                    }
            v = disposablePayload result
        textField "outcome" v `shouldBe` Just "harness_blocked"
        field "blamedCell" v `shouldBe` Nothing
        textField "reason" v
            `shouldSatisfy` maybe False ("harness" `T.isInfixOf`)
