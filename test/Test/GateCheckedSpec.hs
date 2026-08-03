{-# LANGUAGE OverloadedStrings #-}

{- | A candidate that compiles is not thereby a candidate that runs. The gate
proves the first and never the second: an admitted candidate ended a live
session, and the payload said nothing about which property had been checked.
-}
module Test.GateCheckedSpec (spec) where

import Control.Monad (guard)
import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.Edit.Admission (Admission (..), admissionPairs)
import Sabela.AI.Capabilities.Edit.CompileGate (compileGateSpec, submittedOnly)
import Sabela.AI.Capabilities.Edit.GateRepair (acceptGreen)
import Sabela.AI.Capabilities.Edit.Replace (commitPayload)
import Sabela.AI.Capabilities.Try.Payload.Checked (
    RunRecord (..),
    checkedNotes,
    compiledNotRunNote,
    runRecordNote,
    runRecordOf,
 )
import Sabela.Model (Cell (..), CellType (..))
import Sabela.Session.Materialize (CandidateSpec (..))
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    materializeStages,
    stageReachedCandidate,
 )
import Sabela.SessionTypes (CellLang (..))
import Test.HarnessGen (genCellSource, genDiagnostic)

spec :: Spec
spec = describe "which property the gate says it checked" $ do
    disclosureSpec
    acceptSpec
    payloadSpec
    scopeSpec

disclosureSpec :: Spec
disclosureSpec = do
    it
        "prop_saidOnlyWhenTheRecordSettlesIt: a trial that never reached the \
        \candidate claims nothing about it"
        $ property
        $ forAll ((,) <$> genCellSource <*> genUnreachedResult)
        $ \(src, result) -> checkedNotes (specFor src) result === []
    it
        "prop_saidWheneverTheCandidateWasNotRun: a trial that reached the \
        \candidate without running it says so"
        $ property
        $ forAll ((,) <$> genCellSource <*> genReachedResult)
        $ \(src, result) ->
            checkedNotes (specFor src) result === [compiledNotRunNote]
    it
        "prop_readsTheRecordNotTheSource: no call a candidate makes can \
        \strengthen or weaken the claim"
        $ property
        $ forAll ((,,) <$> genCellSource <*> genCellSource <*> genAnyResult)
        $ \(a, b, result) ->
            checkedNotes (specFor a) result === checkedNotes (specFor b) result

acceptSpec :: Spec
acceptSpec = describe "what an admission reads its claim off" $ do
    it
        "prop_everyAdmittedCandidateCarriesIt: an admission states the \
        \property proved, beside whatever notes the repair earned"
        $ property
        $ forAll ((,) <$> genCellSource <*> genNotes)
        $ \(src, notes) ->
            acceptGreen (specFor src) Nothing notes (submittedOnly src) greenResult
                === Right
                    Admission
                        { admittedSource = src
                        , admittedRepairs = notes
                        , admittedChecked = [compiledNotRunNote]
                        }
    it
        "prop_readsTheSpecTheTrialWasRunWith: the claim follows the spec put \
        \to the compiler, not one rebuilt from the admitted text"
        $ property
        $ forAll ((,) <$> genCellSource <*> genRanSpec)
        $ \(src, ranSpec) ->
            fmap
                admittedChecked
                (acceptGreen ranSpec Nothing [] (submittedOnly src) greenResult)
                === Right (checkedNotes ranSpec greenResult)

{- | A spec a trial might have been run with, including ones that evaluate the
candidate — which a spec rebuilt from the admitted source never does, so the
two disagree exactly where the claim does.
-}
genRanSpec :: Gen CandidateSpec
genRanSpec = do
    src <- genCellSource
    expr <- oneof [pure Nothing, Just <$> genCellSource]
    pure (compileGateSpec Nothing src){candidateExpression = expr}

{- | The payload keeps the two apart. A disclosure about a candidate published
under @repairs@ would say the harness had edited a candidate it never touched,
which is the field's whole contract.
-}
payloadSpec :: Spec
payloadSpec = describe "how an admission reaches the caller" $ do
    it "prop_repairsFieldCarriesOnlyRepairs" $
        property $
            forAll ((,,,) <$> genCellSource <*> genNotes <*> genNotes <*> genRunRecord) $
                \(src, repairs, checked, run) ->
                    lookupPair "repairs" (admissionPairs run (admission src repairs checked))
                        === (toJSON repairs <$ guard (not (null repairs)))
    it "prop_checkedFieldCarriesOnlyWhatWasChecked" $
        property $
            forAll ((,,,) <$> genCellSource <*> genNotes <*> genNotes <*> genRunRecord) $
                \(src, repairs, checked, run) ->
                    lookupPair "checked" (admissionPairs run (admission src repairs checked))
                        === (toJSON (checked <> [runRecordNote run]) <$ guard (not (null checked)))
    it "prop_anUntouchedCandidatePublishesNoRepairs" $
        property $
            forAll ((,,) <$> genCellSource <*> genNotes <*> genRunRecord) $
                \(src, checked, run) ->
                    lookupPair "repairs" (admissionPairs run (admission src [] checked))
                        === Nothing

{- | The gate's finding is scoped to the gate: a payload that reports a run of
the candidate never carries the sentence written for a payload that does not,
so no reader has to reconcile the two halves of one object.
-}
scopeSpec :: Spec
scopeSpec = describe "how far the finding may be read" $ do
    it
        "prop_theClaimTracksWhatElseThePayloadSettles: the field is a \
        \function of the run record, not a constant"
        $ property
        $ forAll ((,,) <$> genCellSource <*> genNonEmptyNotes <*> genTwoRecords)
        $ \(src, checked, (a, b)) ->
            lookupPair "checked" (admissionPairs a (admission src [] checked))
                =/= lookupPair "checked" (admissionPairs b (admission src [] checked))
    it
        "prop_noRecordedRunIsCalledUnrun: no payload says nothing has run the \
        \candidate unless nothing has"
        $ property
        $ forAll ((,,) <$> genCellSource <*> genNotes <*> genRunRecord)
        $ \(src, checked, run) ->
            (runRecordNote RunNotAttempted `elem` checkedField run src checked)
                === (run == RunNotAttempted && not (null checked))
    it
        "prop_theRecordIsReadOffThePayload: a committed write's run record is \
        \its own execution field"
        $ property
        $ forAll ((,) <$> genNotes <*> genExecution)
        $ \(checked, execSummary) ->
            ( runRecordNote (runRecordOf execSummary)
                `elem` payloadChecked execSummary checked
            )
                === not (null checked)

checkedField :: RunRecord -> Text -> [Text] -> [Text]
checkedField run src checked =
    maybe
        []
        jsonTexts
        (lookupPair "checked" (admissionPairs run (admission src [] checked)))

{- | The @checked@ array of a real committed replace, so the run record the
payload publishes is the one its own execution field carries.
-}
payloadChecked :: Value -> [Text] -> [Text]
payloadChecked execSummary checked =
    case commitPayload 1 committedCell execSummary [] (admission "x = 1" [] checked) of
        Object o -> maybe [] jsonTexts (KM.lookup "checked" o)
        _ -> []

committedCell :: Cell
committedCell = Cell 1 CodeCell Haskell "x = 1" [] Nothing True

jsonTexts :: Value -> [Text]
jsonTexts v = case A.fromJSON v of
    A.Success ts -> ts
    A.Error _ -> []

admission :: Text -> [Text] -> [Text] -> Admission
admission src repairs checked =
    Admission
        { admittedSource = src
        , admittedRepairs = repairs
        , admittedChecked = checked
        }

lookupPair :: Text -> [Pair] -> Maybe Value
lookupPair = lookup . Key.fromText

specFor :: Text -> CandidateSpec
specFor = compileGateSpec Nothing

genNotes :: Gen [Text]
genNotes = listOf (elements ["applied a fix", "renamed a binder"])

genNonEmptyNotes :: Gen [Text]
genNonEmptyNotes = listOf1 (elements ["applied a fix", "renamed a binder"])

genRunRecord :: Gen RunRecord
genRunRecord = elements [RunNotAttempted, RunUnderway, RunRecorded]

genTwoRecords :: Gen (RunRecord, RunRecord)
genTwoRecords =
    ((,) <$> genRunRecord <*> genRunRecord) `suchThat` uncurry (/=)

-- | What a committed write's execution field can hold, run or no run.
genExecution :: Gen Value
genExecution =
    oneof
        [ pure Null
        , (\ok -> object ["ok" .= (ok :: Bool)]) <$> arbitrary
        , String <$> elements ["ran", "died"]
        ]

greenResult :: DisposableResult
greenResult =
    DisposableResult
        { disposableRoute = "disposable_scratch"
        , disposableVerdict = DisposableOk
        , disposableType = Nothing
        , disposableStdout = ""
        , disposableStderr = ""
        , disposableFailure = Nothing
        , disposableReplayedCells = []
        , disposableSkippedCells = []
        , disposableDependencies = []
        }

-- | A trial that stopped before the candidate, at any stage that can stop one.
genUnreachedResult :: Gen DisposableResult
genUnreachedResult = do
    stage <- elements (filter (not . stageReachedCandidate) materializeStages)
    mcid <- oneof [pure Nothing, Just <$> choose (0, 40)]
    diag <- genDiagnostic
    pure greenResult{disposableFailure = Just (MaterializeFailure stage mcid diag)}

-- | A trial that reached the candidate, whatever the compiler then said.
genReachedResult :: Gen DisposableResult
genReachedResult = do
    verdict <-
        elements
            [ DisposableOk
            , DisposableCompileError
            , DisposableRuntimeError
            , DisposableTimedOut
            , DisposableUnavailable
            ]
    stage <- elements (filter stageReachedCandidate materializeStages)
    failure <-
        oneof
            [ pure Nothing
            , Just . MaterializeFailure stage Nothing <$> genDiagnostic
            ]
    pure greenResult{disposableVerdict = verdict, disposableFailure = failure}

genAnyResult :: Gen DisposableResult
genAnyResult = oneof [genUnreachedResult, genReachedResult]
