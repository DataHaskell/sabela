{-# LANGUAGE OverloadedStrings #-}

{- | Two decisions the gate makes on its own behalf: whether repair may run at
all, and whether a green candidate is really admissible. Both are stated over
every stage and verdict, not over one example (LEDGER C1-4d, C2-8d).
-}
module Test.GateArmingSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.Edit.Admission (Admission (..))
import Sabela.AI.Capabilities.Edit.CompileGate (compileGateSpec, submittedOnly)
import Sabela.AI.Capabilities.Edit.GateRepair (acceptGreen, repairArmed)
import Sabela.AI.Capabilities.Try.Payload.Checked (compiledNotRunNote)
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    materializeStages,
    stageReachedCandidate,
 )
import Test.HarnessGen (genCellSource, genDefaultingWarning, genDiagnostic)

baseResult :: DisposableResult
baseResult =
    DisposableResult
        { disposableRoute = "disposable_scratch"
        , disposableVerdict = DisposableCompileError
        , disposableType = Nothing
        , disposableStdout = ""
        , disposableStderr = ""
        , disposableFailure = Nothing
        , disposableReplayedCells = []
        , disposableSkippedCells = []
        , disposableDependencies = []
        }

genVerdict :: Gen DisposableVerdict
genVerdict =
    elements
        [ DisposableOk
        , DisposableCompileError
        , DisposableRuntimeError
        , DisposableTimedOut
        , DisposableUnavailable
        ]

genFailedAt :: Gen DisposableResult
genFailedAt = do
    stage <- elements materializeStages
    mcid <- oneof [pure Nothing, Just <$> choose (0, 40)]
    diag <- genDiagnostic
    pure baseResult{disposableFailure = Just (MaterializeFailure stage mcid diag)}

greenWith :: Text -> DisposableResult
greenWith err =
    baseResult{disposableVerdict = DisposableOk, disposableStderr = err}

textField :: Text -> Value -> Maybe Text
textField k (Object o) = case KM.lookup (Key.fromText k) o of
    Just (String s) -> Just s
    _ -> Nothing
textField _ _ = Nothing

spec :: Spec
spec = describe "the gate arms and admits only on its own evidence" $ do
    it
        "prop_repairNeverArmsOnAForeignFault: repair runs only on a compile \
        \error the candidate itself produced, never on a stage it never reached"
        $ property
        $ forAll ((,) <$> genVerdict <*> genFailedAt)
        $ \(verdict, result) ->
            let reached =
                    maybe False (stageReachedCandidate . failureStage) (disposableFailure result)
             in repairArmed True verdict result
                    === (verdict == DisposableCompileError && reached)

    it "prop_repairNeverArmsWhenDisabled: the feature flag is a veto" $
        property $
            forAll ((,) <$> genVerdict <*> genFailedAt) $
                \(verdict, result) -> not (repairArmed False verdict result)

    it
        "prop_repairedGreenFacesTheSameCheck: a candidate that went green under \
        \repair is admitted only if a candidate green on arrival would be"
        $ property
        $ forAll ((,) <$> genCellSource <*> genDefaultingWarning (pure "()"))
        $ \(src, warning) ->
            conjoin
                [ counterexample
                    "a unit-defaulted green is refused"
                    ( isLeft
                        (accept src ["repaired"] (greenWith warning))
                    )
                , counterexample
                    "and the refusal reaches the caller as a refusal"
                    ( case accept src [] (greenWith warning) of
                        Left v -> textField "verdict" v === Just "diagnostic"
                        Right _ -> counterexample "expected a refusal" False
                    )
                ]

    it
        "prop_ordinaryGreenIsAdmittedWithItsNotes: nothing else in stderr \
        \blocks a green candidate, and its repair notes survive"
        $ property
        $ forAll ((,) <$> genCellSource <*> genDefaultingWarning (pure "Integer"))
        $ \(src, warning) ->
            accept src ["applied a fix"] (greenWith warning)
                === Right
                    Admission
                        { admittedSource = src
                        , admittedRepairs = ["applied a fix"]
                        , admittedChecked = [compiledNotRunNote]
                        }

    it "an empty stderr is admitted" $
        accept "x = 1" [] (greenWith "") `shouldSatisfy` isRight

-- | A green admitted against the spec its own trial was run with.
accept :: Text -> [Text] -> DisposableResult -> Either Value Admission
accept src notes =
    acceptGreen (compileGateSpec Nothing src) Nothing notes (submittedOnly src)
