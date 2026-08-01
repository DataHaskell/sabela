{-# LANGUAGE OverloadedStrings #-}

{- | The disposable session is run under @-Wtype-defaults@ so the gate can see
a variable GHC had to invent a type for. A warning is not a failure: a green
trial must not come back carrying one, and must not lose the signal that it
compiled (LEDGER C2-8d).
-}
module Test.TrialWarningSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.Edit.CompileGate (compileGateSpec, defaultedToUnit)
import Sabela.AI.Capabilities.Try.Payload (disposablePayload)
import Sabela.Session.Materialize (candidateSafetyPrelude, expressionCandidate)
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    MaterializeStage (..),
    failureFor,
    materializeStages,
 )
import Test.HarnessGen (genDefaultingWarning, genDiagnostic)

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField k v = case field k v of
    Just (String s) -> Just s
    _ -> Nothing

greenWith :: Text -> Text -> Maybe Text -> DisposableResult
greenWith out err ty =
    DisposableResult
        { disposableRoute = "disposable_scratch"
        , disposableVerdict = DisposableOk
        , disposableType = ty
        , disposableStdout = out
        , disposableStderr = err
        , disposableFailure = failureFor DisposableOk StageCandidateRun err
        , disposableReplayedCells = []
        , disposableSkippedCells = []
        , disposableDependencies = []
        }

genWarning :: Gen Text
genWarning =
    genDefaultingWarning
        (elements ["()", "Integer", "Double", "GHC.Types.Any", "[Char]"])

spec :: Spec
spec = describe "a warning is not a failure" $ do
    it
        "the trial prelude asks GHC for exactly the warning the gate's \
        \defaulting check reads"
        $ do
            let gate = candidateSafetyPrelude (compileGateSpec Nothing "x = mempty")
                trial = candidateSafetyPrelude (expressionCandidate "mempty")
            gate `shouldSatisfy` T.isInfixOf "-Wtype-defaults"
            trial `shouldSatisfy` T.isInfixOf "-Wtype-defaults"

    it
        "prop_greenNeverRecordsAFailure: whatever GHC wrote to stderr, a green \
        \run records no failure at any stage"
        $ property
        $ forAll ((,) <$> elements materializeStages <*> oneof [genWarning, genDiagnostic])
        $ \(stage, err) -> failureFor DisposableOk stage err === Nothing

    it
        "prop_nonGreenStillRecordsItsDiagnostic: a run that did fail keeps the \
        \stage and the text it failed with"
        $ property
        $ forAll ((,) <$> elements materializeStages <*> genDiagnostic)
        $ \(stage, err) ->
            not (T.null (T.strip err))
                ==> failureFor DisposableCompileError stage err
                =/= Nothing

    it
        "prop_aSilentRejectionStillRecordsItsStage: a run that was not green \
        \records the stage it reached even when GHC wrote nothing, so silence \
        \is never read back as the candidate having gone uncompiled"
        $ property
        $ forAll ((,) <$> elements materializeStages <*> elements ["", "   ", "\n"])
        $ \(stage, err) ->
            fmap failureStage (failureFor DisposableCompileError stage err)
                === Just stage

    it
        "prop_aWarningDoesNotMakeATrialLookBroken: a green trial reports no \
        \failure and still shows the warning under stderr"
        $ property
        $ forAll genWarning
        $ \w ->
            let v = disposablePayload (greenWith "" w Nothing)
             in conjoin
                    [ counterexample "no failure block" (field "failure" v === Nothing)
                    , counterexample "outcome stays ok" (textField "outcome" v === Just "ok")
                    , counterexample "the warning is still shown" (textField "stderr" v === Just w)
                    ]

    it
        "prop_aWarningDoesNotSilenceTheCompiledSignal: declarations that \
        \compiled say so even when GHC warned about them"
        $ property
        $ forAll genWarning
        $ \w ->
            textField "diagnostic" (disposablePayload (greenWith "" w Nothing))
                =/= Nothing

    it "an executed expression is not reported as no expression executed" $ do
        let v = disposablePayload (greenWith "" "warning: something" (Just "IO ()"))
        textField "diagnostic" v `shouldBe` Nothing

    it "the defaulting check reads the warning the flag produces" $
        property $
            forAll (genDefaultingWarning (pure "()")) $
                \w -> not (null (defaultedToUnit w))
