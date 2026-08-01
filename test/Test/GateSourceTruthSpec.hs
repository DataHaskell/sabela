{-# LANGUAGE OverloadedStrings #-}

{- | Every claim a gate rejection makes about "this cell" is measured against
the text the same payload presents as @source@, and the diagnostic it hands
back names only binders the caller could have written (LEDGER C1-4f, C2-2b).
-}
module Test.GateSourceTruthSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.Edit.CompileGate (
    GateSource (..),
    gateDefaultingRejection,
    notCommittedKind,
    rejectionJson,
    submittedOnly,
 )
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    MaterializeStage (..),
    materializeStages,
    stageReachedCandidate,
 )
import Test.HarnessGen (genCellSource, genDefaultingWarning, genPackageName)

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField k v = case field k v of
    Just (String s) -> Just s
    _ -> Nothing

-- | Every string anywhere inside a value, so no claim can hide in a nested key.
strings :: Value -> [Text]
strings (String s) = [s]
strings (Array xs) = concatMap strings (foldr (:) [] xs)
strings (Object o) = concatMap strings (KM.elems o)
strings _ = []

guidanceText :: Value -> [Text]
guidanceText v = maybe [] strings (field "guidance" v)

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

failingAt :: MaterializeStage -> Text -> DisposableResult
failingAt stage diag =
    baseResult{disposableFailure = Just (MaterializeFailure stage Nothing diag)}

-- | The four verdicts a recorded failure can carry; a green run records none.
genFailingVerdict :: Gen DisposableVerdict
genFailingVerdict =
    elements
        [ DisposableCompileError
        , DisposableRuntimeError
        , DisposableTimedOut
        , DisposableUnavailable
        ]

failingWith :: DisposableVerdict -> MaterializeStage -> Text -> DisposableResult
failingWith verdict stage diag =
    (failingAt stage diag){disposableVerdict = verdict}

greenWith :: Text -> DisposableResult
greenWith err = baseResult{disposableVerdict = DisposableOk, disposableStderr = err}

missingModuleDiagnostic :: Text -> Text
missingModuleDiagnostic m =
    "<no location info>: error: [GHC-35235]\n    Could not find module \8216"
        <> m
        <> "\8217.\n    It is not a module in the current program."

-- | A diagnostic whose outer context frame is the gate's own probe binder.
probeFramedDiagnostic :: Text -> Text
probeFramedDiagnostic name =
    "<interactive>:3:1: error: [GHC-88464]\n\
    \    Variable not in scope: "
        <> name
        <> "\n\
           \In the expression: "
        <> name
        <> "\n\
           \In an equation for `_sabelaGateProbe0': _sabelaGateProbe0 = ("

declaresPackage :: Text -> Text -> Bool
declaresPackage pkg src =
    any
        (\l -> "build-depends:" `T.isInfixOf` l && pkg `T.isInfixOf` l)
        (T.lines src)

spec :: Spec
spec = describe "a rejection is measured against the source it shows" $ do
    it
        "prop_defaultingRefusalShowsTheCallersOwnSource: the refusal that a \
        \green candidate can still earn puts the submission under `source` and \
        \discloses any rewrite under its own key"
        $ property
        $ forAll
            ( (,,)
                <$> genCellSource
                <*> genCellSource
                <*> genDefaultingWarning (pure "()")
            )
        $ \(submitted, compiled, warning) ->
            case gateDefaultingRejection
                Nothing
                []
                (GateSource submitted compiled)
                (greenWith warning) of
                Nothing -> counterexample "expected a refusal" False
                Just v ->
                    conjoin
                        [ counterexample
                            "source is the submission"
                            (textField "source" v === Just submitted)
                        , counterexample
                            "a rewrite is disclosed, never substituted"
                            ( textField "compiledSource" v
                                === (if compiled == submitted then Nothing else Just compiled)
                            )
                        ]

    it
        "prop_defaultingRefusalKeepsTheNotesTheCommitWouldHaveCarried: a fix \
        \the harness applied is still disclosed when the write is refused"
        $ property
        $ forAll ((,) <$> genCellSource <*> genDefaultingWarning (pure "Any"))
        $ \(src, warning) ->
            let note = "Applied GHC's suggested fix before committing: added an import."
             in case gateDefaultingRejection
                    Nothing
                    [note]
                    (submittedOnly src)
                    (greenWith warning) of
                    Nothing -> counterexample "expected a refusal" False
                    Just v ->
                        counterexample (show (textField "note" v)) $
                            any (note `T.isInfixOf`) (strings v)

    it
        "prop_guidanceOnlyClaimsWhatTheShownSourceSays: a package present only \
        \in the compiled text is never reported as one this cell declares"
        $ property
        $ forAll
            ( (,,)
                <$> genCellSource
                <*> genPackageName
                <*> elements ["Zither.Frobnicate", "Quux.Internal.Bar"]
            )
        $ \(submitted, pkg, m) ->
            let compiled = "-- cabal: build-depends: " <> pkg <> "\n" <> submitted
                diag = missingModuleDiagnostic m
                v =
                    rejectionJson
                        (Just pkg)
                        Nothing
                        (GateSource submitted compiled)
                        []
                        (failingAt StageCandidateTypecheck diag)
                shown = fromMaybe "" (textField "source" v)
             in not (declaresPackage pkg submitted) ==>
                    counterexample
                        (show (guidanceText v))
                        ( all
                            (\g -> not ("is already declared" `T.isInfixOf` g))
                            (guidanceText v)
                            && not (declaresPackage pkg shown)
                        )

    it
        "the same guidance does report a package the shown source really \
        \declares, so the property above is not vacuous"
        $ do
            let pkg = "zither"
                src = "-- cabal: build-depends: " <> pkg <> "\nimport Zither.Frobnicate\n"
                diag = missingModuleDiagnostic "Zither.Frobnicate"
                v =
                    rejectionJson
                        (Just pkg)
                        Nothing
                        (submittedOnly src)
                        []
                        (failingAt StageCandidateTypecheck diag)
            guidanceText v `shouldSatisfy` any ("is already declared" `T.isInfixOf`)

    it
        "prop_theDiagnosticNamesNoBinderTheCallerCouldNotHaveWritten: the \
        \gate's own probe wrapper never reaches the model"
        $ property
        $ forAll genCellSource
        $ \src ->
            let v =
                    rejectionJson
                        Nothing
                        Nothing
                        (submittedOnly src)
                        []
                        (failingAt StageCandidateTypecheck (probeFramedDiagnostic "widen"))
                diag = fromMaybe "" (textField "diagnostic" v)
             in conjoin
                    [ counterexample
                        (show diag)
                        (not ("_sabela" `T.isInfixOf` diag))
                    , counterexample
                        "the model's own error was scrubbed away with it"
                        ("widen" `T.isInfixOf` diag)
                    ]

    it
        "prop_theRefusalLabelSaysWhichGateRefused: only a candidate the \
        \compiler read was refused by the compile gate, whatever went wrong"
        $ property
        $ forAll
            ( (,,)
                <$> elements materializeStages
                <*> genFailingVerdict
                <*> genCellSource
            )
        $ \(stage, verdict, src) ->
            let result = failingWith verdict stage "boom"
                v = rejectionJson Nothing Nothing (submittedOnly src) [] result
                expected =
                    if stageReachedCandidate stage then "compile-gate" else "trial-blocked"
             in textField "notCommitted" v === Just expected

    it "the two labels are the two the classifier can produce" $ do
        notCommittedKind (failingAt StageCandidateTypecheck "boom")
            `shouldBe` "compile-gate"
        notCommittedKind (failingWith DisposableUnavailable StageSnapshot "boom")
            `shouldBe` "trial-blocked"
        notCommittedKind (failingWith DisposableTimedOut StageSession "boom")
            `shouldBe` "trial-blocked"
        notCommittedKind baseResult `shouldBe` "trial-blocked"
