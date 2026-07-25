{-# LANGUAGE OverloadedStrings #-}

{- | Pins the G1 compile-gate rejection envelope
('Sabela.AI.Capabilities.Edit.CompileGate.rejectionJson'): the
verdict/stage/diagnostic/hole-fit/source-echo shape every gated AI mutation
returns instead of committing. Built directly from a fabricated
'DisposableResult' — no live GHCi or cabal build — mirroring
'Test.TryOutcomeWireSpec'. A field rename here is a wire break.
-}
module Test.CompileGateWireSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Test.Hspec

import Sabela.AI.Capabilities.Edit.CompileGate (rejectionJson)
import Sabela.Session.Materialize (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    MaterializeStage (..),
    disposableRouteName,
 )

field :: Text -> Value -> Maybe Value
field key (Object obj) = KM.lookup (Key.fromText key) obj
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField key value = case field key value of
    Just (String text) -> Just text
    _ -> Nothing

baseResult :: DisposableResult
baseResult =
    DisposableResult
        { disposableRoute = disposableRouteName
        , disposableVerdict = DisposableCompileError
        , disposableType = Nothing
        , disposableStdout = ""
        , disposableStderr = ""
        , disposableFailure = Nothing
        , disposableReplayedCells = [0]
        , disposableSkippedCells = []
        , disposableDependencies = []
        }

holeDiagnostic :: Text
holeDiagnostic =
    "cell 1: Found hole: _ :: Point -> Point -> Picture\n\
    \Valid hole fits include\n\
    \  line :: Point -> Point -> Picture\n\
    \    with line @Point"

spec :: Spec
spec = describe "compile-gate rejection envelope wire pins" $ do
    it "compile error on an insert: no cellId, verdict/stage/diagnostic/source present" $ do
        let result =
                baseResult
                    { disposableFailure =
                        Just (MaterializeFailure StageCandidateSetup Nothing "parse error on input")
                    }
            v = rejectionJson Nothing "broken = " DisposableCompileError result
        textField "refusal" v `shouldBe` Just "compile-gate"
        field "cellId" v `shouldBe` Nothing
        textField "verdict" v `shouldBe` Just "diagnostic"
        textField "stage" v `shouldBe` Just "candidate_setup"
        textField "diagnostic" v `shouldBe` Just "parse error on input"
        textField "source" v `shouldBe` Just "broken = "
        field "holeFits" v `shouldBe` Nothing
        field "holeProbe" v `shouldBe` Nothing

    it "compile error on a replace: names the replaced cell" $ do
        let result =
                baseResult
                    { disposableFailure =
                        Just
                            ( MaterializeFailure
                                StageCandidateTypecheck
                                Nothing
                                "Variable not in scope: foo"
                            )
                    }
            v = rejectionJson (Just 3) "foo" DisposableCompileError result
        field "cellId" v `shouldBe` Just (Number 3)
        textField "stage" v `shouldBe` Just "candidate_typecheck"

    it "carries parsed hole fits alongside the raw diagnostic when GHC reports them" $ do
        let result =
                baseResult
                    { disposableFailure =
                        Just
                            (MaterializeFailure StageCandidateTypecheck Nothing holeDiagnostic)
                    }
            v = rejectionJson Nothing "line (_ :: Point) (_ :: Point)" DisposableCompileError result
        textField "diagnostic" v `shouldBe` Just holeDiagnostic
        case field "holeFits" v of
            Just (Array fits) -> length fits `shouldBe` 1
            _ -> expectationFailure "expected a non-empty holeFits array"

    it "G3: the rejection carries the probe conclusions harvested by this gate check" $ do
        let result =
                baseResult
                    { disposableFailure =
                        Just
                            (MaterializeFailure StageCandidateTypecheck Nothing holeDiagnostic)
                    }
            v = rejectionJson Nothing "line (_ :: Point) (_ :: Point)" DisposableCompileError result
        case field "holeProbe" v >>= field "facts" of
            Just (Array facts) -> length facts `shouldBe` 1
            _ -> expectationFailure "expected a holeProbe facts array"
        (field "holeProbe" v >>= field "provenance")
            `shouldBe` Just (String "via: hole-probe")

    it "an infra failure (build timeout) is the closed infra verdict, not a diagnostic" $ do
        let result =
                baseResult
                    { disposableVerdict = DisposableTimedOut
                    , disposableFailure =
                        Just
                            (MaterializeFailure StageSession Nothing "trial exceeded its time budget")
                    }
            v = rejectionJson Nothing "x = 1" DisposableTimedOut result
        textField "verdict" v `shouldBe` Just "no-verdict-infra"
        case textField "error" v of
            Just msg -> msg `shouldSatisfy` (/= "")
            Nothing -> expectationFailure "expected an error message"

    it "an infra failure never claims the candidate was verified or committed" $ do
        let result = baseResult{disposableVerdict = DisposableUnavailable}
            v = rejectionJson Nothing "x = 1" DisposableUnavailable result
        textField "verdict" v `shouldBe` Just "no-verdict-infra"
        textField "diagnostic" v
            `shouldBe` Just
                "The compile gate could not verify this write; nothing was committed."
